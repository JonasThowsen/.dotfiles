;;;; Mahogany configuration.
;;;;
;;;; Loaded when Mahogany starts, and again by the loadrc command
;;;; (C-t ; loadrc). Lives in ~/.dotfiles/mahogany, stowed into ~.
;;;; The manual is in doc/manual/ in the Mahogany repository.

(in-package :mahogany-user)

;;; Keyboard, the same as in niri: Norwegian layout, Caps Lock is Escape.
;;; Several options are separated by commas, e.g. "caps:escape,altwin:menu_win".

(xkb:with-xkb-rule-names (rules (:layout "no" :options "caps:escape"))
  (mh::mahogany-set-keymap mh::*compositor-state* :rules rules))

(config-system:set-config mh::keyboard-repeat-rate 70
                          mh::keyboard-repeat-delay 200)

;;; Input, as in niri: tap to click and natural scrolling on the
;;; touchpad, a slower mouse, Num Lock on, the Nordzy cursor.

(config-system:set-config mh::touchpad-tap-to-click t
                          mh::touchpad-natural-scroll t
                          mh::mouse-accel-speed -0.45
                          mh::numlock t
                          mh::cursor-theme "macOS"
                          mh::cursor-size 24)

;;; Outputs: the laptop screen is scaled 2 from its DPI on its own; the
;;; external 4K monitor at 1.5, to the left, as in niri.

(mh::define-output-config "monitor" "DP-1" (:scale 1.5) (:position 0 0))

;;; Tiling: new groups use dwindle instead of master-stack (s-r cycles).

(config-system:set-config mh::*default-layout* :dwindle)

;;; Hex, local voice dictation (anoma.ly/hex; programs.hex in the NixOS
;;; config). Mahogany has no GlobalShortcuts portal, so the keys run Hex's
;;; own commands. Hold-to-talk needs keybindings on key release, which
;;; Mahogany lacks (ferdig: "Push-to-talk: keybindings that also act on key
;;; release"); until then a press starts, stops or toggles.

(defvar *hex-state* nil
  "What Hex is doing, from the last `hex status --json`: :recording,
:transcribing, :idle, :disabled (running, not listening), :off (not
running), or NIL before the first reading.")

(defvar *hex-reading* nil
  "True while a `hex status` runs.")

(defun hex-json-string (text &rest keys)
  "The string value of the last of KEYS in the JSON TEXT, each key looked
for after the one before it: (hex-json-string text \"capture\" \"state\").
Enough for `hex status`, without a JSON library."
  (let ((start 0))
    (dolist (key keys)
      (let ((at (search (format nil "\"~A\"" key) text :start2 start)))
        (unless at (return-from hex-json-string nil))
        (setf start (+ at (length key) 2))))
    (let* ((colon (position #\: text :start start))
           (open (and colon (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                             text :start (1+ colon)))))
      (cond ((null open) nil)
            ((char= (char text open) #\")
             (let ((close (position #\" text :start (1+ open))))
               (and close (subseq text (1+ open) close))))
            (t (let ((end (position-if (lambda (c) (member c '(#\, #\} #\Space #\Newline)))
                                       text :start open)))
                 (subseq text open end)))))))

(defun hex-refresh ()
  "Read Hex's state asynchronously into *hex-state*, then redraw the bar.
Call on the main thread."
  (unless *hex-reading*
    (setf *hex-reading* t)
    (mh::run-async '("hex" "status" "--json")
                   (lambda (output code)
                     (setf *hex-reading* nil
                           *hex-state*
                           (cond ((or (null output) (not (eql code 0))) :off)
                                 ((equal (hex-json-string output "listener" "running") "false")
                                  :disabled)
                                 (t (let ((state (hex-json-string output "capture" "state")))
                                      (cond ((equal state "recording") :recording)
                                            ((equal state "transcribing") :transcribing)
                                            (t :idle))))))
                     (mh::mode-line-update)))))

(defun hex-run (&rest args)
  "Run `hex ARGS...`, then read Hex's state again so the bar follows at once."
  (hrt:with-main-thread ()
    (mh::run-async (cons "hex" args)
                   (lambda (output code)
                     (declare (ignore output))
                     (unless (eql code 0)
                       (mh::log-string :warn "hex ~{~A~^ ~} exited ~A" args code))
                     (hex-refresh)))))

(mh::defcommand hex-dictation-toggle ()
  (:documentation "Hex: start dictating, or stop and paste")
  (:method () (hex-run "dictation" "toggle")))

(mh::defcommand hex-dictation-start ()
  (:documentation "Hex: start dictating")
  (:method () (hex-run "dictation" "start")))

(mh::defcommand hex-dictation-stop ()
  (:documentation "Hex: stop dictating and paste")
  (:method () (hex-run "dictation" "stop")))

(mh::defcommand hex-dictation-cancel ()
  (:documentation "Hex: discard the recording")
  (:method () (hex-run "dictation" "cancel")))

(mh::define-keys mh::*top-map*
  "s-z" #'hex-dictation-toggle
  "s-S-z" #'hex-dictation-start
  "s-C-z" #'hex-dictation-stop
  "s-M-z" #'hex-dictation-cancel)

;; The bar: a microphone, red while recording, yellow while transcribing,
;; crossed out and dimmed while Hex is off or disabled. A click opens Hex's
;; window (and starts Hex if it isn't running).
(mh::define-mode-line-module :hex (:interval 2
                                   :refresh #'hex-refresh
                                   :on-click (lambda () (hex-run)))
  (let ((module (gethash :hex mh::*mode-line-modules*))
        (mic (string (code-char #xF036C)))
        (mic-off (string (code-char #xF036D))))
    (setf (mh::mode-line-module-color module)
          (case *hex-state*
            (:recording (cl-colors2:rgb 0.95 0.3 0.3))
            (:transcribing (cl-colors2:rgb 0.95 0.8 0.3))
            ((:off :disabled nil) (cl-colors2:rgb 0.5 0.5 0.5))
            (t nil)))
    (case *hex-state*
      (:recording (format nil "~A rec" mic))
      (:transcribing (format nil "~A …" mic))
      (:idle mic)
      (t mic-off))))

(config-system:set-config mh::*mode-line-segments*
                          '(:groups :title :center :date :clock
                            :fill :hex :network :volume :brightness :battery))

;; Hex in the background with the session (no window); the NixOS module
;; installs it without a user service.
(config-system:set-config mh::*autostart* '(("hex" "start")))
