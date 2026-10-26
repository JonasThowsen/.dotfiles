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
                          mh::cursor-theme "Nordzy-catppuccin-mocha-yellow"
                          mh::cursor-size 24)

;;; Outputs: the laptop screen is scaled 2 from its DPI on its own; the
;;; external 4K monitor at 1.5, to the left, as in niri.

(mh::define-output-config "monitor" "DP-1" (:scale 1.5) (:position 0 0))
