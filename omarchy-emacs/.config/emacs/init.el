;;; init.el --- User Emacs configuration -*- lexical-binding: t -*-

;; Load Omarchy integration (theme syncing, font syncing, file watchers).
;; Remove this line to opt out of Omarchy Emacs integration.
(load (expand-file-name "omarchy" user-emacs-directory))

;; Your customizations below

;;; Basics
;; Don't pop up *Warnings* for compiler noise in third-party packages
;; while they are natively compiled in the background.
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(global-auto-revert-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Keep M-x customize output out of this (git-tracked) file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;; Packages
;; Unlike NixOS, packages come from MELPA here and are installed on first start.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; Evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; General
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer my-leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my-local-leader
    :states '(normal visual motion)
    :prefix "SPC m"))

;;; Completion
(use-package vertico
  :config
  (vertico-mode))

;; Recommended to save across Emacs restarts
(savehist-mode)

(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq completion-category-defaults nil)
  (setq completion-pcm-leading-wildcard t))

(use-package marginalia
  :config
  (marginalia-mode))

;;; Searching (consult-fd and consult-ripgrep use fd and rg)
(use-package consult
  :config
  ;; In-buffer completion (e.g. SLY symbol completion) through the minibuffer
  (setq completion-in-region-function #'consult-completion-in-region))

;; TAB indents the line; if it's already indented, TAB completes instead
(setq tab-always-indent 'complete)

(use-package embark
  :bind ("C-." . embark-act))

(use-package embark-consult
  :after (embark consult))

;; In a consult-ripgrep result list: C-. E to export, i to edit, :w to write back
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package avy)

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers ".project"))

(my-leader
  "a" 'avy-goto-char-2
  "f" 'consult-fd
  "b" 'consult-buffer
  "s" 'consult-ripgrep
  "i" 'consult-line
  "j" 'consult-imenu)

;;; Magit
(use-package magit)

(defun magit-diff-visit-file-in-new-tab ()
  (interactive)
  (tab-bar-new-tab)
  (magit-diff-visit-file))

(my-leader
  "gg" 'magit)

(my-leader
  :keymaps 'magit-mode-map
  "o" 'magit-diff-visit-file-in-new-tab)

;;; Lisp editing
;; Structural editing for evil: d/c/y/x keep parens balanced, plus sexp
;; text objects and motions. Also on in elisp, for editing this file.
(use-package lispyville
  :hook ((lisp-mode emacs-lisp-mode) . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators            ; d/c/y/x keep parens balanced
     c-w
     text-objects         ; dix (sexp), daf (defun), dil (list), daS (string), dac (comment)
     additional-movement  ; H/L = back/forward sexp, ( ) = up a level, [ ] = prev/next paren
     slurp/barf-cp        ; > and < to slurp/barf
     wrap)))              ; M-( wraps the sexp at point in parens

;; Color parens by nesting depth
(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode sly-mrepl-mode) . rainbow-delimiters-mode))

;;; Common Lisp / SLY
;; SLY comes from MELPA. It turns itself on in lisp-mode buffers.
(use-package sly
  :commands (sly)
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  ;; Load the contribs (REPL, etc.) now rather than on first connect,
  ;; so sly-mrepl is defined before then. sly-tramp translates file names
  ;; for a Lisp on another machine (see Tun below).
  (sly-setup '(sly-fancy sly-tramp))
  ;; Use normal completion-at-point (and so consult) instead of SLY's own
  ;; completion popup
  (sly-symbol-completion-mode -1)
  ;; evil-collection handles the rest: evaluating the sexp under the cursor
  ;; in normal state, and vim keys in the debugger and inspector.
  (evil-set-initial-state 'sly-mrepl-mode 'insert)
  ;; Record gd in Evil's jump list, so C-o / C-i hop back and forth
  (evil-add-command-properties #'sly-edit-definition :jump t)
  ;; REPL output is read-only, and text typed right after it inherits that.
  ;; SLY is supposed to clear this when a program calls READ, but output that
  ;; arrives late (e.g. a y-or-n-p prompt) misses it, giving "Text is
  ;; read-only". Always let typed text start fresh after output.
  (define-advice sly-mrepl--insert-output (:after (&rest _) allow-typing-after-output)
    (let ((end (marker-position sly-mrepl--output-mark))
          (inhibit-read-only t))
      (when (> end (point-min))
        (put-text-property (1- end) end 'rear-nonsticky t))))
  ;; Output that arrives while Lisp is still reading can end up after the
  ;; cursor, leaving it stuck on the (read-only) prompt line. When a program
  ;; is waiting for input, send typing to the end of the REPL instead.
  (defun my/sly-mrepl-jump-to-input ()
    (when (and sly-mrepl--read-mark
               (eq this-command 'self-insert-command)
               (< (point) (marker-position (process-mark (sly-mrepl--process)))))
      (goto-char (point-max))))
  (add-hook 'sly-mrepl-mode-hook
            (lambda () (add-hook 'pre-command-hook #'my/sly-mrepl-jump-to-input nil t))))

(defun sly-mrepl-other-window ()
  "Show the REPL in a split, or jump to it if it's already visible."
  (interactive)
  (sly-mrepl #'pop-to-buffer))

(my-local-leader
  :keymaps 'lisp-mode-map
  "'" 'sly
  "e" 'sly-eval-last-expression
  "d" 'sly-eval-defun
  "r" 'sly-eval-region
  "b" 'sly-eval-buffer
  "c" 'sly-compile-defun
  "k" 'sly-compile-and-load-file
  "z" 'sly-mrepl-other-window
  "h" 'sly-describe-symbol
  "H" 'sly-hyperspec-lookup
  "m" 'sly-macroexpand-1
  "gd" 'sly-edit-definition
  "gb" 'sly-pop-find-definition-stack
  "C" 'tun-connect)

;;; Tun
;; Tun's checkout and its Lisp image live on the dev server. Files open over
;; TRAMP (/ssh:netcup-dev:~/coding/tun/...), and the server's bin/repl reaches
;; this machine through the tunnel in ~/.ssh/config (LocalForward 4010).
;; The repository's docs/emacs.md describes the whole setup.
(defvar tun-remote-host "netcup-dev"
  "The ~/.ssh/config host where Tun's checkout and bin/repl run.")

(defvar tun-repl-port 4010
  "The local end of the SSH tunnel to bin/repl's Slynk port.")

(defun tun-connect ()
  "Connect SLY to bin/repl on the dev server, through the SSH tunnel."
  (interactive)
  (sly-connect "localhost" tun-repl-port))

(with-eval-after-load 'sly-tramp
  ;; The remote Lisp names its files /home/jonas/coding/tun/..., which here
  ;; are TRAMP paths. The match is on (machine-instance) of the remote Lisp.
  ;; This gives the same names you open by hand, so gd reuses open buffers.
  (add-to-list 'sly-filename-translations
               (list "^netcup-dev$"
                     (lambda (emacs-filename)
                       (or (file-remote-p emacs-filename 'localname) emacs-filename))
                     (lambda (lisp-filename)
                       (concat "/ssh:" tun-remote-host ":" lisp-filename)))))

(defun tun-indentation-setup ()
  "In a Tun checkout, indent the way the repository's bin/indent does.
That is Emacs's own `common-lisp-indent-function' with the hints in the
checkout's scripts/indent.el. SLY's indenter would line up some keyword
arguments differently, and bin/indent --check would then flag the file."
  (when-let* ((file (buffer-file-name))
              (root (locate-dominating-file file "tun.asd")))
    (unless (featurep 'tun-indent)
      (load (expand-file-name "scripts/indent.el" root) nil t))
    (tun-indent-mode-setup)))

;; Depth 90 runs this after SLY's own lisp-mode setup, which it overrides.
(add-hook 'lisp-mode-hook #'tun-indentation-setup 90)

;;; init.el ends here
