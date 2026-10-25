;;; init.el --- User Emacs configuration (NixOS) -*- lexical-binding: t -*-

;; Mirrors omarchy-emacs/.config/emacs/init.el, minus Omarchy's theme/font
;; syncing. Packages come from MELPA through use-package, not from Nix, so
;; the system only has to provide Emacs itself (30+, for :vc) and the
;; programs the packages run: fd, rg, direnv, git, sbcl (per project).

;;; Basics
;; Don't pop up *Warnings* for compiler noise in third-party packages
;; while they are natively compiled in the background.
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(global-auto-revert-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(add-hook 'text-mode-hook #'auto-fill-mode)
(setq-default fill-column 80)

;; Keep M-x customize output out of this (git-tracked) file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;; Look
;; Let the compositor size the frame in pixels (a tiling Wayland
;; compositor such as niri gives the window a tile; otherwise pgtk Emacs
;; keeps its own character-grid size and the frame stays small).
(setq frame-resize-pixelwise t)
;; Omarchy syncs these with its themes; here they are set once.
(set-frame-font "Iosevka-14" t t)
(load-theme 'modus-vivendi t)

;;; Shells
;; Emacs runs its own commands through bash; interactive shells are fish.
(setq shell-file-name (executable-find "bash"))
(let ((fish (executable-find "fish")))
  (when fish
    (setq-default vterm-shell fish)
    (setq-default explicit-shell-file-name fish)))

;;; Packages
;; Packages come from MELPA and are installed on first start.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

;;; Multiple cursors
(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds)
  (evil-define-key 'visual 'global
    "R" #'evil-multiedit-match-all))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

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

;;; Org mode
(setq org-agenda-files '("~/org"))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t))))

;; Archive DONE items to Done.org under current date
(defun my/archive-done-task ()
  "Archive current task to Done.org under today's date."
  (interactive)
  (let* ((archive-file (expand-file-name "~/org/Done.org"))
         (location (format "%s::" archive-file)))
    (org-set-property "COMPLETED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
    (setq org-archive-location location)
    (org-archive-subtree)))

;; Automatically archive when marked DONE
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (and (string= org-state "DONE")
                       (string= (buffer-file-name) (expand-file-name "~/org/Todo.org")))
              (my/archive-done-task))))

(defun my/unarchive-task ()
  "Move task at point from Done.org back to its original file as TODO."
  (interactive)
  (let* ((archive-file (org-entry-get nil "ARCHIVE_FILE"))
         (archive-olpath (org-entry-get nil "ARCHIVE_OLPATH")))
    (unless archive-file
      (user-error "No ARCHIVE_FILE property found — can't determine origin"))
    ;; Cut the subtree
    (org-cut-subtree)
    ;; Remove empty date heading if it has no children left
    (when (and (org-at-heading-p)
               (= (org-outline-level) 1)
               (save-excursion
                 (let ((end (save-excursion (org-end-of-subtree t t) (point))))
                   (forward-line 1)
                   (>= (point) end))))
      (delete-region (line-beginning-position)
                     (save-excursion (forward-line 1) (point))))
    (save-buffer)
    ;; Go to the original file
    (find-file archive-file)
    (goto-char (point-max))
    ;; Paste the subtree
    (org-paste-subtree 1)
    ;; Change state to TODO and clean up archive properties
    (org-todo "TODO")
    (dolist (prop '("COMPLETED" "ARCHIVE_TIME" "ARCHIVE_FILE"
                    "ARCHIVE_OLPATH" "ARCHIVE_CATEGORY"
                    "ARCHIVE_TODO" "ARCHIVE_ITAGS"))
      (org-delete-property prop))
    (save-buffer)
    (message "Task moved back to %s as TODO" archive-file)))

;;; Project environments
;; A project with an .envrc (for a Nix flake, "use flake") gets its
;; environment in each of its buffers, and so do the processes started from
;; them: compile, shells, and SLY's `sbcl', which is then the project's own,
;; with its libraries. Needs direnv, and nix-direnv to cache the flake's
;; shell. A new .envrc must be allowed once: M-x envrc-allow.
(use-package envrc
  ;; Turned on last, so that its buffer-local environment is set before
  ;; other modes' hooks start processes.
  :hook (after-init . envrc-global-mode))

;;; Tree-sitter
;; Grammars are compiled on demand (needs a C compiler on PATH), and the
;; built-in modes are remapped to their tree-sitter equivalents.
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("uv" "run" "basedpyright-langserver" "--stdio"))))

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
     additional-movement  ; ( ) = up a level, [ ] = prev/next paren, { } see below
     slurp/barf-cp        ; > and < to slurp/barf
     wrap))               ; M-( wraps the sexp at point in parens
  ;; H and L do what { and } do (in place of back/forward sexp):
  ;; L = next opening paren, H = previous closing paren
  (evil-define-key 'motion lispyville-mode-map
    "L" #'lispyville-next-opening
    "H" #'lispyville-previous-closing))

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
  ;; for a Lisp on another machine (see "Lisp images on other machines").
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
  "C" 'sly-connect)

;;; Lisp images on other machines
;; A project checked out on a server and edited over TRAMP (/ssh:HOST:...),
;; with its Lisp image's Slynk port forwarded here by SSH; SPC m C connects.
(defvar my/sly-ssh-hosts '("netcup-dev")
  "The ~/.ssh/config hosts whose Lisp images SLY reaches through a tunnel.
Each name must also be the host's (machine-instance), which is how SLY
tells where an image runs.")

;; Remote projects get their .dir-locals.el too, as local ones do.
(setq enable-remote-dir-locals t)

;; consult-fd and consult-ripgrep start a process per search; over TRAMP,
;; run those over plain SSH instead of through TRAMP's shell. Faster.
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'remote-direct-async-process)

(with-eval-after-load 'sly-tramp
  ;; An image on HOST names its files /home/..., which here are the TRAMP
  ;; names /ssh:HOST:/home/..., the ones you open by hand, so gd reuses open
  ;; buffers. (sly-tramp's own translator would add user@ to them.)
  (dolist (host my/sly-ssh-hosts)
    (add-to-list 'sly-filename-translations
                 (list (concat "^" (regexp-quote host) "$")
                       (lambda (emacs-filename)
                         (or (file-remote-p emacs-filename 'localname) emacs-filename))
                       (lambda (lisp-filename)
                         (concat "/ssh:" host ":" lisp-filename))))))

;;; init.el ends here
