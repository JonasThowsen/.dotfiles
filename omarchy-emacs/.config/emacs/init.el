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
  ;; In-buffer completion (e.g. SLIME symbol completion) through the minibuffer
  (setq completion-in-region-function #'consult-completion-in-region))

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
  "i" 'consult-line)

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

;;; Common Lisp / SLIME
;; SLIME comes from pacman (emacs-slime), not MELPA.
(use-package slime
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/slime"
  :commands (slime slime-mode)
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  :hook (lisp-mode . slime-mode)
  :config
  ;; evil-collection handles the rest: evaluating the sexp under the cursor
  ;; in normal state, and vim keys in the REPL, debugger and inspector.
  (evil-set-initial-state 'slime-repl-mode 'insert)
  ;; Record gd in Evil's jump list, so C-o / C-i hop back and forth
  (evil-add-command-properties #'slime-edit-definition :jump t))

(my-local-leader
  :keymaps 'lisp-mode-map
  "'" 'slime
  "e" 'slime-eval-last-expression
  "d" 'slime-eval-defun
  "r" 'slime-eval-region
  "b" 'slime-eval-buffer
  "c" 'slime-compile-defun
  "k" 'slime-compile-and-load-file
  "z" 'slime-switch-to-output-buffer
  "h" 'slime-describe-symbol
  "H" 'slime-hyperspec-lookup
  "m" 'slime-macroexpand-1
  "gd" 'slime-edit-definition
  "gb" 'slime-pop-find-definition-stack)

;;; init.el ends here
