;;; Helper functions
(defun open-file-in-new-tab (file)
  "Open FILE in a new tab."
  (interactive "fFile: ")
  (tab-bar-new-tab)
  (find-file file))

(defun open-eshell-in-split ()
  (interactive)
  (evil-window-split)
  (eshell))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq make-backup-files nil)
(global-auto-revert-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;;; Theme and font
(load-theme 'rosebones t)
(set-frame-font "0xProto-14" t t)

;;; General
(require 'general)

;;; Evil mode configuration
(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(when (require 'evil-collection nil t)
  (evil-collection-init))

(evil-set-undo-system 'undo-redo)

;; Evil packages
(global-evil-surround-mode 1)

;; General evil stuff
(general-evil-setup t)

(general-create-definer my-leader
  :states '(normal visual motion)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC")

(my-leader
  "gg" 'magit)

(my-leader
  "a" 'avy-goto-char-timer)

(my-leader
  "ff" 'affe-find
  "fs" 'affe-grep
  "fi" 'consult-line)

(my-leader
  "t" 'open-eshell-in-split)

;;; Vertico
(require 'vertico)
(vertico-mode)

;; Recommended to save across Emacs restarts
(savehist-mode)

;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles partial-completion))))
(setq completion-category-defaults nil)
(setq completion-pcm-leading-wildcard t)

;;; Embark
(require 'embark)
(require 'embark-consult)
(global-set-key (kbd "C-.") #'embark-act)

(defun my-embark-open-in-new-tab ()
  "Open current embark target in a new tab."
  (interactive)
  (let ((target (car (embark--targets))))
    (embark--act #'open-file-in-new-tab target t)))

(defun my-embark-select-and-next ()
  "Select current candidate and move to next."
  (interactive)
  (embark-select)
  (vertico-next))

(with-eval-after-load 'embark
  (define-key minibuffer-local-map (kbd "C-.") #'embark-act)
  (define-key minibuffer-local-completion-map (kbd "C-.") #'embark-act)
  (define-key embark-file-map (kbd "C-t") #'open-file-in-new-tab))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-t") #'my-embark-open-in-new-tab)
  (define-key vertico-map (kbd "C-SPC") #'my-embark-select-and-next)
  (define-key vertico-map (kbd "C-q") #'embark-export))

;;; Wgrep
(require 'wgrep)
