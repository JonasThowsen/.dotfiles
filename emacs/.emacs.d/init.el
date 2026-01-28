;;; Custom keymaps
(global-set-key (kbd "C-SPC") 'completion-at-point)

;;; Wayland clipboard support (for wl-clipboard)
(setq wl-copy-process nil)

(defun wl-copy (text)
  "Copy TEXT to Wayland clipboard using wl-copy."
  (setq wl-copy-process (make-process :name "wl-copy"
                                       :buffer nil
                                       :command '("wl-copy" "-f" "-n")
                                       :connection-type 'pipe
                                       :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  "Paste from Wayland clipboard using wl-paste."
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil
    (shell-command-to-string "wl-paste -n 2>/dev/null")))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

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
(setq ring-bell-function 'ignore)

(setq org-agenda-files '("~/org"))

(setq make-backup-files nil)
(global-auto-revert-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;;; Theme and font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'default-black t)
(set-frame-font "JetBrains Mono Nerd Font-18" t t)

;;; Org mode
(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   ))

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
  "a" 'avy-goto-char-timer)

(my-leader
  "f" 'affe-find
  "p" 'project-find-file
  "b" 'consult-buffer
  "s" 'affe-grep
  "i" 'consult-line)

(my-leader
  "t" 'open-eshell-in-split)

(my-leader
  "r" 'query-replace-regexp
  "R" 'project-query-replace-regexp)

;;; Magit
(defun magit-diff-visit-file-in-new-tab ()
  (interactive)
  (tab-bar-new-tab)
  (magit-diff-visit-file))

(my-leader
  "gg" 'magit)

(my-leader
  :keymaps 'magit-mode-map
  "o" 'magit-diff-visit-file-in-new-tab)

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

(defun find-file-h (target)
  (split-window-below)
  (other-window 1)
  (find-file target))

(defun split-vertical-current-completion-candidate ()
  (interactive)
  (embark--act #'find-file-other-window (car (embark--targets)) t))

(defun split-horizontal-current-completion-candidate ()
  (interactive)
  (embark--act #'+embark:find-file-h (car (embark--targets)) t))

(with-eval-after-load 'embark
  (define-key minibuffer-local-map (kbd "C-.") #'embark-act)
  (define-key minibuffer-local-completion-map (kbd "C-.") #'embark-act)
  (define-key embark-file-map (kbd "C-t") #'open-file-in-new-tab))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-t") #'my-embark-open-in-new-tab)
  (define-key vertico-map (kbd "C-SPC") #'my-embark-select-and-next)
  (define-key vertico-map (kbd "C-q") #'embark-export)
  (define-key vertico-map (kbd "C-v") #'split-vertical-current-completion-candidate)
  (define-key vertico-map (kbd "C-s") #'split-horizontal-current-completion-candidate))

(require 'marginalia)
(marginalia-mode)

(require 'corfu)
(global-corfu-mode)

;;; Wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(defun open-grep-goto-error-new-tab ()
  "Open grep result in a new tab."
  (interactive)
  (tab-bar-new-tab)
  (compile-goto-error)
  (delete-other-windows))

(my-leader
  :keymaps 'grep-mode-map
  "o" 'open-grep-goto-error-new-tab
  "e" 'wgrep-change-to-wgrep-mode
  "d" 'wgrep-abort-changes)

(require 'envrc)
(envrc-global-mode)

;;; Tree-sitter mode associations
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

;; Remap built-in modes to their tree-sitter equivalents
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(javascript-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(csharp-mode . csharp-ts-mode)
	(html-mode . html-ts-mode)))
