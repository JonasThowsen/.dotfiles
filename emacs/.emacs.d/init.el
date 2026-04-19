;;; Fish shell stuff
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell "/run/current-system/sw/bin/fish")
(setq-default explicit-shell-file-name "/run/current-system/sw/bin/fish")

;;; Custom keymaps
(global-set-key (kbd "C-SPC") 'completion-at-point)

;; Expreg
(global-set-key (kbd "C-=") 'expreg-expand)
(global-set-key (kbd "C--") 'expreg-contract)

;; Projects
(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers ".project"))

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

(add-hook 'text-mode-hook #'auto-fill-mode)
(setq-default fill-column 80)

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
(load-theme 'catppuccin t)
(set-frame-font "Iosevka-18" t t)

;;; FFFElisp
(require 'fff)
(global-set-key (kbd "C-c f f") #'fff-find-file)
(global-set-key (kbd "C-c f g") #'fff-grep)

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
  "a" 'avy-goto-char-2)

(my-leader
  "f" 'fff-find-file
  "b" 'consult-buffer
  "s" 'fff-grep
  "i" 'consult-line)

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

;;; Org mode
(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   ))

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

;;; Multiple cursors

;; evil-multiedit
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)
(evil-define-key 'visual 'global
  "R" #'evil-multiedit-match-all)

;; evil-mc
(require 'evil-mc)
(global-evil-mc-mode 1)
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

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

(require 'marginalia)
(marginalia-mode)

(require 'corfu)
(global-corfu-mode)

;;; Wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'envrc)
(envrc-global-mode)

;;; Tree-sitter mode associations
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Remap built-in modes to their tree-sitter equivalents
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(javascript-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(tsx-mode . tsx-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(csharp-mode . csharp-ts-mode)
	(html-mode . html-ts-mode)))

;; Eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("uv" "run" "basedpyright-langserver" "--stdio"))))
