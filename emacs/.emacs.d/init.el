;; Custom keymaps
(global-set-key (kbd "C-SPC") 'completion-at-point)

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
(load-theme 'modus-operandi t)
(set-frame-font "Iosevka-18" t t)

;;; Meow
(require 'view)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("C-j" . View-scroll-half-page-forward)
   '("C-k" . View-scroll-half-page-backward)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)

;;; FFFElisp
(require 'fff)
(global-set-key (kbd "C-c f f") #'fff-find-file)
(global-set-key (kbd "C-c f g") #'fff-grep)

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
