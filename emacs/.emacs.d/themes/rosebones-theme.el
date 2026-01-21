(deftheme rosebones
  "Rosebones-inspired: calm syntax, strong search/avy highlights.")

(let* (;; From your vim colorscheme (dark)
       (bg      "#1A1825")
       (fg      "#E1D4D4")

       (comment "#69657E")
       (string  "#BC9493")
       (ident   "#CAB0AF")
       (type    "#DFDEF1")
       (delim   "#7D7997")

       (cursor-bg "#E7DDDD")
       (hlline   "#222030")
       (regionbg "#523A39")

       (linenum  "#625D7F")
       (nontext  "#565172")

       ;; “Functional contrast” (search/jump)
       (searchbg "#673592")  ;; Search
       (isearchbg "#B48DE0") ;; IncSearch

       ;; Accents from palette
       (blue    "#9CCFD8")
       (purple  "#C4A7E7")
       (warn    "#F6C074")
       (error   "#EB7193")
       (ok      "#317490"))

  (custom-theme-set-faces
   'rosebones

   ;; Base UI
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor  ((t (:background ,cursor-bg :foreground ,bg))))
   `(fringe  ((t (:background ,bg :foreground ,linenum))))
   `(hl-line ((t (:background ,hlline))))
   `(region  ((t (:background ,searchbg))))
   `(minibuffer-prompt ((t (:foreground ,blue :weight bold))))
   `(vertical-border ((t (:foreground ,linenum))))
   `(shadow ((t (:foreground ,nontext))))
   `(link ((t (:foreground ,blue :underline t))))

   ;; Line numbers
   `(line-number ((t (:foreground ,linenum :background ,bg))))
   `(line-number-current-line ((t (:foreground ,fg :background ,bg :weight bold))))

   ;; Search / match (this is what makes things “pop”)
   `(isearch ((t (:background ,isearchbg :foreground ,bg :weight bold))))
   `(lazy-highlight ((t (:background ,searchbg :foreground ,fg))))
   `(match ((t (:background ,searchbg :foreground ,fg))))
   `(show-paren-match ((t (:background ,searchbg :foreground ,fg :weight bold))))

   ;; Syntax (calm, not flashy)
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-string-face  ((t (:foreground ,string :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,ok))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,ident))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-constant-face ((t (:foreground ,string))))
   `(font-lock-builtin-face ((t (:foreground ,blue))))
   `(font-lock-delimiter-face ((t (:foreground ,delim))))
   `(font-lock-warning-face ((t (:foreground ,warn :weight bold))))

   ;; Errors/warnings (built-in faces used all over Emacs)
   `(error   ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warn))))
   `(success ((t (:foreground ,ok))))

   ;; Avy — high-contrast, easy to spot, but still on-palette
   `(avy-lead-face   ((t (:background ,isearchbg :foreground ,bg :weight bold))))
   `(avy-lead-face-0 ((t (:background ,blue     :foreground ,bg :weight bold))))
   `(avy-lead-face-1 ((t (:background ,warn     :foreground ,bg :weight bold))))
   `(avy-lead-face-2 ((t (:background ,purple   :foreground ,bg :weight bold))))
   `(avy-background-face ((t (:foreground ,comment))))
   `(avy-goto-char-timer-face ((t (:background ,isearchbg :foreground ,bg))))))

(provide-theme 'rosebones)
