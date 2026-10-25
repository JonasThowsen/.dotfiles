;;; early-init.el --- Before the first frame -*- lexical-binding: t -*-

;; Frame geometry is decided here, before the initial frame exists, so
;; that init.el does not change it afterwards. Under a tiling Wayland
;; compositor (niri), the compositor sizes the window; an "implied
;; resize" by Emacs, after removing the tool bar or changing the font,
;; leaves the frame drawn smaller than its window.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(font . "Iosevka-14") default-frame-alist)

;;; early-init.el ends here
