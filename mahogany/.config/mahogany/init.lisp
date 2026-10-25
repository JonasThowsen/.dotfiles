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
