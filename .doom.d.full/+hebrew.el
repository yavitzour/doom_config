;;; +hebrew.el -*- lexical-binding: t; -*-

(setq-default bidi-display-reordering nil)
(defun bidi-reordering-toggle ()
  "Toggle bidirectional display reordering"
  (interactive)
  (setq bidi-display-reordering (not bidi-display-reordering))
  (message "bidi-display-reordering is %s" bidi-display-reordering)
  )

(setq-default bidi-paragraph-direction 'left-to-right)
(defun bidi-direction-toggle ()
  "Switch the explitict direction of text for the current buffer"
  (interactive)
  (setq bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (setq bidi-paragraph-direction 'left-to-right)
    (setq  bidi-paragraph-direction 'right-to-left))
  (message "%s" bidi-paragraph-direction)
  )


(defun default-input-font ()

  "changes the set-input-method to nil and selects a default font
bound to C-c d"
  (interactive)
  (set-input-method nil)
  (face-remap-add-relative 'default :family "Consolas" :height 110)
  )

(defun hebrew-input-font ()
  "changes the set-input-method to Hebrew  and selects another font
bound to C-c h"
  (interactive)
  (set-input-method "hebrew") ; you can specify like `hebrew-biblical-tiro`
  (face-remap-add-relative 'default :family "DejaVu Sans" :height 120)
  )

(defun enable-hebrew-mode ()
  "enbale hebrew mode"
  (interactive)
  (setq bidi-display-reordering t)
  (setq bidi-paragraph-direction 'right-to-left)
  (hebrew-input-font)
  )

(defun disable-hebrew-mode ()
  "disable hebrew mode"
  (interactive)
  (setq bidi-display-reordering nil)
  (setq bidi-paragraph-direction 'left-to-right)
  (default-input-font)
  )

(defvar hebrew-mode-active nil)
(defun toggle-hebrew-mode ()
  "toggle hebrew mode"
  (interactive)
  (if hebrew-mode-active
    (disable-hebrew-mode)
    (enable-hebrew-mode))
  (setq hebrew-mode-active (not hebrew-mode-active))
  )

;; (global-set-key (kbd "C-c d") 'default-input-font)
(global-set-key (kbd "C-c t h") 'toggle-hebrew-mode)
