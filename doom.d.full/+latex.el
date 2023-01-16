;;; +latex.el -*- lexical-binding: t; -*-

;; Latex Helpers

(defun slot/LaTeX-self-insert (&optional arg char)
  "`self-insert-command' for LaTeX mode.
If the previous word is just a single character, surround it with
dollar signs.  If already in math mode, do nothing.  If the
character is a single `a', do nothing.
If called with a single \\[universal-argument], just call
`self-insert-command'."
  (interactive "P")
  (pcase arg
    ('(4) (self-insert-command 1))
    (_ (let ((ppoint (save-excursion (backward-word)       (point)))
             (ipoint (save-excursion (back-to-indentation) (point)))
             (word   (word-at-point)))
         (unless (or (length> word 1)   ; longer than a single character
                     (not word)
                     (= ipoint ppoint)  ; the first thing on a new line
                     (equal "a" word)
                     (number-at-point)
                     (texmathp))
           (-let (((open . close) math-delimiters-inline))
             (backward-char)
             (insert open)
             (forward-char 1)
             (insert close)))
         (self-insert-command 1 char)))))


(defun slot/LaTeX-space (&optional arg)
  "Insert a space; or not.
In case the previous character was already a space, insert
\\, instead."
  (interactive "P")
  (if (and (= ?\s (char-before)) (texmathp))
      (insert "\\, ")
    (slot/LaTeX-self-insert arg ?\s)))
