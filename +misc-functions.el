;;; +misc-functions.el -*- lexical-binding: t; -*-

;; Collection of miscellaneous functions I collected from various places

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (f) Full, (d) Directory, (n) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

;; comments - https://stackoverflow.com/a/9697222/14042240
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    ;; (forward-line)
    )
  )

;; backword-kill-word if no region active
(defun obar/kill-region-or-backward-word ()
  "Kill region if active, else backword-kill-word"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;; Capitalize, upcase and downcase word-at-point for real. Taken from
;; https://christiantietze.de/posts/2021/03/change-case-of-word-at-point
(defun ct/word-boundary-at-point-or-region (&optional callback)
  "Return the boundary (beginning and end) of the word at point, or region, if any.
  Forwards the points to CALLBACK as (CALLBACK p1 p2), if present.

URL: https://christiantietze.de/posts/2021/03/change-case-of-word-at-point/"
  (let ((deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when callback
      (funcall callback $p1 $p2))
    (list $p1 $p2)))

(defun ct/capitalize-region (p1 p2)
  (downcase-region p1 p2)
  (upcase-initials-region p1 p2))

(defun ct/capitalize-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'ct/capitalize-region))

;; (defun ct/capitalize-word-at-point ()
;;   (interactive)
;;   (ct/word-boundary-at-point-or-region #'upcase-initials-region))

(defun ct/downcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'downcase-region))

(defun ct/upcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'upcase-region))

(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(defun recenter-correctly ()
  "don't scroll beyond end of file and show line #"
  (interactive)
  (let ((pt (point)))
    (if (save-excursion (forward-line (1- (/ (window-height) 2)))
                        (end-of-line)
                        (eobp))
        (progn (goto-char (point-max))
               (recenter)
               (if (eq (window-height) (* (/ (window-height) 2) 2))
                   (move-to-window-line 0)
                 (move-to-window-line 1))
               (recenter)
               (goto-char pt))
      (recenter)))
  (what-line))

(defun search-specific-glob ()
  "Example for using counsel-projectile-rg to search only within certain extensions"
  (interactive)
  (let ((glob (ivy-completing-read "Glob?: " '("*.cljs"
                                               "*.clj"
                                               "*.md"
                                               "*.styl"
                                               "*.css"))))
    (counsel-projectile-rg (concat "--glob " glob))))

(defun projectile-search-py ()
  "Use counsel-projectile-rg to search only .py files"
  (interactive)
  (let ((glob (ivy-completing-read "Glob?: " '("*.py"))))
    (counsel-projectile-rg (concat "--glob " glob))))
