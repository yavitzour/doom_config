;;; +misc-functions.el -*- lexical-binding: t; -*-

;; Collection of miscellaneous functions I collected from various places

;;; Code:

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


(defun my/vterm-execute-current-line ()
  "Insert text of current line in vterm and execute."
  (interactive)
  (require 'vterm)
  (eval-when-compile (require 'subr-x))
  (let ((command (string-trim (buffer-substring
                               (save-excursion
                                 (beginning-of-line)
                                 (point))
                               (save-excursion
                                 (end-of-line)
                                 (point))))))
    (let ((buf (current-buffer)))
      (unless (get-buffer vterm-buffer-name)
        (vterm))
      (display-buffer vterm-buffer-name t)
      (switch-to-buffer-other-window vterm-buffer-name)
      (vterm--goto-line -1)
      (message command)
      (vterm-send-string command)
      (vterm-send-return)
      (switch-to-buffer-other-window buf)
      )))


(defun ediff-compare-region-clipboard (begin end)
  "Compare current region (BEGIN END) with last saved text in clipboard."
  (interactive "r")
  (save-excursion
    (let ((selected-region (buffer-substring begin end))
          (clipboard-buffer (get-buffer-create "*ediff-clipboard*"))
          (region-buffer (get-buffer-create "*ediff-region*")))
      (with-current-buffer clipboard-buffer
        (insert (car kill-ring)))
      (with-current-buffer region-buffer
        (insert selected-region))
      (ediff-buffers clipboard-buffer region-buffer))))


;; (defun notify-compilation-result(buffer msg)
;;   "Notify that the compilation is finished,
;; close the *compilation* buffer if the compilation is successful,
;; and set the focus back to Emacs frame"
;;   (if (string-match "^finished" msg)
;;     (progn
;;      (delete-windows-on buffer)
;;      (tooltip-show "\n Compilation Successful :-) \n "))
;;     (tooltip-show "\n Compilation Failed :-( \n "))
;;   (setq current-frame (car (car (cdr (current-frame-configuration)))))
;;   (select-frame-set-input-focus current-frame)
;;   )

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished.
Close BUFFER if the compilation is MSG has finished."
  (if (string-match "^finished" msg)
      (progn
        (delete-windows-on buffer)
        (message "No Compilation Errors!")
        )))

(add-to-list 'compilation-finish-functions
             'notify-compilation-result)

(defun kill-buffer-other-window ()
  "Kill buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window 1)
  (delete-other-windows))

(defun dired-dim-git-ignores ()
  "Dim out .gitignore contents."
  (when-let ((_ (require 'vc))
             (ignores (vc-default-ignore-completion-table 'git ".gitignore"))
             (exts (make-local-variable 'completion-ignored-extensions)))
    (dolist (item ignores) (add-to-list exts item))))

(add-hook! 'dired-mode-hook #'dired-dim-git-ignores)

(defun my-previous-line ()
  "Re-Implemetation of 'previous-line' to circumvent python-mls override of 'previous-line'."
  (interactive)
  (forward-line -1))

(defun next-error-or-flycheck-next-error ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

(defun previous-error-or-flycheck-previous-error ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (call-interactively 'flycheck-previous-error)
    (call-interactively 'previous-error)))

(provide '+misc-functions)
;;; +misc-functions.el ends here
