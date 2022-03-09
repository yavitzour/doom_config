;;; +prot-functions.el -*- lexical-binding: t; -*-

;;; Odds and ends from my dotemacs (.emacs)

;; All those are from my Emacs setup.  The literate configuration is on
;; my website: <https://protesilaos.com/dotemacs>.  Otherwise check the
;; Git source: <https://gitlab.com/protesilaos/dotfiles>.

;;;; Excerpt from prot-comment.el

;; helper code from prot-common.el, which is `require'd by
;; prot-comment.el
(defvar prot-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `prot-common-line-regexp-p'.")

(defun prot-common-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`prot-common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type prot-common--line-regexp-alist))))))

;; and what follows is from prot-comment.el
(defcustom prot-comment-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type '(repeat string)
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-concise "%F"
  "Specifier for date in `prot-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :type 'string
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-verbose "%F %T %z"
  "Like `prot-comment-timestamp-format-concise', but longer."
  :type 'string
  :group 'prot-comment)

(defvar prot-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prot-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car prot-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prot-comment--keyword-hist def)))

;;;###autoload
(defun prot-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `prot-comment-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`prot-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `prot-comment-timestamp-format-verbose'."
  (interactive
   (list
    (prot-comment--keyword-prompt prot-comment-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   prot-comment-timestamp-format-verbose
                 prot-comment-timestamp-format-concise))
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond
     ((or (eq beg (point-at-bol))
          (prot-common-line-regexp-p 'empty))
      (let* ((maybe-newline (unless (prot-common-line-regexp-p 'empty 1) "\n")))
        ;; NOTE 2021-07-24: we use this `insert' instead of
        ;; `comment-region' because of a yet-to-be-determined bug that
        ;; traps `undo' to the two states between the insertion of the
        ;; string and its transformation into a comment.
        (insert
         (concat comment-start
                 ;; NOTE 2021-07-24: See function `comment-add' for
                 ;; why we need this.
                 (make-string
                  (comment-add nil)
                  (string-to-char comment-start))
                 comment-padding
                 string
                 comment-end))
        (indent-region beg (point))
        (when maybe-newline
          (save-excursion (insert maybe-newline)))))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

;;;; Excerpt from prot-diff.el

;;;###autoload
(defun prot-diff-buffer-dwim (&optional arg)
  "Diff buffer with its file's last saved state, or run `vc-diff'.
With optional prefix ARG (\\[universal-argument]) enable
highlighting of word-wise changes (local to the current buffer)."
  (interactive "P")
  (let ((buf))
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (unless diff-refine
          (setq-local diff-refine 'font-lock))))))

(defvar-local prot-diff--refine-diff-state 0
  "Current state of `prot-diff-refine-dwim'.")

;;;###autoload
(defun prot-diff-refine-cycle ()
  "Produce buffer-local, 'refined' or word-wise diffs in Diff mode.

Upon first invocation, refine the diff hunk at point or, when
none exists, the one closest to it.  On second call, operate on
the entire buffer.  And on the third time, remove all word-wise
fontification."
  (interactive)
  (let ((point (point)))
    (pcase prot-diff--refine-diff-state
      (0
       (diff-refine-hunk)
       (setq prot-diff--refine-diff-state 1))
      (1
       (setq-local diff-refine 'font-lock)
       (font-lock-flush)
       (goto-char point)
       (setq prot-diff--refine-diff-state 2))
      (_
       (revert-buffer)
       (goto-char point)
       (recenter)
       (setq prot-diff--refine-diff-state 0)))))

;;;; Excerpt from prot-simple.el

;;;;; Narrow DWIM

;; this is the helper code from prot-common.el
;;;###autoload
(defun prot-common-window-bounds ()
  "Determine start and end points in the window."
  (list (window-start) (window-end)))

;;;###autoload
(defun prot-simple-narrow-visible-window ()
  "Narrow buffer to wisible window area.
Also check `prot-simple-narrow-dwim'."
  (interactive)
  (let* ((bounds (prot-common-window-bounds))
         (window-area (- (cadr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cadr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun prot-simple-narrow-dwim ()
  "Do-what-I-mean narrowing.
If region is active, narrow the buffer to the region's
boundaries.

If no region is active, narrow to the visible portion of the
window.

If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (let ((beg (region-beginning))
          (end (region-end)))
      (narrow-to-region beg end)))
   ((null (buffer-narrowed-p))
    (prot-simple-narrow-visible-window))
   (t
    (widen)
    (recenter))))

;;;;; Insert date at point

(defcustom prot-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `prot-simple-inset-date'."
  :type 'string
  :group 'prot-simple)

(defcustom prot-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `prot-simple-inset-date'."
  :type 'string
  :group 'prot-simple)

;;;###autoload
(defun prot-simple-insert-date (&optional arg)
  "Insert the current date as `prot-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `prot-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date prot-simple-date-specifier)
         (time prot-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

;;;;; Escape URL/Email

(autoload 'ffap-url-at-point "ffap")
(defvar ffap-string-at-point-region)

;;;###autoload
(defun prot-simple-escape-url ()
  "Wrap URL in angled brackets."
  (interactive)
  (when-let ((url (ffap-url-at-point)))
    (let* ((reg ffap-string-at-point-region)
           (beg (car reg))
           (end (cadr reg))
           (string (if (string-match-p "^mailto:" url)
                       (substring url 7)
                     url)))
      (delete-region beg end)
      (insert (format "<%s>" string)))))

;;;;; Rename buffer and file

;; A variant of this is present in the crux.el package by Bozhidar
;; Batsov.
;;;###autoload
(defun prot-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

;;;; Excerpt from prot-search.el

;;;;; occur

;; I copy this from `browse-url-button-regexp' simply because there are
;; contexts where we do not need that dependency.
(defvar prot-common-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	     (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.
Copy of variable `browse-url-button-regexp'.")

(autoload 'goto-address-mode "goto-addr")

;;;###autoload
(defun prot-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((buf-name (format "*links in <%s>*" (buffer-name))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 prot-common-url-regexp "\\&" (list (current-buffer)) buf-name)
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;###autoload
(defun prot-search-occur-browse-url ()
  "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `prot-search-occur-urls'."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp prot-common-url-regexp nil t)
        (push (match-string-no-properties 0) matches)))
    (funcall browse-url-browser-function
             (completing-read "Browse URL: " matches nil t))))

;;;;; grep

(defvar prot-search--grep-hist '()
  "Input history of grep searches.")

;;;###autoload
(defun prot-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.

Search in the current directory using `lgrep'.  With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting from the current directory with `rgrep'."
  (interactive
   (list
    (read-from-minibuffer (concat (if current-prefix-arg
                                      (propertize "Recursive" 'face 'warning)
                                    "Local")
                                  " grep for PATTERN: ")
                          nil nil nil 'prot-search--grep-hist)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
    (lgrep regexp "*" default-directory)
    (add-to-history 'prot-search--grep-hist regexp)))

;;;; Honourable mentions (in no particular order):

;; 1. prot-fonts.el: lets me specify comprehensive sets of font
;;    specifications which I can activate on demand.
;;
;; 2. prot-diary.el: I did a recent video demo about diary+calendar and
;;    how I use them to keep track of time-sensitive events.
;;
;; 3. prot-eww.el: Lots of extras for browsing the web with EWW and now
;;    with Elpher (I did a video some months ago, but will have to cover
;;    the up-and-coming features once the time is right).
;;
;; 4. prot-notmuch.el: There is a recent video about how I use notmuch,
;;    but it does not include the various extras found in that file,
;;    including tagging, custom widgets...
