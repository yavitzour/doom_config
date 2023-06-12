;;; +denote.el -*- lexical-binding: t; -*-

;; denote configuration

(use-package! denote
  :init
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/denote/"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.
  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)

  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well.

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  :bind
  (:map global-map
        ("C-c d j" . my-denote-journal) ; our custom command
        ("C-c d n" . denote)
        ("C-c d o" . denote-open-or-create)
        ("C-c d N" . denote-type)
        ("C-c d d" . denote-date)
        ("C-c d s" . denote-subdirectory)
        ("C-c d t" . denote-template)
        ;; If you intend to use Denote with a variety of file types, it is
        ;; easier to bind the link-related commands to the `global-map', as
        ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
        ;; `markdown-mode-map', and/or `text-mode-map'.
        ("C-c d i" . denote-link) ; "insert" mnemonic
        ("C-c d I" . denote-link-add-links)
        ("C-c d b" . denote-link-backlinks)
        ("C-c d f f" . denote-link-find-file)
        ("C-c d f b" . denote-link-find-backlink)
        ;; Note that `denote-rename-file' can work from any context, not just
        ;; Dired bufffers.  That is why we bind it here to the `global-map'.
        ("C-c d r" . denote-rename-file)
        ("C-c d R" . denote-rename-file-using-front-matter)
        )
  (:map dired-mode-map
        ("C-c C-d C-i" . denote-link-dired-marked-notes)
        ("C-c C-d C-r" . denote-dired-rename-marked-files)
        ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  )

;; Here is a custom, user-level command from one of the examples we
;; showed in this manual.  We define it here and add it to a key binding
;; below.
(defun my-denote-journal ()
  "Create an entry tagged 'journal', while prompting for a title."
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))
