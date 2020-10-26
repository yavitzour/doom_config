;;; +org.el -*- lexical-binding: t; -*-

;; org configuration - heavily influenced by https://www.labri.fr/perso/nrougier/GTD/index.html
(after! org
  (setq org-directory "~/org/"                      ; let's put files here
        org-use-property-inheritance t              ; it's convenient to have properties inherited
        org-log-done 'time                          ; having the time a item is done sounds convininet
        org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
        org-export-in-background t                  ; run export processes in external emacs process
        org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
        org-hide-emphasis-markers t
        org-tags-column -77
        org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-news-news)
                               (file . find-file-other-window)
                               (wl . wl-other-frame))
        )

  (setq org-agenda-files (list (concat (file-name-as-directory org-directory) "inbox.org")
                               (concat (file-name-as-directory org-directory) "agenda.org")
                               (concat (file-name-as-directory org-directory) "projects.org")))

  ;; (setq org-capture-templates nil)
  ;; (setq org-capture-templates
  ;;       `(("i" "Inbox" entry  (file "inbox.org")
  ;;          ,(concat "* TODO %?\n"
  ;;                   "/Entered on/ %U"))
  ;;         ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
  ;;          ,(concat "* %? :meeting:\n"
  ;;                   "<%<%Y-%m-%d %a %H:00>>"))
  ;;         ("n" "Note" entry  (file "notes.org")
  ;;          ,(concat "* Note (%a)\n"
  ;;                   "/Entered on/ %U\n" "\n" "%?"))
  ;;         ))

  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry  (file "inbox.org")
                 "* TODO %?\n/Entered on/ %U"))
  (add-to-list 'org-capture-templates
               '("n" "Note" entry  (file "notes.org")
                 "* Note (%a)\n/Entered on/ %U\n" "\n" "%?"))

  (setq org-refile-targets
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (defun log-todo-next-creation-date (&rest ignore)
    "Log STRT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "STRT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))

  ;; (define-key global-map (kbd "C-c i") 'org-capture-inbox)

  ;; have list bullets change with depth
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

  ;; org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  ;; Use the following on buffers that use org-crypt:
  ;; # -*- buffer-auto-save-file-name: nil; -*-
  )

(after! org-agenda
  ;; Aaron Harris's solution for skip-by-tag (https://stackoverflow.com/a/33444799/14042240)
  (defun my/org-match-at-point-p (match)
    "Return non-nil if headline at point matches MATCH.
     Here MATCH is a match string of the same format used by
`org-tags-view'."
    (funcall (cdr (org-make-tags-matcher match))
             (org-get-todo-state)
             (org-get-tags-at)
             (org-reduced-level (org-current-level))))

  (defun my/org-agenda-skip-without-match (match)
    "Skip current headline unless it matches MATCH.
     Return nil if headline containing point matches MATCH (which
     should be a match string of the same format used by
     `org-tags-view').  If headline does not match, return the
     position of the next headline in current buffer.
     Intended for use with `org-agenda-skip-function', where this will
     skip exactly those headlines that do not match."
    (save-excursion
      (unless (org-at-heading-p) (org-back-to-heading))
      (let ((next-headline (save-excursion
                             (or (outline-next-heading) (point-max)))))
        (if (my/org-match-at-point-p match) nil next-headline))))

  (setq org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-compact-blocks t
        org-agenda-window-setup 'reorganize-frame
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo   . " ")
                                   (tags   . " %i %-12:c")
                                   (search . " %i %-12:c"))
        org-agenda-sorting-strategy '((agenda time-up priority-down todo-state-up tag-up alpha-up)
                                      (todo priority-down todo-state-up tag-up alpha-up)
                                      (tags priority-down todo-state-up tag-up alpha-up)))


  ;; Place tags at the right hand side of the window
  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))

  ;; org-agenda
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           (
            ;; (agenda ""
            ;;         ((org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'deadline))
            ;;          (org-deadline-warning-days 0)))
            (agenda nil)
            (todo "STRT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nStarted Tasks\n")))
            (todo "TODO"
                  (
                   (org-agenda-skip-function
                    '(my/org-agenda-skip-without-match "-inbox"))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTODO Tasks\n")))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          ("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                                   :file-path "refile\\.org")
                            (:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Important"
                                   :priority "A"
                                   :order 6)
                            (:name "Today's tasks"
                                   :file-path "journal/")
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Scheduled Soon"
                                   :scheduled future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:name "Meetings"
                                   :and (:todo "MEET" :scheduled future)
                                   :order 10)
                            (:discard (:not (:todo "TODO")))))))))))

  (org-super-agenda-mode))
