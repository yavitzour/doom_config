;;; +org.el -*- lexical-binding: t; -*-

;; org configuration - heavily influenced by https://www.labri.fr/perso/nrougier/GTD/index.html

(use-package! org-appear
  :init
  (add-hook 'org-mode-hook 'org-appear-mode)
  )

(after! org
  (setq org-use-property-inheritance t              ; it's convenient to have properties inherited
        org-log-done 'time                          ; having the time an item is done sounds convininet
        org-log-refile 'time                        ; having the time an item is refiled sounds convininet
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (julia . t)
     (python . t)
     (jupyter . t)))

  (setq org-babel-default-header-args:jupyter-python '(
                                                       (:session . "py")
                                                       (:kernel . "python3")))
  (after! org-src
    (dolist (lang '(python typescript jupyter))
      (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                  org-src-lang-modes :key #'car)))

  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file "inbox.org")
                 "* TODO %?\n/Entered on/ %U"))
  (add-to-list 'org-capture-templates
               '("n" "Note" entry (file "notes.org")
                 "* Note (%a)\n/Entered on/ %U\n" "\n" "%?"))
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry (file+datetree "journal.org")
                 "* %?\n %i\n %a"))

  (setq org-refile-targets
        '(
          ;; ("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
          ;; ("projects.org" :maxlevel . 1)
          ("projects.org" :maxlevel . 2)
          )
        )

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (defun log-todo-next-creation-date (&rest ignore)
    "Log STRT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "STRT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  ;; have list bullets change with depth
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

  ;; org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  ;; Use the following on buffers that use org-crypt:
  ;; # -*- buffer-auto-save-file-name: nil; -*-
  )

(use-package! org-journal
  :config
  (setq org-journal-file-type 'yearly))
