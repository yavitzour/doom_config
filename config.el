;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yoav Avitzour"
      user-mail-address "yavitzour@gmail.com")

;; Move the cache dir out of .emacs.d
;; (setq doom-cache-dir "~/.cache/emacs")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
;; (setq doom-font (font-spec :family "SourceCodePro" :size 14))
(setq doom-font (font-spec :family "OfficeCodePro" :size 15))
;; (setq doom-font (font-spec :family "Consolas" :size 13))
;; (setq-default line-spacing 2)

 ;; There are two ways to load a theme. Both assume the theme is installed and
 ;; available. You can either set `doom-theme' or manually load a theme with the
 ;; `load-theme' function. This is the default:
;; (setq doom-theme 'leuven)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'solo-jazz)
(when (display-graphic-p) (setq doom-theme 'doom-one-light))

;; dark themes:
;; doom-one
;; doom-dark+
;; doom-acario-dark
;; modus-vivendi

(use-package! heaven-and-hell
  :init
  ;; (setq heaven-and-hell-theme-type 'light) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . doom-one-light)
          (dark . modus-vivendi))
        ;; '((light . modus-operandi)
        ;;   (dark . doom-light))
        ;; '((light . modus-one-light)
        ;;   (dark . modus-vivendi))
        ) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)

  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme))
  )

(setq-default tab-width 4
              tab-always-indent 'complete
              uniquify-buffer-name-style 'forward
              window-combination-resize t)


;; modus themes configuration
(use-package! modus-operandi-theme
  :init
  (setq modus-operandi-theme-org-blocks 'rainbow
        modus-operandi-theme-rainbow-headings t
        ;; modus-operandi-theme-section-headings t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-mode-line '3d
        )
  )
(use-package! modus-vivendi-theme
  :init
  (setq modus-vivendi-theme-org-blocks 'rainbow
        modus-vivendi-theme-rainbow-headings t
        ;; modus-vivendi-theme-section-headings t
        modus-vivendi-theme-scale-headings t
        modus-vivendi-theme-mode-line '3d
        )
  )

(use-package! doom-modeline
  :init
  (setq doom-modeline-height 10)
  :custom-face
  (mode-line ((t (:height 0.97))))
  (mode-line-inactive ((t (:height 0.97)))))


 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


 ;; Here are some additional functions/macros that could help you configure Doom:
 ;;
 ;; - `load!' for loading external *.el files relative to this one
 ;; - `use-package' for configuring packages
 ;; - `after!' for running code after a package has loaded
 ;; - `add-load-path!' for adding directories to the `load-path', relative to
 ;;   this file. Emacs searches the `load-path' when you load packages with
 ;;   `require' or `use-package'.
 ;; - `map!' for binding new keys
 ;;
 ;; To get information about any of these functions/macros, move the cursor over
 ;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
 ;; This will open documentation for it, including demos of how they are used.
 ;;
 ;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
 ;; they are implemented.

(smartparens-global-strict-mode t)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

(after! smartparens
  (map! :map smartparens-mode-map
        "C-<right>" nil
        "M-<right>" nil
        "C-<left>"  nil
        "M-<left>"  nil
        "C-)"       'sp-forward-slurp-sexp
        "M-)"       'sp-forward-barf-sexp
        "C-("       'sp-backward-slurp-sexp
        "M-("       'sp-backward-barf-sexp
        "C-c u"     'sp-unwrap-sexp
        "C-c r"     'sp-rewrap-sexp))

(use-package! windmove
  :config
  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings)
  )

;;
;; Copy the buffer file name into the kill ring
;;
(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
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

(global-set-key (kbd "C-c C-f") 'copy-buffer-file-name-as-kill)

;; org
(setq org-directory "~/org/"                      ; let's put files here
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convininet
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
      org-hide-emphasis-markers t)

(after! org
  (setq org-tags-column -77))
;; org-agenda
(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-compact-blocks t)

(setq org-agenda-sorting-strategy
      (quote
       ((agenda priority-down alpha-up)
        (todo priority-down alpha-up)
        (tags priority-down alpha-up))))


;; agenda display format
(setq org-agenda-prefix-format
      (quote
       ((agenda . "%s %?-12t %e ")
        (timeline . "  %s")
        (todo . " %i %e ")
        (tags . " %i %e ")
        (search . " %i %e "))))

;; Place tags at the right hand side of the window
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(setq org-agenda-window-setup 'reorganize-frame)

;; have list bullets change with depth
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))


;; org-crypt
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)
;; Use the following on buffers that use org-crypt:
;; # -*- buffer-auto-save-file-name: nil; -*-


;; comments
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

;; My bindings
(map!
 "M-;" #'comment-or-uncomment-region-or-line
 "C-x F" #'find-file-at-point
 "C-x 4 F" #'ffap-other-window
 "<f2>" #'next-error
 "S-<f2>" #'previous-error
 "C-w" #'obar/kill-region-or-backward-word)

;; Python
(setq python-shell-interpreter-args "-m IPython --simple-prompt -i")

(defun python-shell-send-current-statement ()
  "Send current statement to Python shell.
Taken from elpy-shell-send-current-statement"
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
    (python-shell-send-string (buffer-substring beg end)))
  (python-nav-forward-statement))

(defun python-shell-send-region-or-line nil
  "Sends from 'python-mode' buffer to a python shell, intelligently."
  (interactive)
  (cond ((region-active-p)
         (setq deactivate-mark t)
         (python-shell-send-region (region-beginning) (region-end))
         ) (t (python-shell-send-current-statement))))

(defun my-run-python ()
  (interactive)
  (run-python)
  (pop-to-buffer "*Python*"))

(map! :map python-mode-map
      "C-c C-h" #'python-eldoc-at-point
      "C-c C-f" #'python-shell-send-defun
      [remap python-shell-send-region] #'python-shell-send-region-or-line
      "C-c C-s" #'my-run-python)

;;; Add matlab-like behavior to comint based modes (shell, python-shell)
(map! :map comint-mode-map
      "M-<up>" #'comint-previous-matching-input-from-input
      "M-<down>" #'comint-next-matching-input-from-input)

;; (use-package! pyvenv
;;   :config
;;   (pyvenv-mode t)

;;   ;; Set correct Python interpreter
;;   (setq pyvenv-post-activate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
;;   (setq pyvenv-post-deactivate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter "python")))))


;; (map! :map prog-mode-map
;;       "<C-return>" #'+fold/toggle)

;; (use-package! tree-sitter :after python-mode)

;; (after! tree-sitter
;;   (require 'tree-sitter)
;;   (require 'tree-sitter-langs)
;;   (require 'tree-sitter-hl)
;;   (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

;; pyright - requires new nodejs - next ubuntu?
;; (use-package! lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

;; lsp configs
;; (after! lsp-mode
;;   (setq lsp-eldoc-enable-hover nil
;;         lsp-signature-auto-activate nil
;;         ;; lsp-enable-on-type-formatting nil
;;         lsp-enable-symbol-highlighting nil))
;;         ;; lsp-enable-file-watchers nil))

;; virtualenv
;; (defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
;;   "Use the Python binary from the current virtual environment."
;;   :around #'+python/open-repl
;;   (if (getenv "VIRTUAL_ENV")
;;       (let ((python-shell-interpreter (executable-find "ipython")))
;;         (apply orig-fn args))
;;     (apply orig-fn args)))


;; not sure what this does:
;; (after! python
;;   (setq python-shell-completion-native-enable nil))

;; or this:
;; (set-popup-rule! "^\\*Python*" :ignore t)


(use-package! iedit
  :bind (("C-;" . iedit-mode))
  :config
  (setq iedit-toggle-key-default nil))

(use-package! highlight-symbol
  :config
  (map!
   "C-<f3>" #'highlight-symbol
   "<f3>" #'highlight-symbol-next
   "S-<f3>" #'highlight-symbol-prev
   "M-<f3>" #'highlight-symbol-query-replace))

(use-package! sql-indent
  :config
  (eval-after-load "sql"
    '(load-library "sql-indent"))
  (add-hook! sql-mode #'sqlind-minor-mode))

(use-package! yafolding
  :config
  (add-hook! prog-mode #'yafolding-mode)
  )

(use-package! goto-chg
  :config
  (map!
   "C-x -" #'goto-last-change
   "C-x _" #'goto-last-change-reverse)
  )

(after! highlight-indent-guides
  (highlight-indent-guides-auto-set-faces))

(use-package! winnow
  :hook
  (grep-mode . winnow-mode)
  )

(use-package! ripgrep)

(use-package! projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories "blade_env")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".ipynb"))

;; Treemacs config
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(setq treemacs-file-ignore-extensions '(;; LaTeX
                                        "aux"
                                        "ptc"
                                        "fdb_latexmk"
                                        "fls"
                                        "synctex.gz"
                                        "toc"
                                        ;; LaTeX - glossary
                                        "glg"
                                        "glo"
                                        "gls"
                                        "glsdefs"
                                        "ist"
                                        "acn"
                                        "acr"
                                        "alg"
                                        ;; LaTeX - pgfplots
                                        "mw"
                                        ;; LaTeX - pdfx
                                        "pdfa.xmpi"
                                        ;; Python
                                        "pyc"
                                        "ipynb"
                                        ))
(setq treemacs-file-ignore-globs '(;; LaTeX
                                   "*/_minted-*"
                                   ;; AucTeX
                                   "*/.auctex-auto"
                                   "*/_region_.log"
                                   "*/_region_.tex"
                                   ;; Python
                                   "*/__pycache__/*"))


;; Maximize by default
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; autorevert
(setq global-auto-revert-mode t
      global-auto-revert-non-file-buffers t)

;; exec path from shell
(use-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; info colors
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; vterm
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

(map! "<f5>" #'+vterm/toggle
      :map vterm-mode-map
      "<f5>" #'+vterm/toggle)
