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
(setq doom-font (font-spec :family "OfficeCodePro" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "OfficeCodePro" :size 20))
;; (setq doom-font (font-spec :family "Consolas" :size 13))
;; (setq-default line-spacing 2)

 ;; There are two ways to load a theme. Both assume the theme is installed and
 ;; available. You can either set `doom-theme' or manually load a theme with the
 ;; `load-theme' function. This is the default:

(if (not (display-graphic-p))
    (setq doom-theme 'doom-dark+)
  (setq doom-theme 'doom-one-light))

;; (when (display-graphic-p) (setq doom-theme 'doom-one-light))

;; (use-package! mixed-pitch
;;   :defer
;;   :config
;;   (setq mixed-pitch-variable-pitch-cursor nil)
;;   :hook
;;   (text-mode . mixed-pitch-mode))
(setq mixed-pitch-set-height t)

;; (setq doom-theme 'leuven)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'solo-jazz)

;; dark themes:
;; doom-one
;; doom-dark+
;; doom-acario-dark
;; modus-vivendi

(use-package! heaven-and-hell
  :init
  ;; (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
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
              ;; uniquify-buffer-name-style 'forward
              window-combination-resize t)

(setq kill-whole-line t)

;; Backup
(setq auto-save-default t
      make-backup-files t)

;; modus themes configuration
(use-package! modus-operandi-theme
  :defer t
  :init
  (setq modus-operandi-theme-org-blocks 'rainbow
        modus-operandi-theme-rainbow-headings t
        ;; modus-operandi-theme-section-headings t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-mode-line '3d
        )
  )

(use-package! modus-vivendi-theme
  :defer t
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
  (setq doom-modeline-height 10
        doom-modeline-major-mode-icon t)

  :custom-face
  (mode-line ((t (:height 0.97))))
  (mode-line-inactive ((t (:height 0.97)))))


 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


 ;; Here are some additional functions/macros that could help you configure Doom:
 ;;
 ;; - `load!' for loading external *.el files relative to this one
 ;; - `use-package!' for configuring packages
 ;; - `after!' for running code after a package has loaded
 ;; - `add-load-path!' for adding directories to the `load-path', relative to
 ;;   this file. Emacs searches the `load-path' when you load packages with
 ;;   `require' or `use-package'.
 ;; - `map!' for binding new keys
 ;;
 ;; To get information about any of these functions/macros, move the cursor over
 ;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
 ;; This will open documentation for it, including demos of how they are used.
 ;;
 ;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
 ;; they are implemented.

(use-package! smartparens
  :defer t
  :config
  (smartparens-global-strict-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  )
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
  (add-hook! org-shiftup-final #'windmove-up)
  (add-hook! org-shiftdown-final #'windmove-down)
  (add-hook! org-shiftleft-final #'windmove-left)
  (add-hook! org-shiftright-final #'windmove-right)
  )

;;
;; Copy the buffer file name into the kill ring
;;
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

(global-set-key (kbd "C-c C-f") 'copy-buffer-file-name-as-kill)

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

(after! realgud-pdb
  (setq realgud:pdb-command-name "python3 -m pdb"))

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
  "Starts python shell buffer if one is not running and jumps to it"
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

(use-package! pyvenv
  :config
  (pyvenv-mode 1)

  ;; ;; Set correct Python interpreter
  ;; (setq pyvenv-post-activate-hooks
  ;;       (list (lambda ()
  ;;               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  ;; (setq pyvenv-post-deactivate-hooks
  ;;       (list (lambda ()
  ;;               (setq python-shell-interpreter "python"))))
  )

(defun pyvenv-autoload ()
  (interactive)
  "auto activate venv/env directory if exists"
  (setq venv-patterns '("venv" "env"))
  (dolist (venv venv-patterns)
    (f-traverse-upwards (lambda (path)
                          (let ((venv-path (f-expand venv path)))
                            (when (f-exists? venv-path)
                              (progn (message venv-path)
                                     (pyvenv-activate venv-path))
                              ))))
    )
  )


(add-hook 'python-mode-hook 'pyvenv-autoload)

(setq dap-python-debugger 'debugpy)

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
  :defer t
  :config
  (add-hook! sql-mode #'sqlind-minor-mode))
(after! sql (load-library "sql-indent"))

(use-package! yafolding
  :hook
  (prog-mode . yafolding-mode)
  )

(use-package! goto-chg
  :config
  (map!
   "C-x -" #'goto-last-change
   "C-x _" #'goto-last-change-reverse)
  )

(setq highlight-indent-guides-suppress-auto-error t)
(after! highlight-indent-guides
  (highlight-indent-guides-auto-set-faces))

(use-package! winnow
  :hook
  (grep-mode . winnow-mode)
  )

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

;; Cache gpg file password so you only need to set it once and not every time you save the file
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Useful Functions
;; "C-u M-x what-cursor-position" ("C-u C-x =") find out everything about the state under the cursor (face name, font, etc)


;; Latex configuration
(setq TeX-save-query nil)
;; Enable yafolding for tex-mode
(add-hook! cdlatex-mode #'yafolding-mode)

(use-package! easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(after! ivy
  (map!
   :map ivy-minibuffer-map
   ;; map TAB to ivy-partial-or-done. Two tabs restores the ivy-alt-done functionality
  "TAB" #'ivy-partial-or-done))

(use-package! peep-dired)

;; org mode configuration
(load! "+org")

;; show parentheses matches outside the visible window
(load! "+show-paren")
