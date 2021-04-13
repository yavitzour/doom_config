;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yoav Avitzour"
      user-mail-address "yavitzour@gmail.com")

;; Move the cache dir out of .emacs.d
(setq doom-cache-dir "~/.cache/emacs")

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

;; Mixed Pitch
;; (use-package! mixed-pitch
;;   :defer
;;   :config
;;   (setq mixed-pitch-variable-pitch-cursor nil)
;;   :hook
;;   (text-mode . mixed-pitch-mode))
;; (setq mixed-pitch-set-height t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(if (not (display-graphic-p))
    (setq doom-theme 'doom-dark+)
  (setq doom-theme 'modus-operandi)
  )

;; dark themes:
;; modus-vivendi
;; doom-one
;; doom-dark+
;; doom-acario-dark

;; light themes
;; modus-operandi
;; leuven
;; doom-one-light
;; solo-jazz

(use-package! heaven-and-hell
  :demand t
  :init
  ;; (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . modus-operandi)
          (dark . modus-vivendi))
        )
  ;; Load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)

  (defvar my-themes '(doom-one-light doom-dark+ modus-operandi modus-vivendi leuven solo-jazz))
  ;; Changing list to circular list
  (nconc my-themes my-themes)
  (defvar my-current-theme 'default)

  (defun my/load-next-theme ()
    (interactive)
    (setq my-current-theme (pop my-themes))
    (message "Changing theme to: %s" my-current-theme)
    (heaven-and-hell-clean-load-themes my-current-theme)
    )

  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)
         ("<f7>" . my/load-next-theme)
         )
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
(use-package! modus-themes
  :init
  (setq modus-themes-org-blocks 'rainbow
        modus-themes-rainbow-headings t
        ;; modus-themes-section-headings t
        modus-themes-scale-headings t
        modus-themes-mode-line '3d
        modus-themes-completions 'opinionated
        ;; modus-themes-bold-constructs t
        ;; modus-themes-slanted-constructs t
        ;; modus-themes-intense-hl-line t
        )
  (modus-themes-load-themes)
  ;; :bind
  ;; (("<f5>" . modus-themes-toggle))
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

(load! "+misc-functions")

;; My bindings
(map!
 "M-;" #'comment-or-uncomment-region-or-line
 "C-x F" #'find-file-at-point
 "C-x 4 F" #'ffap-other-window
 "<f2>" #'next-error
 "S-<f2>" #'previous-error
 "C-w" #'obar/kill-region-or-backward-word
 "C-c C-b" #'copy-buffer-file-name-as-kill
 "M-c" #'ct/capitalize-word-at-point
 "M-u" #'ct/upcase-word-at-point
 "M-l" #'ct/downcase-word-at-point
 "C-z" #'zap-up-to-char
 )

;; Python
(load! "+python")

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
(after! lsp-mode
  ;; (setq lsp-eldoc-enable-hover nil
  ;;       lsp-signature-auto-activate nil
  ;;       ;; lsp-enable-on-type-formatting nil
  ;;       lsp-enable-file-watchers nil
  ;;       lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-mode 1)
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  )

;; add .bash_aliases to sh-mode auto mode
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))

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

;; Make scratchpad buffers inherit major mode of the current buffer
(setq doom-scratch-buffer-major-mode 1)

;; Latex configuration
(setq TeX-save-query nil
      TeX-error-overview-open-after-TeX-run t
      TeX-error-overview-setup 'separate-frame)

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

(use-package! peep-dired
  :after dired
  :bind (:map dired-mode-map
         ("C-SPC" . peep-dired)))

(use-package! find-file-rg
  :bind (("C-c C-f" . find-file-rg)
         ("C-c C-S-f" . find-file-rg-at-point)
         )
  )


(use-package! good-scroll
  :config
  (good-scroll-mode 1)
  )

;; org mode configuration
(setq org-directory "~/Dropbox/org/") ; let's put files here
(when (display-graphic-p) (load! "+org"))

;; ejc-sql
;; (use-package! ejc-sql
;;   :config
;;   (setq ejc-use-flx t)
;;   (setq ejc-flx-threshold 2)
;;   (require 'ejc-company)
;;   (push 'ejc-company-backend company-backends)
;;   (add-hook 'ejc-sql-minor-mode-hook
;;             (lambda ()
;;               (company-mode t)))
;;   (setq ejc-complete-on-dot t)
;;   (setq ejc-completion-system 'ivy)
;;   (add-hook 'ejc-sql-minor-mode-hook
;;           (lambda ()
;;             (ejc-eldoc-setup)))
;; )

;; show parentheses matches outside the visible window
(load! "+show-paren")

;; Useful Functions
;; "C-u M-x what-cursor-position" ("C-u C-x =") find out everything about the state under the cursor (face name, font, etc)
