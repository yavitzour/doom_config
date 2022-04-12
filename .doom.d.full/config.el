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
;; (setq doom-font (font-spec :family "Consolas" :size 14))
;; (setq doom-font (font-spec :family "Hack" :size 14))
;; (setq doom-font (font-spec :family "DejaVuSansMono" :size 14))
(setq doom-font (font-spec :family "OfficeCodePro" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "OfficeCodePro" :size 20))
;; (setq doom-font (font-spec :family "Consolas" :size 13))
;; (setq-default line-spacing 1)

;; Mixed Pitch
;; (use-package! mixed-pitch
;;   :defer
;;   :config
;;   (setq mixed-pitch-variable-pitch-cursor nil)
;;   :hook
;;   (text-mode . mixed-pitch-mode))
;; (setq mixed-pitch-set-height t)

(defvar my-doom-common-dir "~/.doom.d.common")

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

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; ask to choose buffer after splitting window
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(split-window)
;;   (consult-buffer))


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
       modus-themes-bold-constructs t
       ;; modus-themes-slanted-constructs t
       ;; modus-themes-intense-hl-line t
       modus-themes-syntax 'alt-syntax
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

(use-package! vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(load! "+misc-functions" my-doom-common-dir)

(load! "+key-bindings" my-doom-common-dir)

;; Python
(load! "+python")

;; (map! :map prog-mode-map
;;       "<C-return>" #'+fold/toggle)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


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
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode 1)
  (setq lsp-python-ms-extra-paths '["."])

  ;; UI settings
  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-header t)
  ;; (setq lsp-ui-doc-include-signature t)
  ;; (setq lsp-ui-doc-border (face-foreground 'default))
  ;; (setq lsp-ui-sideline-show-code-actions t)
  ;; (setq lsp-ui-sideline-delay 0.05)

  ;; Other settings
  ;; (setq lsp-auto-guess-root t)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-restart 'auto-restart)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-modeline-code-actions-enable nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-semantic-tokens-enable nil)
  ;; (setq lsp-enable-folding nil)
  ;; (setq lsp-enable-imenu nil)
  ;; (setq lsp-enable-snippet nil)
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1MB
  ;; (setq lsp-idle-delay 0.5)
  )

;; add .bash_aliases to sh-mode auto mode
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))

(use-package! iedit
  :bind (("C-;" . iedit-mode))
  )

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

;; (use-package! goto-chg
;;   :config
;;   (map!
;;    "C-x -" #'goto-last-change
;;    "C-x _" #'goto-last-change-reverse)
;;   )

(after! highlight-indent-guides
  (highlight-indent-guides-auto-set-faces))

(use-package! winnow
  :hook
  (grep-mode . winnow-mode)
  )

(load! "winnow-embark")
(use-package! winnow-embark
  :hook
  (embark-collect-mode . winnow-embark-mode)
  )

(use-package! projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories "blade_env")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".ipynb"))

(load! "+treemacs")

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

(use-package! math-delimiters
  :config
  (setq math-delimiters-compressed-display-math nil)
  (setq math-delimiters-include-characters '(?. ?,))
  )

(after! org
  (map!
   :map org-mode-map
   "$" #'math-delimiters-insert)
  )

(after! tex
  (map!
   :map TeX-mode-map
   "$" #'math-delimiters-insert)
  )

(use-package! easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(after! ivy
  (map!
   :map ivy-minibuffer-map
   ;; map TAB to ivy-partial-or-done. Two tabs restores the ivy-alt-done functionality
   "TAB" #'ivy-partial-or-done
   "C-w" #'ivy-yank-word))

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

(use-package! dogears
  :init
  (dogears-mode t)
  (map!
   "C-x -" #'dogears-back
   "C-x _" #'dogears-forward
   "M-g d" #'dogears-go
   "M-g M-b" #'dogears-back
   "M-g M-f" #'dogears-forward
   "M-g M-d" #'dogears-list
   "M-g M-D" #'dogears-sidebar
   )
  )

(use-package! popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (setq popper-group-function #'popper-group-by-projectile)
  ;; (setq popper-display-function #'display-buffer-in-child-frame)
  (popper-mode +1)
  (popper-echo-mode +1))


;; org mode configuration
(setq org-directory "~/projects/org/") ; let's put files here
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

(use-package! dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("TAB" . dired-subtree-toggle)))

(use-package! zk
  :custom
  (zk-directory "~/zkdir")
  (zk-file-extension "md")
  :config
  ;; (require 'zk-consult)
  (zk-setup-auto-link-buttons)
  (zk-setup-embark)
  (setq zk-tag-grep-function #'zk-consult-grep-tag-search
        zk-grep-function #'zk-consult-grep)
  (add-hook 'completion-at-point-functions #'zk-completion-at-point 'append)
)

(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
            source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  )
(add-to-list 'flycheck-checkers 'vale 'append)

(use-package! svg-tag-mode
  :hook ((prog-mode . svg-tag-mode)
         (org-mode . svg-tag-mode))
  :config
  (setq svg-tag-tags
        '(
          ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
          ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
          ("MARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

          ("\\/\\/\\W?swiftlint:disable" . ((lambda (tag) (svg-tag-make "swiftlint:disable" :face 'org-level-3 :inverse t :margin 0 :crop-right t))))
          ("swiftlint:disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-3 :crop-left t))))

          ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("TODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
          )))
; //MARK: - Do something
; TODO fix me later
; //swiftlint:disable hello


;; CPP
(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h" ".hpp"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".cpp" ".c"))
    ))
(defvar my-cpp-search-directories-list
  '("." "../src" "../include")
  )
(setq-default ff-search-directories 'my-cpp-search-directories-list)
(setq-default ff-other-file-alist 'my-cpp-other-file-alist)
(map! :map c-mode-base-map
      "M-o" #'ff-get-other-file)


(use-package! howm
  :init
  ;; Directory configuration
  (setq howm-home-directory "~/howm/")
  (setq howm-directory "~/howm/")
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")

  ;; Use ripgrep as grep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name)

  ;; Fix howm setting C-h
  :config
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)
)

;; Useful Functions
;; "C-u M-x what-cursor-position" ("C-u C-x =") find out everything about the state under the cursor (face name, font, etc)
