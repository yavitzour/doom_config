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
(setq doom-font (font-spec :family "SourceCodePro"))
;; (setq doom-font (font-spec :family "Consolas" :size 14))
;; (setq doom-font (font-spec :family "Hack" :size 14))
;; (setq doom-font (font-spec :family "DejaVuSansMono" :size 13))
;; (setq doom-font (font-spec :family "Hack")
;;       doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
;;       doom-big-font (font-spec :family "OfficeCodePro" :size 20))
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

(defvar my-doom-common-dir "~/doom_config/doom.d.common")

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

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; ask to choose buffer after splitting window
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(split-window)
;;   (consult-buffer))


;; modus themes configuration
(use-package! modus-themes
  :init
  (setq modus-themes-org-blocks 'rainbow
       modus-themes-rainbow-headings t
       ;; modus-themes-section-headings t
       modus-themes-scale-headings t
       modus-themes-mode-line '3d
       ;; modus-themes-completions 'opinionated
       modus-themes-bold-constructs t
       ;; modus-themes-slanted-constructs t
       ;; modus-themes-intense-hl-line t
       modus-themes-syntax 'alt-syntax
       )
  ;;(modus-themes-load-themes)            
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

;; (use-package! tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(global-tree-sitter-mode t)


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
  (lsp-headerline-breadcrumb-mode t)

  ;; UI settings
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  ;; (setq lsp-ui-doc-include-signature t)
  ;; (setq lsp-ui-doc-border (face-foreground 'default))
  ;; (setq lsp-ui-sideline-show-code-actions t)
  ;; (setq lsp-ui-sideline-delay 0.05)

  ;; Other settings
  ;; (setq lsp-auto-guess-root t)
  ;; (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-modeline-code-actions-enable nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable t)
  ;; (setq lsp-semantic-tokens-enable nil)
  ;; (setq lsp-enable-folding nil)
  ;; (setq lsp-enable-imenu nil)
  ;; (setq lsp-enable-snippet nil)
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1MB
  ;; (setq lsp-idle-delay 0.5)
  )


(load! "common_config" my-doom-common-dir)

;; (use-package! goto-chg
;;   :config
;;   (map!
;;    "C-x -" #'goto-last-change
;;    "C-x _" #'goto-last-change-reverse)
;;   )


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
(add-hook! org-mode :append
           ;; #'visual-line-mode
           ;; #'solaire-mode
           ;; #'typopunct-mode
           #'writegood-mode
           ;; #'variable-pitch-mode
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

;; Denote
(load! "+denote")


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

(after! dired
  (setq dired-listing-switches "-ahlGg -v --group-directories-first")
  (setq dired-kill-when-opening-new-dired-buffer t)
  )

(use-package! peep-dired
  :after dired
  :bind (:map dired-mode-map
         ("C-SPC" . peep-dired)))

(use-package! dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("TAB" . dired-subtree-toggle)))


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

(setq-default c-default-style "linux"
              c-basic-offset 4)

(setq-hook! '(c-mode-hook c++-mode-hook) tab-width 4)
(setq-hook! '(c-mode-hook c++-mode-hook) c-basic-offset 4)

;; Compilation tweaks
(setq compilation-environment '("TERM=tmux-256color"))
(setq compilation-max-output-line-length nil)
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (ansi-color-apply string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)
(advice-add 'compilation-filter :after #'my/advice-compilation-filter)

(add-hook 'compilation-mode-hook
  (lambda ()
    (setq-local compilation-scroll-output t)
    (setq-local scroll-conservatively most-positive-fixnum)
    (setq-local scroll-margin 0)))


(use-package! sqlformat
  ;; :ensure-system-package (pg_format . "sudo apt install pgformatter")
  :defer t
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g"))
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))
(after! sql-indent (load-library "sqlformat"))

;; Useful Functions
;; "C-u M-x what-cursor-position" ("C-u C-x =") find out everything about the state under the cursor (face name, font, etc)


(setq-default bidi-display-reordering nil)
(defun bidi-reordering-toggle ()
  "Toggle bidirectional display reordering"
  (interactive)
  (setq bidi-display-reordering (not bidi-display-reordering))
  (message "bidi-display-reordering is %s" bidi-display-reordering)
  )

(setq-default bidi-paragraph-direction 'left-to-right)
(defun bidi-direction-toggle ()
  "Switch the explitict direction of text for the current buffer"
  (interactive)
  (setq bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (setq bidi-paragraph-direction 'left-to-right)
    (setq  bidi-paragraph-direction 'right-to-left))
  (message "%s" bidi-paragraph-direction)
  )

(use-package! matlab-mode
  :defer t
  :init
  (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
  :config
  (setq matlab-indent-function-body nil)
  )
