;;; common_config.el -*- lexical-binding: t; -*-

;;; Code:

;; Set some defaults
(setq-default tab-width 4
              tab-always-indent 'complete
              ;; uniquify-buffer-name-style 'forward
              window-combination-resize t)

(setq kill-whole-line t)

;; Backup
(setq auto-save-default t
      make-backup-files t)

;; add .bash_aliases to sh-mode auto mode
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))


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

;; (after! highlight-indent-guides
;;   (highlight-indent-guides-auto-set-faces))

(use-package! winnow
  :hook
  (grep-mode . winnow-mode)
  )

(load! "winnow-embark"  my-doom-common-dir)
(use-package! winnow-embark
  :hook
  (embark-collect-mode . winnow-embark-mode)
  )

(use-package! projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories "blade_env")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".ipynb"))
