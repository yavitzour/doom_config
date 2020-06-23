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
(setq doom-font (font-spec :family "OfficeCodePro" :size 15))
;; (setq doom-font (font-spec :family "Consolas" :size 13))
(setq-default line-spacing 2)

 ;; There are two ways to load a theme. Both assume the theme is installed and
 ;; available. You can either set `doom-theme' or manually load a theme with the
 ;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)
;; (setq doom-theme 'leuven)

;; (use-package! doom-modeline
;;   :config
;;   (setq doom-modeline-height 10)
;;   :custom-face
;;   (mode-line ((t (:height 1.0))))
;;   )

(defun my-doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (+ (frame-char-height) 2))
(advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

 ;; If you use `org' and don't want your org files in the default location below,
 ;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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
  (windmove-default-keybindings))

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
(setq-default org-hide-emphasis-markers t)

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

;; My bindings
(map!
 "M-;" #'comment-or-uncomment-region-or-line
 "C-x F" #'find-file-at-point
 "C-x 4 F" #'ffap-other-window
 "<f2>" #'next-error
 "S-<f2>" #'previous-error)

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

;; (map! :map prog-mode-map
;;       "<C-return>" #'+fold/toggle)

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

(use-package! goto-last-change
  :bind
  (("C-x -" . goto-last-change)))
