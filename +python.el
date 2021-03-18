;;; +python.el -*- lexical-binding: t; -*-

;; Python configuration
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

(use-package! importmagic
  :config
  (add-hook! 'python-mode-hook 'importmagic-mode)
)
;; (map! :map importmagic-mode-map
;;       "C-c C-f" #'importmagic-fix-symbol-at-point)

(setq dap-python-debugger 'debugpy)

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
