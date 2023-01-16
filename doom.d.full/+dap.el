;;; ../projects/doom_config/.doom.d.full/+dap.el -*- lexical-binding: t; -*-

(after! dap-mode
  (dap-register-debug-template "parplot"
                               (list :type "python"
                                     :args nil
                                     :cwd nil
                                     :env '(("DEBUG" . "1"))
                                     :target-module (expand-file-name "~/projects/parplot/parplot.py")
                                     :request "lanuch"
                                     :name "parplot"
                                     ))
  )
