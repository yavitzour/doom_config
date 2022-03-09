;;; +key-bindings.el -*- lexical-binding: t; -*-

(map!
 "M-;" #'comment-or-uncomment-region-or-line
 ;; "C-x F" #'find-file-at-point
 ;; "C-x 4 F" #'ffap-other-window
 "C-x F" #'projectile-find-file-dwim
 "C-x 4 F" #'projectile-find-file-dwim-other-window
 "<f2>" #'next-error
 "S-<f2>" #'previous-error
 "C-w" #'obar/kill-region-or-backward-word
 "C-c C-b" #'copy-buffer-file-name-as-kill
 "M-c" #'ct/capitalize-word-at-point
 "M-u" #'ct/upcase-word-at-point
 "M-l" #'ct/downcase-word-at-point
 "C-z" #'zap-up-to-char
 "C-l" #'recenter-correctly
 "C-x 4 k" #'kill-buffer-other-window
)

(fset     'my-cmds-prefix (make-sparse-keymap))
(defconst  my-cmds-map    (symbol-function 'my-cmds-prefix))

(let ((former-ctrl-r (key-binding "\C-r")))
  (and (not (equal 'my-cmds-prefix former-ctrl-r))
   (define-key my-cmds-map "\C-r"  former-ctrl-r)))

(map! "\C-r" #'my-cmds-prefix)

(map! :map my-cmds-map
      "."    #'set-mark-command
      "<"    #'shrink-window
      "="    #'goto-line
      ">"    #'enlarge-window
      "!"    #'command-history
      "\C-a" #'call-last-kbd-macro
      "\C-b" #'bury-buffer
      "\C-d" #'del-cmds-prefix
      "\C-f" #'finder
      "\C-i" #'ins-cmds-prefix
      "\C-k" #'kill-server-buffer
      "\C-m" #'mark-cmds-prefix
      "a"    #'repeat-complex-command
      "b"    #'boss-has-come
      "h"    #'help-for-help
      "i"    #'c-insert-function-headder
      "p"    #'pipe-region
      "t"    #'find-tag
      "v"    #'set-variable
      "x"    #'hexl-find-file
      )

(fset     'del-cmds-prefix (make-sparse-keymap))
(defconst  del-cmds-map    (symbol-function 'del-cmds-prefix))
(map! :map del-cmds-map
      "\C-a"     #'kill-all-of-buffer
      "\C-b"     #'kill-server-buffer
      "\C-c"     #'kill-comment
      "\C-k"     #'kill-line
      "\C-p"     #'kill-paragraph
      "\C-r"     #'kill-region
      "\C-s"     #'kill-sentance
      "\C-w"     #'kill-word
      "\C-x"     #'kill-rectangle
      )

(map! :map minibuffer-local-map
      "C-,"      #'embark-act
      "C-."      #'embark-collect-snapshot
      "C->"      #'embark-become)
