;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! iedit)

(package! highlight-symbol)

(package! sql-indent)

(package! yafolding)

(package! goto-chg)

(package! winnow)

(package! heaven-and-hell)

(package! ripgrep)

(package! modus-themes
  :recipe (:host gitlab
           :repo "protesilaos/modus-themes"
           )
  :pin "e02480f0b0a56b8575351db6504bf0d0417719ad"
)

(package! exec-path-from-shell)

(package! info-colors)

(package! vlf)

(package! systemd)

(package! solo-jazz-theme
  :recipe (:host github
           :repo "cstby/solo-jazz-emacs-theme"))

(package! org-super-agenda)

(package! mixed-pitch)

(package! easy-kill)

(package! peep-dired)

(package! sunrise-commander :recipe (:host github :repo "sunrise-commander/sunrise-commander"))

(package! org-jira)

(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

(package! find-file-rg
  :recipe (:host github
           :repo "muffinmad/emacs-find-file-rg"))

(package! importmagic)

;; (package! ejc-sql)

(package! good-scroll)

(package! emacs-jupyter
  :recipe (:host github
           :repo "nnicandro/emacs-jupyter")
  :pin nil)

(package! math-delimiters
    :recipe (:host github
             :repo "oantolin/math-delimiters")
    )

(package! dogears
  :recipe (
           :host github
           :repo "alphapapa/dogears.el")
  )

(package! popper)

(package! dired-subtree)

(package! python-mls)

(package! zk
  :recipe (
           :host github
           :repo "localauthor/zk"
           )
  )

(package! svg-tag-mode)

(package! howm)

(package! dirvish)

(package! transpose-frame)

(package! pet)
