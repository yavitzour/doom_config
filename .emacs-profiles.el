;;; package --- Summary

;;; Commentary:
;; Emacs profiles configuration

;;; Code:
(("doom-full" . ((user-emacs-directory . "~/.emacs.d.doom-full")
                 (env . (("DOOMDIR" . "~/.doom.d.full")))))
 ("doom-thin" . ((user-emacs-directory . "~/.emacs.d.doom-thin")
                 (env . (("DOOMDIR" . "~/.doom.d.thin")))))
 ("doom-default" . ((user-emacs-directory . "~/.emacs.d.doom-default")
                    (env . (("DOOMDIR" . "~/.doom.d.default")))))
 ("crafted" . ((user-emacs-directory . "~/crafted-emacs")
               (env . (("CRAFTED_EMACS_HOME" . "~/crafted-emacs-config")))))
 ("lambda" . ((user-emacs-directory . "~/lambda-emacs")
              ))
 )

;;; .emacs-profiles.el ends here
