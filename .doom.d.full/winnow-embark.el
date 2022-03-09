;;; winnow-embark.el --- winnow-embark ag/grep results by matching/excluding lines -*- lexical-binding: t; -*-

;; Adapted from winnow.el - by Charles L.G. Comstock; URL: https://github.com/dgtized/winnow-embark.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Winnow `embark-collect' results by matching or excluding lines from the
;; results. Normally these buffers are `read-only-mode', preventing the use of
;; editing commands, but `winnow-embark-mode' commands inhibits this to apply
;; `flush-line' or `keep-lines' on the command output.
;;
;; As the edits are to the buffer, `recompile' is the recommended way to
;; regenerate the search.
;;
;; There is nothing specific to `embark-collect-mode' in the code, the main
;; adaptation from the regular winnow.el package was to remove the
;; `compilation-mode' specific commands. My intention was to use it in
;; `embark-collect', but maybe it can be useful elsewhere.

;;; Usage:

;; Enable the package in `embark-collect-mode' with the following:
;;
;;  (add-hook 'embark-collect-mode-hook 'winnow-embark-mode)

;;; Code:

(require 'compile)

(defun winnow-embark-results-start ()
  "Find the start position of the compilation output."
  (save-excursion
    (goto-char (point-min))))

(defun winnow-embark-results-end ()
  "Find the end position of the compilation output."
  (save-excursion
    (goto-char (point-max))))

(defun winnow-embark-exclude-lines (regexp &optional rstart rend interactive)
  "Exclude the REGEXP matching lines from the compilation results.

Ignores read-only-buffer to exclude lines from a result.

See `flush-lines' for additional details about arguments REGEXP,
RSTART, REND, INTERACTIVE."
  (interactive (keep-lines-read-args "Flush lines containing match for regexp"))
  (let ((inhibit-read-only t)
        (start (or rstart (winnow-embark-results-start)))
        (end (or rend (winnow-embark-results-end))))
    (flush-lines regexp start end interactive)
    (goto-char (point-min))))

(defun winnow-embark-match-lines (regexp &optional rstart rend interactive)
  "Limit the compilation results to the lines matching REGEXP.

Ignores read-only-buffer to focus on matching lines from a
result.

See `keep-lines' for additional details about arguments REGEXP,
RSTART, REND, INTERACTIVE."
  (interactive (keep-lines-read-args "Keep lines containing match for regexp"))
  (let ((inhibit-read-only t)
        (start (or rstart (winnow-embark-results-start)))
        (end (or rend (winnow-embark-results-end))))
    (keep-lines regexp start end interactive)
    (goto-char (point-min))))

;;;###autoload
(define-minor-mode winnow-embark-mode
  "Filter compilation results by matching/excluding lines.

This is invaluable for excluding or limiting to matching `ag-mode' results.

\\{winnow-embark-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "x") 'winnow-embark-exclude-lines)
            (define-key map (kbd "m") 'winnow-embark-match-lines)
            map))

(provide 'winnow-embark)
;;; winnow-embark.el ends here
