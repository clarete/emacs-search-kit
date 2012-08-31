;;; esk.el --- Emacs search kit

;; Copyright (C) 2012  Lincoln de Sousa and Suneel Chakravorty

;; Author: Suneel Chakravorty <suneel0101@gmail.com>
;; Author: Lincoln de Sousa <lincoln@comum.org>
;; Keywords: search, find
;; Version: 0.1

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
;;
;; This is a very simple extention to search for files and for contents
;; inside files.
;; 
;; Enjoy!

;;; Code:




(defun esk-find-file (pattern)
  "Searches for a pattern"
  (interactive "sPattern to search: ")
  (esk-perform-search
   (esk-find-nearest-git-directory
    (esk-get-current-buffer-directory)) pattern))


(defun esk-find-top-dir (flag dir)
  "Looks for a directory that contains a directory called `flag' and stops at `/'"
  (if (or (equal dir "/") (file-exists-p (concat dir flag)))
      dir
      (esk-find-top-dir flag (expand-file-name (concat dir "../")))))


(defun esk-find-nearest-git-directory (dir)
  "Looks for the nearest directory containing a .git directory"
  (esk-find-top-dir ".git" dir))


(defun esk-get-current-buffer-directory ()
  "Returns the directory of the current buffer"
  (if (file-directory-p buffer-file-name)
      buffer-file-name
     (concat
      (mapconcat 'identity (butlast (split-string buffer-file-name "/") 1) "/")
      "/")))

(defun esk-create-link-in-buffer (start fname end)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] '(lambda () (interactove) (message-box "blah")))
    (add-text-properties start end '(keymap map mouse-face highlight))))


(defun esk-show-results (results)
  (with-output-to-temp-buffer "*esk*"
    (switch-to-buffer-other-window "*esk*")
    (princ (format "Listing %d files found\n\n" (length results)))
    (mapcar '(lambda (line)
               (princ " * ")
               (esk-create-link-in-buffer (point) (princ line) (point))
               (princ "\n"))
            results)))


(defun esk-process-find-output (output)
  (let ((all-lines (mapcar '(lambda (l) (split-string l "//")) (split-string output "\n"))))
    (let ((lines (delq nil (mapcar '(lambda (l) (car (nthcdr 1 l))) all-lines))))
      lines)))


(defun esk-perform-search (dir pattern)
  "Issues the find command to search matching the given `pattern'"
  (esk-show-results
   (esk-process-find-output
    (let ((param (or (and (string-match "\/" pattern) "-path") "-name")))
      (shell-command-to-string (concat "find " dir " " param " '*" pattern "*'"))))))

(provide 'esk)

;;; esk.el ends here
