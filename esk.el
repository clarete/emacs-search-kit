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



;;; Interactive functions


(defun esk-find-file (pattern)
  "Searches for a pattern"
  (interactive "sFind file in project: ")
  (esk-perform-find
   (esk-find-nearest-git-directory
    (esk-get-current-buffer-directory)) pattern))


(defun esk-find-in-project (pattern)
  "Searches within the project for `pattern'"
  (interactive "sPattern to find in project: ")
  (esk-perform-grep
   (esk-find-nearest-git-directory
    (esk-get-current-buffer-directory)) pattern))


;;; Generic functions for both grep and find


(defun esk-find-nearest-git-directory (dir)
  "Looks for the nearest directory containing a .git directory"
  (esk-find-top-dir ".git" dir))


(defun esk-find-top-dir (flag dir)
  "Looks for a directory that contains a directory called `flag' and stops at `/'"
  (if (or (equal dir "/") (file-exists-p (concat dir flag)))
      dir
      (esk-find-top-dir flag (expand-file-name (concat dir "../")))))


(defun esk-get-current-buffer-directory ()
  "Returns the directory of the current buffer"
  (if (file-directory-p buffer-file-name)
      buffer-file-name
     (concat
      (mapconcat 'identity (butlast (split-string buffer-file-name "/") 1) "/")
      "/")))



(defun esk-create-link-in-buffer (start fname end)
  (lexical-let ((fname fname) (map (make-sparse-keymap)))
    (defun esk-open-file (e)
      (interactive "p")
      (find-file fname))
    (define-key map (kbd "<RET>") #'esk-open-file)
    (define-key map (kbd "<down-mouse-1>") #'esk-open-file)
    (add-text-properties
     start end `(keymap, map mouse-face highlight))))


;;; Find related functions


(defun esk-perform-find (dir pattern)
  "Issues the find command to search matching the given `pattern'"
  (esk-show-find-results
   (esk-process-find-output
    dir
    (let ((param (or (and (string-match "\/" pattern) "-path") "-name")))
      (shell-command-to-string (concat "find " dir " " param " '*" pattern "*'"))))))


(defun esk-process-find-output (dir output)
  "Break the output in lines and filter them"
  (mapcar '(lambda (s) (substring s (length dir) (length s)))
          (remove "" (remove dir (split-string output "\n")))))


(defun esk-show-find-results (results)
  (with-output-to-temp-buffer "*esk*"
    (switch-to-buffer-other-window "*esk*")
    (setq font-lock-mode nil)
    (princ (format "Listing %d files found\n\n" (length results)))
    (mapcar 'esk-format-find-result-line results)))


(defun esk-format-find-result-line (line)
  (princ " * ")
  (esk-create-link-in-buffer
   (point) (princ line) (point))
  (princ "\n"))


;;; Grep functions


(defun esk-show-grep-results (results)
  (with-output-to-temp-buffer "*esk*"
    (switch-to-buffer-other-window "*esk*")
    (setq font-lock-mode nil)
    (princ (format "Listing %d results found\n\n" (length results)))
    (mapcar '(lambda (line)
               (insert
                (format
                 "%s:%s: %s"
                 (propertize (car line) 'face '(:foreground "green"))
                 (propertize (nth 1 line) 'face '(:foreground "yellow"))
                 (nth 2 line)))
               (princ "\n"))
            results)))


(defun esk-process-grep-output (output)
  (butlast
   (mapcar '(lambda (l) (split-string l ":")) (split-string output "\n"))))


(defun esk-perform-grep (dir pattern)
  (esk-show-grep-results
   (esk-process-grep-output
    (shell-command-to-string (concat "grep -I -nH -r -e '" pattern "' " dir)))))


(provide 'esk)

;;; esk.el ends here
