;;; qjp-defuns-misc.el --- Misc defuns               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: convenience

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

;;; Code:

(defun qjp-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun qjp-delete-this-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))

;; eval and replace using calc
(defun qjp-calc-eval-and-replace (&optional prefix start end)
  (interactive "P\nr")
  (let ((result (calc-eval (buffer-substring-no-properties start end))))
    (when prefix
      (kill-region start end))
    (insert result)))

;; ------------------ ;;
;; functions from esk ;;
;; ------------------ ;;
(defun qjp-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun qjp-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun qjp-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (qjp-indent-buffer)
  (qjp-untabify-buffer)
  (delete-trailing-whitespace))

(defun qjp-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun qjp-sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (helm-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun qjp-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun qjp-suck-it (suckee)
  "Insert a comment of appropriate length about what can suck it."
  (interactive "MWhat can suck it? ")
  (let ((prefix (concat ";; " suckee " can s"))
        (postfix "ck it!")
        (col (current-column)))
    (insert prefix)
    (dotimes (_ (- 80 col (length prefix) (length postfix))) (insert "u"))
    (insert postfix)))

(defun qjp-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun qjp-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

;; ---------------------- ;;
;; Functions from prelude ;;
;; ---------------------- ;;
(defun qjp-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt.

Modified the original function. Always put the word at prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (read-string
             prompt
             (if (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))
               (thing-at-point 'word t)))))))

(defmacro qjp-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "qjp-search-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (qjp-search ,search-engine-url ,search-engine-prompt)))

(qjp-install-search-engine "google" "http://www.google.com/search?q=" "Google: ")
(qjp-install-search-engine "bing-dict" "http://www.bing.com/dict/search?q=" "Bing Dict: ")

;; Change themes
(defun qjp-switch-theme ()
  (interactive)
  (let ((word
         (completing-read
          "Choose a theme set(dark or light):"
          '(dark light))))
    (cond
     ((string= word "dark")
      (require 'zenburn-theme)
      (require 'smart-mode-line)
      (load-theme 'zenburn t)
      (sml/apply-theme 'powerline))
     ((string= word "light")
      (disable-theme 'zenburn)
      (sml/apply-theme 'light)))))

(provide 'qjp-defuns-misc)
;;; qjp-defuns-misc.el ends here
