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

;; In prelude:
;; - `prelude-switch-to-previous-buffer'
;; - `prelude-kill-other-buffers'
;; - `prelude-wrap-with'
;; - `prelude-goto-symbol'

;;; Code:

;; ------------ ;;
;; Swap windows ;;
;; ------------ ;;
(defun qjp-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

;; -------------------- ;;
;; Find shell init file ;;
;; -------------------- ;;
(defun qjp-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string= "zsh" shell) ".zshrc")
                           ((string= "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file
     (expand-file-name shell-init-file (getenv "HOME")))))

;; ------------------ ;;
;;  *scratch* related ;;
;; ------------------ ;;
(defun qjp-create-scratch-buffer ()
  "Create a new scratch buffer and return it."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (insert initial-scratch-message)
    buf))

;; Quickly switch from and to *scratch*
(defvar qjp-scratch-from-buffer nil
  "From where we switch to *scratch*.")

(defun qjp-switch-to-scratch-or-back ()
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (when qjp-scratch-from-buffer
        (switch-to-buffer qjp-scratch-from-buffer))
    (let ((from-buffer (current-buffer)))
      (switch-to-buffer (or (get-buffer "*scratch*")
                            (qjp-create-scratch-buffer)))
      (setq qjp-scratch-from-buffer from-buffer))))

;; ---------- ;;
;; Web search ;;
;; ---------- ;;
(defun qjp-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt.

Modified the original function.  Always put the word at prompt."
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
  "Given some information regarding a search engine, install the interactive command to search through them."
  `(defun ,(intern (format "qjp-search-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (qjp-search ,search-engine-url ,search-engine-prompt)))

(qjp-install-search-engine "google" "http://www.google.com/search?q=" "Google: ")
(qjp-install-search-engine "bing-dict" "http://www.bing.com/dict/search?q=" "Bing Dict: ")

(provide 'qjp-defuns-misc)
;;; qjp-defuns-misc.el ends here
