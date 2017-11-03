;;; qjp-programming-basic.el --- Basic settings for programming

;; Copyright (C) 2013  Junpeng Qiu

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

;; maybe put which-function-mode config and others(nyan-mode etc.) to another
;; place?

;;; Code:

;; Enable which-function-mode
(which-function-mode +1)
(setq which-func-unknown "n/a")
;; truncate which-func information
(defun qjp-which-func-current ()
  (let ((current (gethash (selected-window) which-func-table)))
    (if current
        (reverse (truncate-string-to-width (reverse current) 20 nil nil "…"))
      which-func-unknown)))
;; (setq which-func-format
;;       `("["
;;         (:propertize (:eval (qjp-which-func-current))
;;                      local-map ,which-func-keymap
;;                      face which-func
;;                      mouse-face mode-line-highlight
;;                      help-echo "mouse-1: go to beginning\n\
;; mouse-2: toggle rest visibility\n\
;; mouse-3: go to end")
;;         "]"))
;; Set title
(when (display-graphic-p)
  (let ((which-func '(which-func-mode ("" which-func-format " "))))
    (setq frame-title-format
          `((:eval (if (and (fboundp 'mc/num-cursors)
                            (> (mc/num-cursors) 1))
                       (format ,(concat "mc: "
                                        (propertize "%d" 'face 'font-lock-warning-face)
                                        " ")
                               (mc/num-cursors))
                     ""))
            ,which-func
            (:eval (when (> (buffer-size) 1024)
                     "(%I) "))
            (:eval
             (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
               "%b"))))
    (setq-default mode-line-format (remove which-func mode-line-format))
    (setq-default mode-line-misc-info (remove which-func mode-line-misc-info))))

;; Move which-function to more important place
;; (let ((which-func '(which-func-mode ("" which-func-format " ")))
;;       cell)
;;   (setq-default mode-line-format (remove which-func mode-line-format))
;;   (setq-default mode-line-misc-info (remove which-func mode-line-misc-info))
;;   (setq cell (last mode-line-format 8))
;;   (setcdr cell
;;           (cons which-func
;;                 (cdr cell)))
;;   (setq-default mode-line-format (append (butlast mode-line-format 8)
;;                                          cell)))
;; (set-face-foreground 'which-func (face-foreground font-lock-function-name-face))

;; Set which-function-mode to show in header line
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

;; --------------- ;;
;; electriy-return ;;
;; --------------- ;;
;; From http://www.emacswiki.org/emacs/ParEdit
(defvar qjp-electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun qjp-electrify-return-if-match (arg)
  "If the text after the cursor matches `qjp-electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (when (looking-at qjp-electrify-return-match)
      (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defvar qjp-indent-when-closing-pair-modes nil)

(defun qjp-indent-when-closing-pair (orig-func &rest args)
  (prog1
      (apply orig-func args)
    (and (memq major-mode qjp-indent-when-closing-pair-modes)
         (looking-back "}" nil)
         (let ((end (point)))
           (save-excursion
             (sp-backward-sexp)
             (indent-region (point) end))))))

(advice-add 'c-electric-brace :around 'qjp-indent-when-closing-pair)

;; Put *Shell Command Output* buffers into view-mode
(defadvice shell-command-on-region
    (after qjp-shell-command-in-view-mode
           (start end command &optional output-buffer replace error-buffer display-error-buffer)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

(defun qjp-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode +1))

(defun qjp-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun qjp-prog-mode-hook ()
  (qjp-local-comment-auto-fill)
  (qjp-add-watchwords)
  (abbrev-mode +1)
  (prettify-symbols-mode +1)
  (highlight-symbol-mode +1)
  (highlight-symbol-nav-mode +1)
  (hs-minor-mode +1)
  (hl-line-mode +1)
  (flycheck-mode +1)
  (smartparens-mode +1))

(add-hook 'prog-mode-hook #'qjp-prog-mode-hook)

;; sh-mode
;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(defun qjp-sh-mode-hook ()
  (add-to-list (make-local-variable 'company-backends) 'company-shell))

(add-hook 'sh-mode-hook 'qjp-sh-mode-hook)

;; eshell
(defalias 'eshell/f 'find-file-other-window)
(defalias 'eshell/d 'dired)
(defun qjp-eshell-mode-hook ()
  ;; Use company instead of helm-eshell to complete
  (define-key eshell-mode-map [remap eshell-pcomplete] #'company-capf)
  ;; (define-key eshell-mode-map [remap eshell-pcomplete] #'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
  ;; Make company-capf use pcomplete
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  ;; Eldoc
  (autoload 'esh-help-eldoc-command "esh-help")
  (set (make-local-variable 'eldoc-documentation-function)
       'esh-help-eldoc-command)
  (define-key eshell-mode-map (kbd "M-h") #'esh-help-run-help)
  ;; No margin
  (set (make-local-variable 'scroll-margin) 0)
  ;; Don't scroll to bottom
  (remove-hook 'eshell-output-filter-functions
               'eshell-postoutput-scroll-to-bottom)
  (setenv "PAGER" "cat"))

(add-hook 'eshell-mode-hook #'qjp-eshell-mode-hook)

(defface qjp-eshell-dir-face
  '((t . (:bold t
                :inherit font-lock-constant-face)))
  "Face for directory name."
  :group 'eshell)

(defun qjp-eshell-git-is-dirty ()
  (let ((output (with-temp-buffer
                  (call-process "git" nil t nil "status" "--porcelain")
                  (buffer-string))))
    (not (string-empty-p output))))

(defun qjp-eshell-prompt ()
  (require 'magit)
  (concat
   "\u250c\u2500["
   (propertize (eshell/pwd) 'face 'qjp-eshell-dir-face)
   "]"
   "\u2500\u2500["
   (format-time-string "%Y-%m-%d %H:%M" (current-time))
   "]\n\u2514\u2500>"
   (let ((branch (magit-get-current-branch)))
     (if branch
         (concat " ("
                 (propertize branch 'face font-lock-function-name-face)
                 (if (qjp-eshell-git-is-dirty)
                     (propertize " \u2717" 'face font-lock-warning-face)
                   "")
                 ")")
       ""))
   (if (= (user-uid) 0)
       (propertize " #" 'face font-lock-warning-face)
     " $")
   " "))

(setq eshell-prompt-function 'qjp-eshell-prompt)
(setq eshell-highlight-prompt nil)

(provide 'qjp-programming-basic)
;;; qjp-programming-basic.el ends here
