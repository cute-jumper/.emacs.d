;;; qjp-misc.el --- Simple settings for various modes

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

;; Guideline:
;; 1. If the settings are short, put them directly in this file
;; 2. Otherwise, put it into a separate file under `misc' directory.
;;    The feature name should be `qjp-misc-{mode name}'

;;; Code:

;; Macro definition
(defmacro qjp-misc-subdir-defun-macro (mode-name)
  "A macro to simplify the `defun' for requiring feature in
  sub-directory"
  (let ((func-name (intern (concat "qjp-misc-" (symbol-name mode-name)))))
    `(defun ,func-name ()
       (qjp-modules-require-subdir-feature
        "misc"
        (concat "qjp-misc-" ,(symbol-name mode-name) "-mode")))))

;; ------------------------------------- ;;
;; Enable or disable some built-in modes ;;
;; ------------------------------------- ;;
(qjp-misc-subdir-defun-macro builtin)

;; -------------- ;;
;; key-chord mode ;;
;; -------------- ;;
(defun qjp-misc-key-chord ()
  (require 'key-chord)
  (key-chord-mode t))

;; -------------------- ;;
;; region-bindings mode ;;
;; -------------------- ;;
(defun qjp-misc-region-bindings ()
  (require 'region-bindings-mode)
  (region-bindings-mode-enable))

;; ---------------- ;;
;; goto-last-change ;;
;; ---------------- ;;
(defun qjp-misc-goto-last-change ()
  (require 'goto-last-change)
  (global-set-key "\C-x\C-\\" 'goto-last-change))

;; ------------- ;;
;; ace-jump-mode ;;
;; ------------- ;;
(defun qjp-misc-ace-jump ()
  (global-set-key (kbd "C-;") 'ace-jump-line-mode)
  (global-set-key (kbd "C-:") 'ace-jump-mode))

;; --------------- ;;
;; ace-jump-buffer ;;
;; --------------- ;;
(defun qjp-misc-ace-jump-buffer ()
  (key-chord-define-global "bb" 'ace-jump-buffer))

;; --------- ;;
;; jump-char ;;
;; --------- ;;
(defun qjp-misc-jump-char ()
  (key-chord-define-global "jk" 'jump-char-forward)
  (key-chord-define-global "kl" 'jump-char-backward))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(defun qjp-misc-expand-region ()
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  ;; (pending-delete-mode)
  )

;; ----------------------------------------------------- ;;
;; multiple-cursors, using region, global and mouse bindings ;;
;; ----------------------------------------------------- ;;
(qjp-misc-subdir-defun-macro multiple-cursors)

;; ------ ;;
;; EasyPG ;;
;; ------ ;;
(defun qjp-misc-easypg ()
  (setenv "GPG_AGENT_INFO" nil)
  ;; save the password
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  ;; auto-save
  (setq epa-file-inhibit-auto-save nil))

;; ---------- ;;
;; dictionary ;;
;; ---------- ;;
(defun qjp-misc-dictionary ()
  (setq dictionary-tooltip-dictionary "stardic")
  (setq dictionary-server "localhost")
  (global-set-key (kbd "C-c d") 'dictionary-search))

;; --- ;;
;; w3m ;;
;; --- ;;
(defun qjp-misc-w3m ()
  (setq w3m-default-display-inline-images t)
  (setq w3m-home-page "http://www.google.com"))

;; ---------------- ;;
;; command-log-mode ;;
;; ---------------- ;;
(defun qjp-misc-command-log ()
  (require 'command-log-mode))

;; ----- ;;
;; calfw ;;
;; ----- ;;
(defun qjp-misc-calfw ()
  (require 'calfw)
  (require 'calfw-org))

;; ------------- ;;
;; markdown-mode ;;
;; ------------- ;;
(defun qjp-misc-markdown ()
  (require 'markdown-mode+)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; ----------- ;;
;; sr-speedbar ;;
;; ----------- ;;
(defun qjp-misc-sr-speedbar ()
  (require 'sr-speedbar)
  (setq sr-speedbar-right-side nil)
  (setq speedbar-use-images nil)
  (global-set-key [f8] 'sr-speedbar-toggle))

;; --------------- ;;
;; predictive mode ;;
;; --------------- ;;
(defun qjp-misc-predictive ()
  (require 'predictive))

;; --------- ;;
;; evil-mode ;;
;; --------- ;;
(qjp-misc-subdir-defun-macro evil)

;; --------- ;;
;; term-mode ;;
;; --------- ;;
(defun qjp-misc-term ()
  ;; Turn of yasnippet in order to let tab behave normally
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

;; ----- ;;
;; dired ;;
;; ----- ;;
(qjp-misc-subdir-defun-macro dired)

;; ----------- ;;
;; tabbar-mode ;;
;; ----------- ;;
(qjp-misc-subdir-defun-macro tabbar)

;; --------------------- ;;
;; hideshow, hideshowvis ;;
;; --------------------- ;;
(qjp-misc-subdir-defun-macro hs)

;; --------- ;;
;; site-lisp ;;
;; --------- ;;

;; ------------ ;;
;; fetch-bibtex ;;
;; ------------ ;;
(defun qjp-misc-fetch-bibtex ()
  (require 'fetch-bibtex)
  (setq fetch-bibtex-script
        (expand-file-name         
         "fetch_bibtex.py" (expand-file-name "fetch-bibtex" qjp-site-lisp-dir))))

;; ------------------ ;;
;; My own extensions! ;;
;; ------------------ ;;

;; ------- ;;
;; THUmacs ;;
;; ------- ;;
(defun qjp-misc-thumacs ()
  (require 'deadline-util)
  (setq dp-userid "")                   ;Demo only
  (setq dp-userpass "")                 ;Demo only
  (setq dp-homework-file (concat "Agenda/homework.org")))

;; -------------------------------------- ;;
;; List the modes you want to enable here ;;
;; -------------------------------------- ;;
(defvar qjp-enabled-misc-settings-list
  '(builtin key-chord region-bindings goto-last-change ace-jump ace-jump-buffer
            jump-char expand-region multiple-cursors easypg dictionary w3m
            command-log calfw markdown sr-speedbar evil term dired hs fetch-bibtex)
  "The short mode function name that should be enabled")

;; Enable these settings
(defun qjp-enable-misc-settings ()
  (mapc (lambda (m) (funcall (intern (concat "qjp-misc-" (symbol-name m)))))
        qjp-enabled-misc-settings-list))

(qjp-enable-misc-settings)

(provide 'qjp-misc)
;;; qjp-misc.el ends here
