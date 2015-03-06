;;; esk-config.el --- Configurations borrowed from Emacs Starter Kit

;; Copyright (C) 2014  Junpeng Qiu

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

;; Mainly from starter-kit-misc.el

;;; Code:

;; ------- ;;
;; toolbar ;;
;; ------- ;;
(defun esk-turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

;; --------- ;;
;; Variables ;;
;; --------- ;;
;; uniquify
(require 'uniquify)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat qjp-base-dir "oddmuse")
      save-place-file (concat qjp-base-dir "places")
      backup-directory-alist `(("." . ,(concat qjp-base-dir "backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; -------- ;;
;; ido-mode ;;
;; -------- ;;
;; Obsolete. Use helm instead
;; (ido-mode t)
;; (ido-ubiquitous-mode)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-auto-merge-work-directories-length nil
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-use-virtual-buffers t
;;       ido-handle-duplicate-virtual-buffers 2
;;       ido-max-prospects 10)

;; ---- ;;
;; ffap ;;
;; ---- ;;
(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.

This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

;; --------- ;;
;; Variables ;;
;; --------- ;;
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ------ ;;
;; ispell ;;
;; ------ ;;
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))

(defalias 'auto-tail-revert-mode 'tail-mode)

(eval-after-load 'hippie-exp
  '(progn
     (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
       (delete f hippie-expand-try-functions-list))

     ;; Add this back in at the end of the list.
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

;; Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; Get around the emacswiki spam protection
;; (eval-after-load 'oddmuse
;;   (add-hook 'oddmuse-mode-hook
;;             (lambda ()
;;               (unless (string-match "question" oddmuse-post)
;;                 (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))))

;; smex
;; (setq smex-save-file (concat qjp-base-dir ".smex-items"))
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)

(provide 'esk-config)
;;; esk-config.el ends here
