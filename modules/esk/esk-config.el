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
;; (defun esk-turn-off-tool-bar ()
;;   (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

;; can't do it at launch or emacsclient won't always honor it
;; (add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

;; --------- ;;
;; Variables ;;
;; --------- ;;
;; uniquify
;; (require 'uniquify)

;; ---- ;;
;; ffap ;;
;; ---- ;;
;; (require 'ffap)
;; (defvar ffap-c-commment-regexp "^/\\*+"
;;   "Matches an opening C-style comment, like \"/***\".")

;; (defadvice ffap-file-at-point (after avoid-c-comments activate)
;;   "Don't return paths like \"/******\" unless they actually exist.

;; This fixes the bug where ido would try to suggest a C-style
;; comment as a filename."
;;   (ignore-errors
;;     (when (and ad-return-value
;;                (string-match-p ffap-c-commment-regexp
;;                                ad-return-value)
;;                (not (ffap-file-exists-string ad-return-value)))
;;       (setq ad-return-value nil))))

;; --------- ;;
;; Variables ;;
;; --------- ;;

;; ------ ;;
;; ispell ;;
;; ------ ;;
;; (eval-after-load "ispell"
;;   '(when (executable-find ispell-program-name)
;;      (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; (defalias 'auto-tail-revert-mode 'tail-mode)

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

(provide 'esk-config)
;;; esk-config.el ends here
