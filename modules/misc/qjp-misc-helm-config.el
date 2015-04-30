;;; qjp-misc-helm-config.el --- Helm configuration   -*- lexical-binding: t; -*-

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

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(eval-after-load "helm"
  '(progn
                                        ; rebind tab to run persistent action
     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
                                        ; make TAB works in terminal
     (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
                                        ; list actions using C-z
     (define-key helm-map (kbd "C-z")  'helm-select-action)))
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      ;;helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t)
(autoload 'helm-autoresize-mode "helm")
(setq helm-bibtex-bibliography qjp-bibtex-database-file)

;; Helm-mode is slow (more than 0.5s to start)
(run-with-idle-timer 1 nil (lambda ()
                             (helm-mode)
                             (helm-autoresize-mode t)
                             (helm-descbinds-mode)))

(provide 'qjp-misc-helm-config)
;;; qjp-misc-helm-config.el ends here
