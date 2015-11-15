;;; qjp-misc-helm.el --- Helm configuration   -*- lexical-binding: t; -*-

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

(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "M-x") #'helm-M-x)
  (define-key qjp-mode-map (kbd "M-y") #'helm-show-kill-ring)
  (define-key qjp-mode-map (kbd "C-x C-f") #'helm-find-files)
  (define-key qjp-mode-map (kbd "C-x b") #'helm-mini)
  (define-key qjp-mode-map (kbd "C-c h") #'helm-command-prefix)
  (define-key qjp-mode-map (kbd "C-x c") nil)
  (define-key qjp-mode-map (kbd "C-c h g") #'helm-google-suggest)
  (define-key qjp-mode-map (kbd "C-c h d") #'helm-dash)
  ;; helm-multi-swoop
  (define-key qjp-mode-map (kbd "C-c h M-i") #'helm-multi-swoop))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      ;;helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      ;; Following is good, but sometimes annoying
      ;;helm-ff-auto-update-initial-value t
      helm-ff-file-name-history-use-recentf t)

;; Helm-mode is slow (more than 0.5s to start)
(with-eval-after-load 'helm
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (helm-mode)
  (helm-autoresize-mode t)
  (helm-descbinds-mode)
  (require 'helm-swoop))

;; helm-swoop
(setq helm-swoop-speed-or-color t) ;; Color needed

;; helm-bibtex
(setq helm-bibtex-bibliography qjp-bibtex-database-file)

(provide 'qjp-misc-helm)
;;; qjp-misc-helm.el ends here
