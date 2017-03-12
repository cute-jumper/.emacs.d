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
(define-key helm-command-map "g" #'helm-do-grep-ag)
(define-key helm-command-map "G" #'helm-google-suggest)
(define-key helm-command-map "d" #'helm-dash)
(define-key helm-command-map "k" #'helm-descbinds)
(define-key helm-command-map "=" #'helm-calcul-expression)
(define-key helm-command-map "o" #'helm-occur)
(define-key helm-command-map "p" #'helm-browse-project)
(define-key helm-command-map "P" #'helm-list-emacs-process)
(define-key helm-command-map (kbd "SPC") #'helm-all-mark-rings)
(define-key helm-command-map (kbd "M-!") #'helm-run-external-command)
(define-key helm-command-map (kbd "M-:") #'helm-eval-expression)
(define-key helm-command-map (kbd "M-i") #'helm-multi-swoop)
(define-key helm-command-map (kbd "M m") #'helm-browse-menubar)
;; helm-mode-manager
(define-key helm-command-map (kbd "M d") #'helm-disable-minor-mode)
(define-key helm-command-map (kbd "M e") #'helm-enable-minor-mode)
(define-key helm-command-map (kbd "M M") #'helm-switch-major-mode)

(define-key qjp-mode-map (kbd "M-x") #'helm-M-x)
(define-key qjp-mode-map (kbd "M-y") #'helm-show-kill-ring)
(define-key qjp-mode-map (kbd "C-x C-f") #'helm-find-files)
(define-key qjp-mode-map (kbd "C-x b") #'helm-mini)
(define-key qjp-mode-map (kbd "C-x c") nil)
(global-unset-key (kbd "C-x c"))
(define-key qjp-mode-map (kbd "C-c h") 'helm-command-prefix)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      ;; helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      ;; Following is good, but sometimes annoying
      ;;helm-ff-auto-update-initial-value t
      helm-ff-file-name-history-use-recentf t
      ;; ignore case
      ;; helm-case-fold-search 'smart
      )

;; Helm-mode is slow (more than 0.5s to start)
(with-eval-after-load 'helm
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (helm-mode +1)
  (helm-autoresize-mode +1)
  (helm-descbinds-mode +1)
  (helm-flx-mode +1)
  ;; Hide header when only one source
  ;; From: https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window
  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
  (defvar helm-source-header-default-height (face-attribute 'helm-source-header :height))

  (defun helm-toggle-header-line ()
    "Hide the `helm' header is there is only one source."
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height helm-source-header-default-height)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  ;; helm-swoop
  (require 'helm-swoop)
  ;; helm-projectile
  (require 'helm-projectile))

(define-key ctrl-c-git-grep-map "a" #'helm-ag)
(define-key ctrl-c-git-grep-map "h" #'helm-do-ag)
(define-key ctrl-c-git-grep-map "p" #'helm-do-ag-project-root)
(define-key ctrl-c-git-grep-map "/" #'helm-find)
(define-key ctrl-c-git-grep-map "g" #'helm-grep-do-git-grep)

;; helm-swoop
(setq helm-swoop-speed-or-color t) ;; Color needed

;; helm-bibtex
(setq helm-bibtex-bibliography qjp-bibtex-database-file)

;; ----------------------------------------- ;;
;; fuzzy-match for all helm related packages ;;
;; ----------------------------------------- ;;
(setq helm-M-x-fuzzy-match t
      helm-ff-fuzzy-matching t
      helm-buffers-fuzzy-matching t
      helm-apropos-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-locate-library-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-mode-fuzzy-match t
      ;; third-party
      helm-swoop-use-fuzzy-match t
      helm-projectile-fuzzy-match t
      helm-ag-fuzzy-match t)

;; ---------- ;;
;; `helm-ext' ;;
;; ---------- ;;
(with-eval-after-load 'helm-files
  (helm-ext-ff-enable-auto-path-expansion t)
  (helm-ext-ff-enable-skipping-dots t))

(with-eval-after-load 'helm
  (helm-ext-minibuffer-enable-header-line-maybe t))

(provide 'qjp-misc-helm)
;;; qjp-misc-helm.el ends here
