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
  (define-key qjp-mode-map (kbd "C-x i") #'helm-imenu)
  (define-key qjp-mode-map (kbd "C-c h d") #'helm-dash)
  ;; helm-multi-swoop
  (define-key qjp-mode-map (kbd "C-c h M-i") #'helm-multi-swoop))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-M-x-fuzzy-match t
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
  ;; Hide header when only one source
  ;; From: https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window
  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
  (defvar helm-source-header-default-height (face-attribute 'helm-source-header :height) )

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
  (require 'helm-swoop))

(with-eval-after-load 'helm-files
  ;; Let helm support zsh-like path expansion.
  (defvar helm-ff-expand-valid-only-p t)
  (defvar helm-ff-sort-expansions-p t)
  (defun helm-ff-try-expand-fname (candidate)
    (let ((dirparts (split-string candidate "/"))
          valid-dir
          fnames)
      (catch 'break
        (while dirparts
          (if (file-directory-p (concat valid-dir (car dirparts) "/"))
              (setq valid-dir (concat valid-dir (pop dirparts) "/"))
            (throw 'break t))))
      (setq fnames (cons candidate (helm-ff-try-expand-fname-1 valid-dir dirparts)))
      (if helm-ff-sort-expansions-p
          (sort fnames
                (lambda (f1 f2) (or (file-directory-p f1)
                                (not (file-directory-p f2)))))
        fnames)))

  (defun helm-ff-try-expand-fname-1 (parent children)
    (if children
        (if (equal children '(""))
            (and (file-directory-p parent) `(,(concat parent "/")))
          (when (file-directory-p parent)
            (apply 'nconc
                   (mapcar
                    (lambda (f)
                      (or (helm-ff-try-expand-fname-1 f (cdr children))
                          (unless helm-ff-expand-valid-only-p
                            (and (file-directory-p f)
                                 `(,(concat f "/" (mapconcat 'identity
                                                             (cdr children)
                                                             "/")))))))
                    (directory-files parent t (concat "^"
                                                      (regexp-quote
                                                       (car children))))))))
      `(,(concat parent (and (file-directory-p parent) "/")))))

  (defun qjp-helm-ff-try-expand-fname (orig-func &rest args)
    (let* ((candidate (car args))
           (collection (helm-ff-try-expand-fname candidate)))
      (if (and (> (length collection) 1)
               (not (file-exists-p candidate)))
          (with-helm-alive-p
            (when (helm-file-completion-source-p)
              (helm-exit-and-execute-action
               (lambda (_)
                 (helm-find-files-1
                  (helm-comp-read "Expand Path to: " collection))))))
        (apply orig-func args))))

  (advice-add 'helm-ff-kill-or-find-buffer-fname :around #'qjp-helm-ff-try-expand-fname))

;; helm-swoop
(setq helm-swoop-speed-or-color t) ;; Color needed

;; helm-bibtex
(setq helm-bibtex-bibliography qjp-bibtex-database-file)

(provide 'qjp-misc-helm)
;;; qjp-misc-helm.el ends here
