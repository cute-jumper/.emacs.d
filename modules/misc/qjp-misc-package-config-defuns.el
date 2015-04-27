;;; qjp-misc-package-config-defuns.el --- Settings for various packages

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

;; 

;;; Code:

;; I don't want to override existing things so save the original version of `d'
;; if it exists
(when (fboundp 'd)
    (defalias 'orig-d 'd))

;; First part: simple settings

;; Define short name to simplify typing
(defalias 'd 'qjp-misc-inline-defun)

;; ------------------ ;;
;; auto-complete-mode ;;
;; ------------------ ;;
(d auto-complete
   (require 'auto-complete-config)
   (ac-config-default)
   (global-auto-complete-mode))

;; ------------ ;;
;; company-mode ;;
;; ------------ ;;
(d company
   (global-company-mode))

;; --------- ;;
;; yasnippet ;;
;; --------- ;;
(d yasnippet
   (yas-global-mode))

;; -------------- ;;
;; key-chord mode ;;
;; -------------- ;;
(d key-chord
   (require 'key-chord)
   (key-chord-mode t))

;; -------------------- ;;
;; region-bindings mode ;;
;; -------------------- ;;
(d region-bindings
   (require 'region-bindings-mode)
   (region-bindings-mode-enable))

;; ---------------- ;;
;; goto-last-change ;;
;; ---------------- ;;
(d goto-last-change
   (require 'goto-last-change)
   (global-set-key "\C-x\C-\\" 'goto-last-change))

;; ------------- ;;
;; ace-jump-mode ;;
;; ------------- ;;
(d ace-jump
   (global-set-key (kbd "C-;") 'ace-jump-mode)
   (global-set-key (kbd "C-:") 'ace-jump-char-mode))

;; --------------- ;;
;; ace-jump-buffer ;;
;; --------------- ;;
(d ace-jump-buffer
   (key-chord-define-global "jb" 'ace-jump-buffer))

;; ------------ ;;
;; ace-jump-zap ;;
;; ------------ ;;
(d ace-jump-zap
   (global-set-key (kbd "M-z") 'ace-jump-zap-to-char-dwim)
   (global-set-key (kbd "M-Z") 'ace-jump-zap-up-to-char-dwim))

;; ------------ ;;
;; ace-flyspell ;;
;; ------------ ;;
(d ace-flyspell
   (ace-flyspell-setup))

;; ------------------ ;;
;; ace-jump-helm-line ;;
;; ------------------ ;;
(d ace-jump-helm-line
   (eval-after-load "helm"
     '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line)))

;; ---------- ;;
;; ace-pinyin ;;
;; ---------- ;;
(d ace-pinyin
   (ace-pinyin-global-mode))

;; --------- ;;
;; jump-char ;;
;; --------- ;;
(d jump-char
   (key-chord-define-global "jc" 'jump-char-forward))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(d expand-region
   (require 'expand-region)
   (global-set-key (kbd "C-=") 'er/expand-region)
   ;; (pending-delete-mode)
   )

;; ------ ;;
;; ispell ;;
;; ------ ;;
(d ispell
   (setq ispell-program-name "hunspell")
   (require 'rw-hunspell)
   (when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; -------- ;;
;; flyspell ;;
;; -------- ;;
(d flyspell
   (eval-after-load "flyspell"
     '(progn (define-key flyspell-mode-map (kbd "C-,") nil)
             (define-key flyspell-mode-map (kbd "C-M-i") nil)
             (define-key flyspell-mode-map (kbd "C-;") nil))))

;; ---------- ;;
;; dictionary ;;
;; ---------- ;;
(d dictionary
   (setq dictionary-tooltip-dictionary "stardic")
   (setq dictionary-server "localhost")
   (global-set-key (kbd "C-c d") 'dictionary-search))

;; ----- ;;
;; magit ;;
;; ----- ;;
(d magit
   (setq magit-last-seen-setup-instructions "1.4.0"))

;; --- ;;
;; w3m ;;
;; --- ;;
(d w3m
   (setq w3m-default-display-inline-images t)
   (setq w3m-home-page "http://www.google.com"))

;; ---------------- ;;
;; command-log-mode ;;
;; ---------------- ;;
(d command-log
   (require 'command-log-mode))

;; ----- ;;
;; calfw ;;
;; ----- ;;
(d calfw
   (require 'calfw)
   (require 'calfw-org))

;; ------------- ;;
;; markdown-mode ;;
;; ------------- ;;
(d markdown
   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; ----------- ;;
;; sr-speedbar ;;
;; ----------- ;;
(d sr-speedbar
   (require 'sr-speedbar)
   (setq sr-speedbar-right-side nil)
   (setq speedbar-use-images nil)
   (global-set-key [f8] 'sr-speedbar-toggle))

;; --------------- ;;
;; predictive mode ;;
;; --------------- ;;
(d predictive
   (require 'predictive))

;; --------- ;;
;; term-mode ;;
;; --------- ;;
(d term
   "Turn of yasnippet in order to let tab behave normally"
   (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

;; ---------- ;;
;; rebox-mode ;;
;; ---------- ;;
(d rebox
   "See the source code to find out styles"
   (setq rebox-style-loop '(21 23 25)))

;; ------------------- ;;
;; idle-highlight-mode ;;
;; ------------------- ;;
(d idle-highlight
   "Use bold and underline instead of `region' color to
highlight."
   (require 'idle-highlight-mode)
   (set-face-attribute 'idle-highlight nil
                       :weight 'semi-bold :underline
                       '(:color foreground-color :style line)
                       :inherit nil))

;; --------------- ;;
;; smart mode line ;;
;; --------------- ;;
(d sml
   "Setup smart mode line"
   (require 'powerline)
   (setq sml/theme "powerline")
   (sml/setup))

;; -------- ;;
;; fcitx.el ;;
;; -------- ;;
(d fcitx
   (fcitx-aggressive-setup))

;; ------------------------------------------------------------
;; nyan-mode
(d nyan
   (nyan-mode))

;; ------------------------------------------------------------
;; hl-sentence
(d hl-sentence
   (require 'hl-sentence)
   (set-face-attribute 'hl-sentence-face nil
                       :foreground "white")
   (set-face-attribute 'variable-pitch nil
                       :foreground "gray40"))

;; --------- ;;
;; anzu-mode ;;
;; --------- ;;
(d anzu
   (global-anzu-mode +1)
   (setq anzu-minimum-input-length 3))

;; -------------------- ;;
;; whole-line-or-region ;;
;; -------------------- ;;
(d whole-line-or-region
   (whole-line-or-region-mode +1))

;; ------------------ ;;
;; anchored-transpose ;;
;; ------------------ ;;
(d anchored-transpose
   (global-set-key (kbd "C-x t") 'anchored-transpose))

;; ------- ;;
;; lacarte ;;
;; ------- ;;
(d lacarte
   (require 'lacarte)
   (global-set-key (kbd "C-x M-x") 'lacarte-execute-menu-command))

;; ------ ;;
;; swiper ;;
;; ------ ;;
(d swiper
   (global-set-key (kbd "C-r") 'swiper)
   (global-set-key (kbd "C-s") 'swiper)
   (setq swiper-completion-method 'helm))

;; --------------- ;;
;; gscholar-bibtex ;;
;; --------------- ;;
(d gscholar-bibtex
   (setq gscholar-bibtex-database-file
         (expand-file-name "~/texmf/bibtex/bib/refs.bib")))

;; Restore the original version of `d'
(if (fboundp 'orig-d)
    (defalias 'd 'orig-d)
  (defalias 'd nil))

;; Second part: make defuns for settings in files
  
(let ((file-basenames
       (mapcar
        'file-name-base
        (directory-files
         (concat qjp-modules-dir "/misc") t "^[^#]*config.el$"))))
  (dolist (basename file-basenames)
    (eval
     `(qjp-misc-file-defun
       ,(intern
         (replace-regexp-in-string "qjp-misc-\\|-config" "" basename))))))

(provide 'qjp-misc-package-config-defuns)
;;; qjp-misc-package-config-defuns.el ends here
