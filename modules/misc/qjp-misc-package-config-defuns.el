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
  (defvar orig-d (symbol-function 'd)))

;; First part: simple settings

;; Define short name to simplify typing
(fset 'd (symbol-function 'qjp-misc-inline-defun))

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
   (defun key-chord-define (keymap keys command)
     "Redefine to make it have order"
     (if (/= 2 (length keys))
         (error "Key-chord keys must have two elements"))
     ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
     (let ((key1 (logand 255 (aref keys 0)))
           (key2 (logand 255 (aref keys 1))))
       (define-key keymap (vector 'key-chord key1 key2) command)))
   (key-chord-mode t))

;; -------------------- ;;
;; region-bindings mode ;;
;; -------------------- ;;
(d region-bindings
   (autoload 'region-bindings-mode-enable "region-bindings-mode")
   (region-bindings-mode-enable))

;; ---------------- ;;
;; goto-last-change ;;
;; ---------------- ;;
(d goto-last-change
   (global-set-key "\C-x\C-\\" 'goto-last-change))

;; ------------- ;;
;; ace-jump-mode ;;
;; ------------- ;;
(d ace-jump
   (global-set-key (kbd "C-;") 'ace-jump-mode)
   (global-set-key (kbd "C-:") 'ace-jump-char-mode))

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
   ;; This is faster(1 vs 26 ms) than calling setup function. I don't know why
   (global-set-key (kbd "C-.") 'ace-flyspell-dwim)
   (with-eval-after-load 'flyspell
     (define-key flyspell-mode-map (kbd "C-.") 'ace-flyspell-dwim)))

;; ------------------ ;;
;; ace-jump-helm-line ;;
;; ------------------ ;;
(d ace-jump-helm-line
   (with-eval-after-load 'helm
     (with-eval-after-load 'key-chord
       (key-chord-define helm-map "jj" 'ace-jump-helm-line))))

;; ---------- ;;
;; ace-pinyin ;;
;; ---------- ;;
(d ace-pinyin
   (ace-pinyin-global-mode))

;; --------- ;;
;; jump-char ;;
;; --------- ;;
(d jump-char
   (key-chord-define-global "jf" 'jump-char-forward)
   (key-chord-define-global "jb" 'jump-char-backward))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(d expand-region
   (global-set-key (kbd "C-=") 'er/expand-region))

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
   (with-eval-after-load 'flyspell
     (define-key flyspell-mode-map (kbd "C-,") nil)
     (define-key flyspell-mode-map (kbd "C-M-i") nil)
     (define-key flyspell-mode-map (kbd "C-;") nil)))

;; ----- ;;
;; magit ;;
;; ----- ;;
(d magit
   (setq magit-last-seen-setup-instructions "1.4.0")
   (global-set-key (kbd "C-x g") 'magit-status)
   (with-eval-after-load 'magit
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; ------------------- ;;
;; volatile-highlights ;;
;; ------------------- ;;
(d volatile-highlights
   (autoload 'volatile-highlights-mode "volatile-highlights")
   (volatile-highlights-mode +1)
   (diminish 'volatile-highlights-mode))

;; ------------- ;;
;; markdown-mode ;;
;; ------------- ;;
(d markdown
   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; ----------- ;;
;; sr-speedbar ;;
;; ----------- ;;
(d sr-speedbar
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
   (with-eval-after-load 'idle-highlight-mode
     (set-face-attribute 'idle-highlight nil
                         :weight 'semi-bold :underline
                         '(:color foreground-color :style line)
                         :inherit nil)))

;; --------------- ;;
;; smart mode line ;;
;; --------------- ;;
(d sml
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
   (global-set-key (kbd "C-x M-x") 'lacarte-execute-menu-command))

;; ------ ;;
;; swiper ;;
;; ------ ;;
(d swiper
   (global-set-key (kbd "C-r") 'swiper)
   (global-set-key (kbd "C-s") 'swiper)
   (setq swiper-completion-method 'helm))

;; -------- ;;
;; quickrun ;;
;; -------- ;;
(d quickrun
   (global-set-key (kbd "C-c q q") #'quickrun)
   (defun quickrun-region-dwim (&optional prefix)
     (interactive "P")
     (call-interactively
      (if prefix #'quickrun-replace-region
        #'quickrun-region)))
   (global-set-key (kbd "C-c q r") #'quickrun-region-dwim)
   (with-eval-after-load 'evil
     (add-to-list 'evil-emacs-state-modes 'quickrun/mode)))

;; --------------- ;;
;; gscholar-bibtex ;;
;; --------------- ;;
(d gscholar-bibtex
   (setq gscholar-bibtex-database-file qjp-bibtex-database-file))

;; --------- ;;
;; bing-dict ;;
;; --------- ;;
(d bing-dict
   (defun qjp-search-word-at-mouse ()
     "bing search word at mouse"
     (interactive)
     (when (featurep 'bing-dict)
       (save-excursion
         (mouse-set-point last-input-event)
         (let ((word (word-at-point)))
           (when word
             (bing-dict-brief word))))))
   (global-set-key (kbd "C-c D") 'bing-dict-brief)
   (global-set-key (kbd "<C-mouse-1>") 'qjp-search-word-at-mouse))

;; Restore the original version of `d'
(if (boundp 'orig-d)
    (fset 'd orig-d)
  (fset 'd nil))

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
