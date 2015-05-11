;;; qjp-misc.el --- Simple settings for various modes/features

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
(defsubst qjp-misc--make-func-name (feature)
  "Helper function to construct function name of `misc' module"
  (concat "qjp-misc-" (symbol-name feature)))

(defmacro qjp-misc--defun (feature &rest body)
  "Toplevel macro to define macro of `misc' module"
  (declare (indent 1))
  (let* ((func-name (qjp-misc--make-func-name feature))
         (func-symbol (intern func-name))
         (func-inactive-p (intern (concat func-name "--inactive-p"))))
    `(progn
       (defvar ,func-inactive-p t)
       (defsubst ,func-symbol (&optional v)
         (when (or ,func-inactive-p v)
           (setq ,func-inactive-p nil)
           ,@body)))))

(defmacro qjp-misc-config-inline (feature &rest args)
  "Macro for inline settings"
  (declare (indent 1))
  `(qjp-misc--defun ,feature
     ,@args))

(defmacro qjp-misc-config-infile (feature)
  "Macro for infile settings"
  (declare (indent 1))
  `(defun ,feature ()
     (require ',feature)))

;; ---------------------------------- ;;
;; Load settings for various packages ;;
;; ---------------------------------- ;;
;; First part: simple settings

;; ------------------ ;;
;; auto-complete-mode ;;
;; ------------------ ;;
(qjp-misc-config-inline auto-complete
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode))

;; ------------ ;;
;; company-mode ;;
;; ------------ ;;
(qjp-misc-config-inline company
  (global-company-mode))

;; --------- ;;
;; yasnippet ;;
;; --------- ;;
(qjp-misc-config-inline yasnippet
  (yas-global-mode))

;; -------------- ;;
;; key-chord mode ;;
;; -------------- ;;
(qjp-misc-config-inline key-chord
  (with-eval-after-load 'key-chord
    (defun key-chord-define (keymap keys command)
      "Redefine to make it have order"
      (if (/= 2 (length keys))
          (error "Key-chord keys must have two elements"))
      ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
      (let ((key1 (logand 255 (aref keys 0)))
            (key2 (logand 255 (aref keys 1))))
        (define-key keymap (vector 'key-chord key1 key2) command))))
  (key-chord-mode +1))

;; -------------------- ;;
;; region-bindings mode ;;
;; -------------------- ;;
(qjp-misc-config-inline region-bindings
  (autoload 'region-bindings-mode-enable "region-bindings-mode")
  (region-bindings-mode-enable))

;; ---------------- ;;
;; goto-last-change ;;
;; ---------------- ;;
(qjp-misc-config-inline goto-last-change
  (global-set-key "\C-x\C-\\" 'goto-last-change))

;; ------------- ;;
;; ace-jump-mode ;;
;; ------------- ;;
(qjp-misc-config-inline ace-jump
  (global-set-key (kbd "C-;") 'ace-jump-mode)
  (global-set-key (kbd "C-:") 'ace-jump-char-mode))

;; ------------ ;;
;; ace-jump-zap ;;
;; ------------ ;;
(qjp-misc-config-inline ace-jump-zap
  (global-set-key (kbd "M-z") 'ace-jump-zap-to-char-dwim)
  (global-set-key (kbd "M-Z") 'ace-jump-zap-up-to-char-dwim))

;; ------------ ;;
;; ace-flyspell ;;
;; ------------ ;;
(qjp-misc-config-inline ace-flyspell
  ;; This is faster(1 vs 26 ms) than calling setup function. I don't know why
  (global-set-key (kbd "C-.") 'ace-flyspell-dwim)
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-.") 'ace-flyspell-dwim)))

;; ------------------ ;;
;; ace-jump-helm-line ;;
;; ------------------ ;;
(qjp-misc-config-inline ace-jump-helm-line
  (with-eval-after-load 'helm
    (with-eval-after-load 'key-chord
      (key-chord-define helm-map "jj" 'ace-jump-helm-line))))

;; ---------- ;;
;; ace-pinyin ;;
;; ---------- ;;
(qjp-misc-config-inline ace-pinyin
  (ace-pinyin-global-mode +1))

;; --------- ;;
;; jump-char ;;
;; --------- ;;
(qjp-misc-config-inline jump-char
  (key-chord-define-global "jf" 'jump-char-forward)
  (key-chord-define-global "jb" 'jump-char-backward))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(qjp-misc-config-inline expand-region
  (global-set-key (kbd "C-=") 'er/expand-region))

;; ------ ;;
;; ispell ;;
;; ------ ;;
(qjp-misc-config-inline ispell
  (setq ispell-program-name "hunspell")
  (require 'rw-hunspell)
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; -------- ;;
;; flyspell ;;
;; -------- ;;
(qjp-misc-config-inline flyspell
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-,") nil)
    (define-key flyspell-mode-map (kbd "C-M-i") nil)
    (define-key flyspell-mode-map (kbd "C-;") nil)))

;; ----- ;;
;; magit ;;
;; ----- ;;
(qjp-misc-config-inline magit
  (setq magit-last-seen-setup-instructions "1.4.0")
  (global-set-key (kbd "C-x g") 'magit-status)
  (with-eval-after-load 'magit
    (set-face-foreground 'magit-diff-add "green4")
    (set-face-foreground 'magit-diff-del "red3")))

;; ------------------- ;;
;; volatile-highlights ;;
;; ------------------- ;;
(qjp-misc-config-inline volatile-highlights
  (autoload 'volatile-highlights-mode "volatile-highlights")
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

;; ------------- ;;
;; markdown-mode ;;
;; ------------- ;;
(qjp-misc-config-inline markdown
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; ----------- ;;
;; sr-speedbar ;;
;; ----------- ;;
(qjp-misc-config-inline sr-speedbar
  (setq sr-speedbar-right-side nil)
  (setq speedbar-use-images nil)
  (global-set-key [f8] 'sr-speedbar-toggle))

;; --------------- ;;
;; predictive mode ;;
;; --------------- ;;
(qjp-misc-config-inline predictive
  (require 'predictive))

;; --------- ;;
;; term-mode ;;
;; --------- ;;
(qjp-misc-config-inline term
  "Turn of yasnippet in order to let tab behave normally"
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

;; ---------- ;;
;; rebox-mode ;;
;; ---------- ;;
(qjp-misc-config-inline rebox
  "See the source code to find out styles"
  (setq rebox-style-loop '(21 23 25)))

;; ------------------- ;;
;; idle-highlight-mode ;;
;; ------------------- ;;
(qjp-misc-config-inline idle-highlight
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
(qjp-misc-config-inline sml
  (setq sml/theme "powerline")
  (sml/setup))

;; -------- ;;
;; fcitx.el ;;
;; -------- ;;
(qjp-misc-config-inline fcitx
  (fcitx-aggressive-setup))

;; ------------------------------------------------------------
;; nyan-mode
(qjp-misc-config-inline nyan
  (nyan-mode))

;; ------------------------------------------------------------
;; hl-sentence
(qjp-misc-config-inline hl-sentence
  (require 'hl-sentence)
  (set-face-attribute 'hl-sentence-face nil
                      :foreground "white")
  (set-face-attribute 'variable-pitch nil
                      :foreground "gray40"))

;; --------- ;;
;; anzu-mode ;;
;; --------- ;;
(qjp-misc-config-inline anzu
  (global-anzu-mode +1)
  (setq anzu-minimum-input-length 3))

;; -------------------- ;;
;; whole-line-or-region ;;
;; -------------------- ;;
(qjp-misc-config-inline whole-line-or-region
  (whole-line-or-region-mode +1))

;; ------------------ ;;
;; anchored-transpose ;;
;; ------------------ ;;
(qjp-misc-config-inline anchored-transpose
  (global-set-key (kbd "C-x t") 'anchored-transpose))

;; ------- ;;
;; lacarte ;;
;; ------- ;;
(qjp-misc-config-inline lacarte
  (global-set-key (kbd "C-x M-x") 'lacarte-execute-menu-command))

;; ------ ;;
;; swiper ;;
;; ------ ;;
(qjp-misc-config-inline swiper
  (global-set-key (kbd "C-r") 'swiper)
  (global-set-key (kbd "C-s") 'swiper)
  (setq swiper-completion-method 'helm))

;; -------- ;;
;; quickrun ;;
;; -------- ;;
(qjp-misc-config-inline quickrun
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
(qjp-misc-config-inline gscholar-bibtex
  (setq gscholar-bibtex-database-file qjp-bibtex-database-file))

;; --------- ;;
;; bing-dict ;;
;; --------- ;;
(qjp-misc-config-inline bing-dict
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

;; Second part: make defuns for settings in files
(add-to-list 'load-path (concat qjp-modules-dir "/misc"))
(let ((file-basenames
       (mapcar
        #'(lambda (x) (intern (file-name-base x)))
        (directory-files
         (concat qjp-modules-dir "/misc") t "^[^#]*.el$"))))
  (dolist (basename file-basenames)
    (eval `(qjp-misc-config-infile ,basename))))

;; -------------------------------------- ;;
;; List the modes you want to enable here ;;
;; -------------------------------------- ;;
(defvar qjp-enabled-misc-settings-list
  '(ace-jump ace-jump-zap ace-flyspell ace-jump-helm-line ace-pinyin anchored-transpose anzu auto-insert
             bing-dict
             company
             dired
             easypg expand-region
             flyspell
             goto-last-change gscholar-bibtex
             helm hs fcitx hydra;; loaded after helm
             idle-highlight ispell
             jump-char
             key-chord
             lacarte
             magit markdown multiple-cursors
             nyan
             projectile
             quickrun
             rebox region-bindings
             sml sr-speedbar
             term
             volatile-highlights
             whole-line-or-region
             ;;yasnippet
             )
  "The short names for the packages that should be enabled")

;; Enable these settings
(defun qjp-misc-enable-setting (feature-name)
  (let ((func-name (qjp-misc--make-func-name feature-name)))
    (qjp-timed (funcall (intern func-name)) func-name "21qjp-misc-details")))

(mapc 'qjp-misc-enable-setting qjp-enabled-misc-settings-list)

(provide 'qjp-misc)
;;; qjp-misc.el ends here
