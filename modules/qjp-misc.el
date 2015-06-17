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

;; Guidelines:
;; 1. If the settings are short, put them directly in this file
;; 2. Otherwise, put it into a separate file under `misc' directory.
;;    The feature name should be `qjp-misc-{mode name}'

;;; Code:

;; ------------------------------- ;;
;; Macro and functions definitions ;;
;; ------------------------------- ;;
(defsubst qjp-misc--make-func-name (feature)
  "Helper function to construct function name of `misc' module by FEATURE."
  (concat "qjp-misc-" (symbol-name feature)))

(defmacro qjp-misc--defun (feature &rest body)
  "Toplevel macro to define macro of `misc' module by FEATURE and BODY."
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
  "Macro for inline settings."
  (declare (indent 1))
  `(qjp-misc--defun ,feature
     ,@args))

(defmacro qjp-misc-config-infile (feature)
  "Macro for infile settings."
  (declare (indent 1))
  `(defun ,feature ()
     (require ',feature)))

;; ---------------------------------- ;;
;; Load settings for various packages ;;
;; ---------------------------------- ;;
;; First part: simple settings

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
(defun qjp-key-chord-define (keymap keys command)
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))

(qjp-misc-config-inline key-chord
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
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-x C-\\") 'goto-last-change)))

;; --- ;;
;; avy ;;
;; --- ;;
(qjp-misc-config-inline avy
  (with-eval-after-load 'qjp-mode
    (setq avy-background t)
    (qjp-key-chord-define qjp-mode-map "jk" 'avy-goto-word-1)
    (qjp-key-chord-define qjp-mode-map "jl" 'avy-goto-char)))

;; ------- ;;
;; avy-zap ;;
;; ------- ;;
(qjp-misc-config-inline avy-zap
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "M-z") 'avy-zap-to-char-dwim)
    (define-key qjp-mode-map (kbd "M-Z") 'avy-zap-up-to-char-dwim)))

;; ------------ ;;
;; ace-flyspell ;;
;; ------------ ;;
(qjp-misc-config-inline ace-flyspell
  ;; This is faster(1 vs 26 ms) than calling setup function. I don't know why
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-.") 'ace-flyspell-dwim))
  ;; FIXME
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-.") 'ace-flyspell-dwim)))

;; -------- ;;
;; ace-link ;;
;; -------- ;;
(qjp-misc-config-inline ace-link
  (ace-link-setup-default "j"))

;; ------------------ ;;
;; ace-jump-helm-line ;;
;; ------------------ ;;
(qjp-misc-config-inline ace-jump-helm-line
  ;; FIXME
  (with-eval-after-load 'helm
    (qjp-key-chord-define helm-map "jj" #'ace-jump-helm-line)))

;; ---------- ;;
;; ace-pinyin ;;
;; ---------- ;;
(qjp-misc-config-inline ace-pinyin
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode +1))

;; --------- ;;
;; jump-char ;;
;; --------- ;;
(qjp-misc-config-inline jump-char
  (with-eval-after-load 'qjp-mode
    (qjp-key-chord-define qjp-mode-map "jf" 'jump-char-forward)
    (qjp-key-chord-define qjp-mode-map "jb" 'jump-char-backward)))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(qjp-misc-config-inline expand-region
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-=") 'er/expand-region)))

;; ------ ;;
;; ispell ;;
;; ------ ;;
(qjp-misc-config-inline ispell
  (let ((hunspell-name "hunspell"))
    (if (executable-find hunspell-name)
        (eval `(with-eval-after-load 'ispell
                 (setq ispell-local-dictionary-alist '((nil
                                                        "[[:alpha:]]"
                                                        "[^[:alpha:]]"
                                                        "[']"
                                                        t
                                                        ("-d" "en_US")
                                                        nil
                                                        iso-8859-1)))
                 (setq ispell-program-name ,hunspell-name)
                 (add-hook 'text-mode-hook 'turn-on-flyspell)))
      (message "[Warning]: Please consider installing %s." hunspell-name))))

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
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-x g") 'magit-status))
  (with-eval-after-load 'magit
    (set-face-foreground 'magit-diff-add "green4")
    (set-face-foreground 'magit-diff-del "red3")))

;; --------- ;;
;; guide-key ;;
;; --------- ;;
(qjp-misc-config-inline guide-key
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-x r" "M-s" "M-o"))
  (run-with-idle-timer 2 nil #'guide-key-mode))

;; ------------------- ;;
;; volatile-highlights ;;
;; ------------------- ;;
(qjp-misc-config-inline volatile-highlights
  (autoload 'volatile-highlights-mode "volatile-highlights")
  (volatile-highlights-mode +1))

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
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map [f8] 'sr-speedbar-toggle)))

;; --------- ;;
;; undo-tree ;;
;; --------- ;;
(qjp-misc-config-inline undo-tree
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; ------- ;;
;; diff-hl ;;
;; ------- ;;
(qjp-misc-config-inline diff-hl
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

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
  "See the source code to find out styles. Use hydra for key bindings."
  (setq rebox-style-loop '(21 23 25)))

;; ------------------- ;;
;; idle-highlight-mode ;;
;; ------------------- ;;
(qjp-misc-config-inline highlight-symbol
  "Use bold and underline instead of `region' color to highlight."
  (with-eval-after-load 'highlight-symbol
    (set-face-attribute 'highlight-symbol-face nil
                        :weight 'semi-bold :underline t)))

;; --------------- ;;
;; smart mode line ;;
;; --------------- ;;
(qjp-misc-config-inline sml
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/Programs/" ":Pro:") t))

;; -------- ;;
;; flycheck ;;
;; -------- ;;
(qjp-misc-config-inline flycheck
  nil)

;; -------- ;;
;; fcitx.el ;;
;; -------- ;;
(qjp-misc-config-inline fcitx
  (ignore-errors
    (fcitx-aggressive-setup)))

;; --------- ;;
;; nyan-mode ;;
;; --------- ;;
(qjp-misc-config-inline nyan
  (nyan-mode))

;; ------------------ ;;
;; whitespace-cleanup ;;
;; ------------------ ;;
(qjp-misc-config-inline whitespace-cleanup
  (global-whitespace-cleanup-mode))

;; ----------- ;;
;; hl-sentence ;;
;; ----------- ;;
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
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-x t") 'anchored-transpose)))

;; ------- ;;
;; lacarte ;;
;; ------- ;;
(qjp-misc-config-inline lacarte
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-x M-x") 'lacarte-execute-menu-command)))

;; --------- ;;
;; easy-kill ;;
;; --------- ;;
(qjp-misc-config-inline easy-kill
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "M-w") 'easy-kill)))

;; ------------ ;;
;; change-inner ;;
;; ------------ ;;
(qjp-misc-config-inline change-inner
  (with-eval-after-load 'qjp-mode
    (define-key meta-o-map (kbd "i") 'change-inner)
    (define-key meta-o-map (kbd "o") 'change-outer)))

;; -------- ;;
;; quickrun ;;
;; -------- ;;
(qjp-misc-config-inline quickrun
  (defun quickrun-region-dwim (&optional prefix)
    (interactive "P")
    (call-interactively
     (if prefix #'quickrun-replace-region
       #'quickrun-region)))
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-c q q") #'quickrun)
    (define-key qjp-mode-map (kbd "C-c q r") #'quickrun-region-dwim))
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
    "bing search word at mouse."
    (interactive)
    (when (featurep 'bing-dict)
      (save-excursion
        (mouse-set-point last-input-event)
        (let ((word (word-at-point)))
          (when word
            (bing-dict-brief word))))))
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-c D") 'bing-dict-brief))
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "<C-mouse-1>") 'qjp-search-word-at-mouse)))

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
  '(avy avy-zap ace-flyspell ace-jump-helm-line ace-link ace-pinyin anchored-transpose anzu auto-insert
        bing-dict
        company change-inner
        dired diminish diff-hl
        easypg expand-region easy-kill
        flyspell flycheck
        goto-last-change gscholar-bibtex guide-key
        helm hs fcitx hydra;; loaded after helm
        highlight-symbol ispell
        ;;jump-char
        key-chord
        lacarte
        magit markdown multiple-cursors
        projectile
        quickrun
        rebox region-bindings
        sml nyan;; load after sml
        sr-speedbar smartparens
        term
        undo-tree
        volatile-highlights
        whitespace-cleanup whole-line-or-region
        ;;yasnippet
        )
  "The names for the packages that should be enabled.")

;; Enable these settings
(defun qjp-misc-enable-setting (feature-name)
  (let ((func-name (qjp-misc--make-func-name feature-name)))
    (qjp-timed (funcall (intern func-name)) func-name "21qjp-misc-details")))

(mapc 'qjp-misc-enable-setting qjp-enabled-misc-settings-list)

(provide 'qjp-misc)
;;; qjp-misc.el ends here
