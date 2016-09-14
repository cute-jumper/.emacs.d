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
  (global-company-mode +1)
  (setq company-show-numbers t)
  (company-quickhelp-mode +1)
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-<tab>") #'company-complete)))

;; ------------ ;;
;; helm-company ;;
;; ------------ ;;
(qjp-misc-config-inline helm-company
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-c h") 'helm-company)
    (define-key company-active-map (kbd "C-c h") 'helm-company)))

;; --------- ;;
;; yasnippet ;;
;; --------- ;;
(qjp-misc-config-inline yasnippet
  (autoload 'yas-reload-all "yasnippet")
  (defun qjp-yasnippet-initialize ()
    (interactive)
    (yas-reload-all)
    (yas-minor-mode +1))
  (with-eval-after-load 'qjp-mode
    (define-key ctrl-c-yasnippet-map "i" #'qjp-yasnippet-initialize)
    (define-key ctrl-c-yasnippet-map "y" #'yas-insert-snippet)
    (define-key ctrl-c-yasnippet-map (kbd "<tab>") #'company-yasnippet)))

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
  (key-chord-mode +1)
  (setq key-chord-two-keys-delay 0.5)
  (fset 'key-chord-define 'qjp-key-chord-define))

;; -------------------- ;;
;; region-bindings mode ;;
;; -------------------- ;;
(qjp-misc-config-inline region-bindings
  (autoload 'region-bindings-mode-enable "region-bindings-mode")
  (region-bindings-mode-enable)
  (defun qjp-region-bindings-mode-on ()
    "Turn on region bindings mode.
Don't use this, use `region-bindings-mode-enable'."
    (and (use-region-p)
         (or (not region-bindings-mode-enabled-modes)
             (memq major-mode region-bindings-mode-enabled-modes))
         (not (memq major-mode region-bindings-mode-disabled-modes))
         (not (catch 'disable
                (dolist (pred region-bindings-mode-disable-predicates)
                  (and (funcall pred)
                       (throw 'disable t)))))
         (region-bindings-mode 1)))
  (fset 'region-bindings-mode-on 'qjp-region-bindings-mode-on))

;; ------------------ ;;
;; persistent-scratch ;;
;; ------------------ ;;
(qjp-misc-config-inline persistent-scratch
  (persistent-scratch-autosave-mode +1))

;; --------- ;;
;; pdf-tools ;;
;; --------- ;;
(qjp-misc-config-inline pdf-tools
  (autoload 'pdf-view-mode "pdf-view")
  (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
  (let ((epdfinfo-program (car
                           (file-expand-wildcards
                            (concat qjp-base-dir "elpa/pdf-tools-*/epdfinfo")))))
    (unless (and epdfinfo-program
                 (file-executable-p epdfinfo-program))
      (ignore-errors
        (pdf-tools-install t))))
  (defun qjp-pdf-view-mode-hook ()
    (pdf-tools-enable-minor-modes)
    (key-chord-define-local "jj" nil)
    (setq pdf-view-display-size 'fit-height))
  (with-eval-after-load 'pdf-view
    (add-hook 'pdf-view-mode-hook #'qjp-pdf-view-mode-hook)
    (define-key pdf-view-mode-map "j" #'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map "k" #'pdf-view-previous-line-or-previous-page)))

;; ------- ;;
;; avy-zap ;;
;; ------- ;;
(qjp-misc-config-inline avy-zap
  (with-eval-after-load 'qjp-mode
    (setq avy-zap-forward-only t)
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
  (ace-link-setup-default "o"))

;; ------------------ ;;
;; ace-jump-helm-line ;;
;; ------------------ ;;
(qjp-misc-config-inline ace-jump-helm-line
  ;; FIXME
  (with-eval-after-load 'helm
    (setq ace-jump-helm-line-style 'pre)
    (setq ace-jump-helm-line-default-action 'select)
    (setq ace-jump-helm-line-select-key ?e)
    (setq ace-jump-helm-line-move-only-key ?o)
    (setq ace-jump-helm-line-persistent-key ?p)
    (define-key helm-map (kbd "C-'") #'ace-jump-helm-line)))

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
    (qjp-key-chord-define qjp-mode-map ";j" 'jump-char-forward)
    (qjp-key-chord-define qjp-mode-map ";h" 'jump-char-backward)))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(qjp-misc-config-inline expand-region
  (dolist (func '(er/mark-inside-pairs
                  er/mark-inside-quotes
                  er/mark-outside-pairs
                  er/mark-outside-quotes
                  er/mark-defun
                  er/mark-comment
                  er/mark-text-sentence
                  er/mark-text-paragraph
                  er/mark-word
                  er/mark-url
                  er/mark-email
                  er/mark-symbol))
    (autoload func "expand-region")))

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
  (with-eval-after-load 'magit
    (add-hook 'magit-popup-mode-hook
              (lambda () (setq show-trailing-whitespace nil))))
  (with-eval-after-load 'qjp-mode
    (define-key ctrl-c-git-grep-map "s" #'magit-status)
    (define-key ctrl-c-git-grep-map "c" #'magit-clone)
    (define-key ctrl-c-git-grep-map "i" #'magit-init)))

;; god-mode
(qjp-misc-config-inline god-mode
  (with-eval-after-load 'qjp-mode
    ;; convenient window management key bindings
    (define-key qjp-mode-map (kbd "C-x C-1") 'delete-other-windows)
    (define-key qjp-mode-map (kbd "C-x C-2") 'split-window-below)
    (define-key qjp-mode-map (kbd "C-x C-3") 'split-window-right)
    (define-key qjp-mode-map (kbd "C-x C-0") 'delete-window)
    (define-key qjp-mode-map (kbd "C-x C-b") 'helm-mini)
    (define-key qjp-mode-map (kbd "C-x M-b") 'ibuffer)
    (define-key qjp-mode-map (kbd "C-x C-;") 'mode-line-other-buffer)
    (qjp-define-highest-priority-mode-function god-local-mode)
    (defun qjp-god-mode-enable-hook ()
      (set (make-local-variable 'input-method-function) nil)
      (qjp-gain-highest-keys-priority-god-local-mode nil)
      (if (and (fboundp 'evil-emacs-state-p)
               (evil-emacs-state-p))
          (setq evil-emacs-state-cursor 'hollow)
        (setq cursor-type 'hollow)))
    (add-hook 'god-mode-enabled-hook 'qjp-god-mode-enable-hook)
    (defun qjp-god-mode-disable-hook ()
      (setq input-method-function 'key-chord-input-method)
      (if (and (fboundp 'evil-emacs-state-p)
               (evil-emacs-state-p))
          (setq evil-emacs-state-cursor 'bar)
        (setq cursor-type 'bar)))
    (add-hook 'god-mode-disabled-hook 'qjp-god-mode-disable-hook))
  (with-eval-after-load 'god-mode
    (define-key god-local-mode-map "m" #'kill-word)
    (define-key god-local-mode-map "i" #'backward-kill-word)
    (define-key god-local-mode-map "." #'repeat)
    ;; isearch god-mode integration
    (require 'god-mode-isearch)
    (defun qjp-god-mode-isearch-activate ()
      (interactive)
      (set-cursor-color "yellow")
      (god-mode-isearch-activate))
    (define-key isearch-mode-map (kbd "<escape>") #'qjp-god-mode-isearch-activate)
    (qjp-key-chord-define isearch-mode-map "jj" #'qjp-god-mode-isearch-activate)
    (defun qjp-god-mode-isearch-disable ()
      (interactive)
      (set-cursor-color "white")
      (god-mode-isearch-disable))
    (define-key god-mode-isearch-map (kbd "<escape>") 'qjp-god-mode-isearch-disable)
    (add-hook 'isearch-mode-end-hook (lambda () (set-cursor-color "white")))))

;; ------------ ;;
;; indent-guide ;;
;; ------------ ;;
(qjp-misc-config-inline indent-guide
  (setq indent-guide-delay 0.1))

;; --------- ;;
;; which-key ;;
;; --------- ;;
(qjp-misc-config-inline which-key
  (setq which-key-idle-delay 1.0)
  (setq which-key-special-keys nil)
  (setq which-key-sort-order 'which-key-description-order)
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-c ?") #'which-key-show-top-level))
  (which-key-add-key-based-replacements
    "C-c f" "useful commands")
  (which-key-add-key-based-replacements
    "C-c q" "visual replace/quickrun")
  (which-key-mode +1))

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
  (global-diff-hl-mode +1)
  (setq diff-hl-side 'right)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (with-eval-after-load 'qjp-mode
    (define-key ctrl-c-git-grep-map "[" #'diff-hl-previous-hunk)
    (define-key ctrl-c-git-grep-map "]" #'diff-hl-next-hunk)))

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

;; ---------------- ;;
;; highlight-symbol ;;
;; ---------------- ;;
(qjp-misc-config-inline highlight-symbol
  "Use bold and underline instead of `region' color to highlight."
  (with-eval-after-load 'highlight-symbol
    (set-face-attribute 'highlight-symbol-face nil
                        :weight 'semi-bold :underline t)))

;; --------------- ;;
;; smart mode line ;;
;; --------------- ;;
(qjp-misc-config-inline sml
  (setq sml/theme 'dark)
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/Programs/" ":Pro:") t))

;; -------- ;;
;; fcitx.el ;;
;; -------- ;;
(qjp-misc-config-inline fcitx
  (ignore-errors
    (setq fcitx-use-dbus t)
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
    (define-key qjp-mode-map (kbd "C-c t") 'anchored-transpose)))

;; ------- ;;
;; lacarte ;;
;; ------- ;;
(qjp-misc-config-inline lacarte
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-c M-x") 'lacarte-execute-menu-command)))

;; --------- ;;
;; easy-kill ;;
;; --------- ;;
(qjp-misc-config-inline easy-kill
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "M-w") 'easy-kill)
    (define-key ctrl-c-mark-map (kbd "SPC") #'easy-mark)))

;; ----------- ;;
;; google-this ;;
;; ----------- ;;
(qjp-misc-config-inline google-this
  (with-eval-after-load 'qjp-mode
    (define-key ctrl-c-extension-map "g" #'google-this)))

;; ------------ ;;
;; change-inner ;;
;; ------------ ;;
(qjp-misc-config-inline change-inner
  (with-eval-after-load 'qjp-mode
    (define-key meta-i-map (kbd "i") 'change-inner)
    (define-key meta-i-map (kbd "o") 'change-outer)))

;; -------------------- ;;
;; centered-cursor-mode ;;
;; -------------------- ;;
(qjp-misc-config-inline centered-cursor-mode
  (global-centered-cursor-mode +1))

;; ---------- ;;
;; workgroups ;;
;; ---------- ;;
(qjp-misc-config-inline workgroups2
  (setq wg-emacs-exit-save-behavior 'ask)
  (setq wg-load-last-workgroup nil)
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "s-z") #'wg-revert-workgroup)
    (define-key qjp-mode-map (kbd "s-/") #'wg-switch-to-workgroup)))

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
    (define-key qjp-mode-map (kbd "C-c q k") #'quickrun)
    (define-key qjp-mode-map (kbd "C-c q K") #'quickrun-region-dwim))
  (with-eval-after-load 'evil
    (add-to-list 'evil-emacs-state-modes 'quickrun/mode)))

(qjp-misc-config-inline visual-regexp
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "C-c q r") #'vr/replace)
    (define-key qjp-mode-map (kbd "C-c q q") #'vr/query-replace)
    (define-key qjp-mode-map (kbd "C-c q m") #'vr/mc-mark)))

;; ------- ;;
;; keyfreq ;;
;; ------- ;;
(qjp-misc-config-inline keyfreq
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

;; ----------- ;;
;; beacon-mode ;;
;; ----------- ;;
(qjp-misc-config-inline beacon
  (beacon-mode +1))

;; ------- ;;
;; paradox ;;
;; ------- ;;
(qjp-misc-config-inline paradox
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t))


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
    (when (fboundp 'bing-dict-brief)
      (save-excursion
        (mouse-set-point last-input-event)
        (let ((word (word-at-point)))
          (when word
            (bing-dict-brief word))))))
  (setq bing-dict-add-to-kill-ring t)
  (with-eval-after-load 'qjp-mode
    (define-key qjp-mode-map (kbd "<C-mouse-1>") 'qjp-search-word-at-mouse)
    (define-key ctrl-c-extension-map "b" 'bing-dict-brief)))

;; --------- ;;
;; gmpl-mode ;;
;; --------- ;;
(qjp-misc-config-inline gmpl-mode
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . gmpl-mode))
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
                 '(gmpl-mode "and" "else" "mod" "union" "by" "if" "not" "within"
                             "cross" "in" "or" "diff" "inter" "symdiff" "div" "less" "then"
                             "abs" "atan" "card" "ceil" "cos" "exp" "floor"
                             "gmtime" "length" "log" "log10"
                             "max" "min" "round" "sin" "sqrt" "str2time" "trunc"
                             "Irand224" "Uniform" "Normal01" "Normal"
                             "sum" "prod"
                             "substr" "time2str"
                             "param" "by" "setof" "forall" "exists" "dimen"
                             "default" "integer" "binary" "symbolic" "var"))))

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
        beacon bing-dict
        company change-inner
        dired diminish diff-hl
        easypg expand-region easy-kill
        easypg expand-region easy-kill evil-surround evil
        flyspell flycheck
        gmpl-mode gscholar-bibtex google-this
        helm hs fcitx hydra;; loaded after helm
        gmpl-mode gscholar-bibtex google-this god-mode
        highlight-symbol ispell
        ;;jump-char
        key-chord keyfreq
        lacarte
        magit markdown multiple-cursors
        pdf-tools projectile
        paradox quickrun
        rebox region-bindings
        sml nyan;; load after sml
        sr-speedbar smartparens
        term
        undo-tree
        visual-regexp volatile-highlights
        whitespace-cleanup whole-line-or-region workgroups2 which-key
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
