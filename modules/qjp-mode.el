;;; qjp-mode.el --- qjp minor mode:-)                -*- lexical-binding: t; -*-

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

;; TODO: add tips
;; how to bind following keys
;; C-i, C-j, C-m (dangerous)
;; C-return, C-tab, C-;, C-', C-, C-\
;; M-m, M-', M-[, M-]
;; C-x {w, t, y, p, g, j, x, c} (no bindings yet)
;; C-x {i, a, f, l, g, m, (maybe v)} (not that useful bindings)

;;; Code:

(require 'qjp-defuns)

;; key remapping
(defmacro qjp-push-event (key-str)
  `(lambda () (interactive)
     (setq unread-command-events
           (listify-key-sequence (kbd ,key-str)))))

;;; prefix for editting functions
(define-prefix-command 'ctrl-c-extension-map)
(defun qjp-mode-define-ctrl-c-extension-map ()
  (define-key ctrl-c-extension-map "f" #'mode-line-other-buffer)
  (define-key ctrl-c-extension-map "a" #'align-regexp)
  (define-key ctrl-c-extension-map "d" #'qjp-duplicate-line-or-region)
  (define-key ctrl-c-extension-map "e" #'qjp-eval-and-replace)
  (define-key ctrl-c-extension-map "=" #'qjp-calc-eval-and-replace)
  (define-key ctrl-c-extension-map "J" #'qjp-join-next-line)
  (define-key ctrl-c-extension-map "j" #'join-line)
  (define-key ctrl-c-extension-map "s" #'eshell)
  (define-key ctrl-c-extension-map "S" #'ansi-term)
  (define-key ctrl-c-extension-map "m" #'qjp-kill-back-to-indentation)
  (define-key ctrl-c-extension-map "r" #'revert-buffer)
  (define-key ctrl-c-extension-map "lf" #'add-file-local-variable)
  (define-key ctrl-c-extension-map "lp" #'add-file-local-variable-prop-line)
  (define-key ctrl-c-extension-map "ld" #'add-dir-local-variable)
  (define-key ctrl-c-extension-map (kbd "M-s") #'center-line)
  (define-key ctrl-c-extension-map (kbd "M-S") #'center-paragraph))
(qjp-mode-define-ctrl-c-extension-map)

(define-prefix-command 'ctrl-c-git-grep-map)
(defun qjp-mode-define-ctrl-c-git-grep-map ()
  (define-key ctrl-c-git-grep-map "r" #'rgrep)
  (define-key ctrl-c-git-grep-map "d" #'find-grep-dired)
  (define-key ctrl-c-git-grep-map "n" #'find-name-dired))
(qjp-mode-define-ctrl-c-git-grep-map)

(define-prefix-command 'ctrl-c-yasnippet-map)
(defun qjp-mode-define-ctrl-c-yasnippet-map ()
  (define-key ctrl-c-yasnippet-map "c" #'aya-create)
  (define-key ctrl-c-yasnippet-map "e" #'aya-expand))
(qjp-mode-define-ctrl-c-yasnippet-map)

(defvar qjp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; redefine basic movement
    (define-key map (kbd "C-h") #'backward-delete-char)
    (define-key map (kbd "M-[") #'beginning-of-defun)
    (define-key map (kbd "M-]") #'end-of-defun)
    ;; Bindings for `qjp-defuns-edit'
    (define-key map [remap move-beginning-of-line]
      #'qjp-back-to-indentation-or-beginning)
    (define-key map (kbd "M-;") #'qjp-comment-dwim-line)
    (define-key map (kbd "C-o") #'qjp-open-new-line)
    (define-key map (kbd "C-x C-k") #'kill-this-buffer)
    (define-key map [S-up] #'qjp-move-line-up)
    (define-key map [S-down] #'qjp-move-line-down)
    ;; Bindings for `qjp-defun-misc'
    (define-key map (kbd "C-z") #'qjp-switch-to-scratch-or-back)

    ;; ------------------ ;;
    ;; Other key bindings ;;
    ;; ------------------ ;;
    (define-key map [remap list-buffers] #'ibuffer)
    (define-key map [remap back-to-indentation] #'cycle-spacing)
    (define-key map [remap count-words-region] #'count-words)
    ;; (define-key map (kbd "M-s r") #'replace-string)
    (define-key map (kbd "M-/") #'hippie-expand)

    ;; -------------------- ;;
    ;; ctrl-c-extension-map ;;
    ;; -------------------- ;;
    (define-key map (kbd "C-c f") 'ctrl-c-extension-map)

    ;; ------------------- ;;
    ;; ctrl-c-git-grep-map ;;
    ;; ------------------- ;;
    (define-key map (kbd "C-c g") 'ctrl-c-git-grep-map)

    ;; -------------------- ;;
    ;; ctrl-c-yasnippet-map ;;
    ;; -------------------- ;;
    (define-key map (kbd "C-c y") 'ctrl-c-yasnippet-map)

    ;; ----- ;;
    ;; C-c q ;;
    ;; ----- ;;
    (define-key map (kbd "C-c q %") #'map-query-replace-regexp)

    ;; Mouse clicks are annoying. Globally set mouse 1's single-click events to
    ;; `qjp-nop'. Navigating using doulbe-click is still available.
    (define-key map [down-mouse-1] nil)
    (define-key map [mouse-1] #'qjp-nop)
    (define-key map (kbd "<C-mouse-1>") #'qjp-nop)
    (define-key map (kbd "<C-down-mouse-1>") #'qjp-nop)
    map))

(defun qjp-mode-translate-keys (on)
  (if on
      (progn
        (define-key key-translation-map (kbd "<menu>") (kbd "C-c")))
    (define-key key-translation-map (kbd "<menu>") (kbd "<menu>"))))

(defun qjp-mode-set-dangerous-ctrl-keys (on)
  (if on
      (progn
        (define-key input-decode-map (kbd "C-[") (kbd "H-["))
        (define-key qjp-mode-map (kbd "H-[") #'recenter-top-bottom)
        (define-key input-decode-map (kbd "C-m") (kbd "H-m"))
        (define-key qjp-mode-map (kbd "H-m") #'kill-word)
        (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
        (define-key qjp-mode-map (kbd "H-i") #'backward-kill-word))
    (define-key input-decode-map (kbd "C-[") (kbd "C-["))
    (define-key qjp-mode-map (kbd "H-[") nil)
    (define-key input-decode-map (kbd "C-m") (kbd "C-m"))
    (define-key qjp-mode-map (kbd "H-m") nil)
    (define-key input-decode-map (kbd "C-i") (kbd "C-i"))
    (define-key qjp-mode-map (kbd "H-i") nil)))

(defun qjp-mode-define-meta-o (on)
  (if on
      (progn
        (global-unset-key (kbd "M-o"))
        (define-key qjp-mode-map (kbd "M-o") #'other-window))
    (global-set-key (kbd "M-o") 'meta-o-map)))

(defun qjp-mode--keychord-remap (keychord key-str)
  (key-chord-define-global
   keychord
   (qjp-push-event key-str)))

(defun qjp-mode-define-keychords ()
  (with-eval-after-load 'qjp-misc
    (qjp-key-chord-define qjp-mode-map ";;" (µ (end-of-line) (insert ";")))
    (qjp-key-chord-define qjp-mode-map ";s" #'isearch-forward)
    (qjp-key-chord-define qjp-mode-map ";d" #'delete-char)
    (qjp-key-chord-define qjp-mode-map ";z" (kbd "C-/"))
    (qjp-key-chord-define qjp-mode-map ";g" (kbd "C-g"))
    (qjp-key-chord-define qjp-mode-map ";f" #'forward-char)
    (qjp-key-chord-define qjp-mode-map ";b" #'backward-char)
    (qjp-key-chord-define qjp-mode-map ";e" #'end-of-line)
    (qjp-key-chord-define qjp-mode-map ";a" #'qjp-back-to-indentation-or-beginning)
    (qjp-key-chord-define qjp-mode-map ";u" #'previous-line)
    (qjp-key-chord-define qjp-mode-map ";n" #'next-line)
    (qjp-key-chord-define qjp-mode-map ";y" #'yank)
    (qjp-key-chord-define qjp-mode-map ";w" #'kill-region)
    (qjp-key-chord-define qjp-mode-map ";1" (kbd "C-x 1"))
    (qjp-key-chord-define qjp-mode-map ";2" (kbd "C-x 2"))
    (qjp-key-chord-define qjp-mode-map ";3" (kbd "C-x 3"))
    (qjp-key-chord-define qjp-mode-map ";o" (kbd "C-x o"))))

(defun qjp-mode-define-isearch (on)
  (if on
      (progn
        (define-key isearch-mode-map (kbd "C-p") #'isearch-yank-x-selection)
        (define-key isearch-mode-map (kbd "C-o") #'qjp-isearch-occur)
        (define-key isearch-mode-map (kbd "C-k") #'qjp-kill-isearch-match)
        (define-key isearch-mode-map (kbd "M-z") #'qjp-zap-to-isearch)
        (define-key isearch-mode-map [(control return)] #'qjp-isearch-exit-other-end))
    (define-key isearch-mode-map (kbd "C-p") nil)
    (define-key isearch-mode-map (kbd "C-o") nil)
    (define-key isearch-mode-map (kbd "C-k") nil)
    (define-key isearch-mode-map (kbd "M-z") nil)
    (define-key isearch-mode-map [(control return)] nil)))

(define-minor-mode qjp-mode
  "Minor mode designed for Junpeng Qiu!

\\{qjp-mode-map}"
  nil
  " QJP"
  qjp-mode-map
  :group qjp-mode)

(define-globalized-minor-mode global-qjp-mode
  qjp-mode
  qjp-mode-on
  :group 'qjp-mode
  :require 'qjp-mode)

(defun qjp-mode-on ()
  "Turn on `qjp-mode'."
  (interactive)
  ;; key translation
  (qjp-mode-translate-keys t)
  ;; key-chord-mode
  (qjp-mode-define-keychords)
  ;; Bindings for `isearch'
  (qjp-mode-define-isearch t)
  ;; rebind meta-o
  (qjp-mode-define-meta-o t)
  ;; dangerous ctrl-keys
  ;; (qjp-mode-set-dangerous-ctrl-keys t)
  ;; additional quit
  (define-key key-translation-map (kbd "M-'") (kbd "C-g"))
  (qjp-mode +1))

(defun qjp-mode-off ()
  "Turn off `qjp-mode'."
  (interactive)
  (qjp-mode-translate-keys nil)
  ;; Bindings `qjp-defuns-isearch'
  (qjp-mode-define-isearch nil)
  (qjp-mode-define-meta-o nil)
  ;; (qjp-mode-set-dangerous-ctrl-keys nil)
  (define-key key-translation-map (kbd "M-'") (kbd "M-'"))
  (qjp-mode -1))

(global-qjp-mode +1)

;; Fix the problem when minibuffer first pop up
(defun qjp-mode-minibuffer-setup-hook ()
  (remove-hook 'minibuffer-setup-hook #'qjp-mode-minibuffer-setup-hook)
  (qjp-mode-on))
(add-hook 'minibuffer-setup-hook #'qjp-mode-minibuffer-setup-hook)

;; From http://stackoverflow.com/a/5340797
(defmacro qjp-define-highest-priority-mode-function (mode)
  (let ((fn (intern (format "qjp-gain-highest-keys-priority-%S" mode))))
    `(defun ,fn (_file)
       "Try to ensure that my keybindings retain priority over other minor modes.

Called via the `after-load-functions' special hook."
       (unless (eq (caar minor-mode-map-alist) ',mode)
         (let ((mykeys (assq ',mode minor-mode-map-alist)))
           (assq-delete-all ',mode minor-mode-map-alist)
           (add-to-list 'minor-mode-map-alist mykeys))))))

(qjp-define-highest-priority-mode-function qjp-mode)

(add-hook 'after-load-functions 'qjp-gain-highest-keys-priority-qjp-mode)

(provide 'qjp-mode)
;;; qjp-mode.el ends here
