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

;;; Code:

(require 'qjp-defuns)

;;; prefix for editting functions
(define-prefix-command 'ctrl-c-extension-map)
(defun qjp-mode-define-ctrl-c-extension-map ()
  (define-key ctrl-c-extension-map "d" #'qjp-duplicate-line-or-region)
  (define-key ctrl-c-extension-map "e" #'qjp-eval-and-replace)
  (define-key ctrl-c-extension-map "=" #'qjp-calc-eval-and-replace)
  (define-key ctrl-c-extension-map "j" #'qjp-join-next-line)
  (define-key ctrl-c-extension-map "J" #'join-line)
  (define-key ctrl-c-extension-map "s" #'qjp-isearch-other-window)
  (define-key ctrl-c-extension-map "m" #'qjp-kill-back-to-indentation)
  (define-key ctrl-c-extension-map "r" #'revert-buffer))
(qjp-mode-define-ctrl-c-extension-map)

;; Used for `easy-kill' and `expand-region'
(define-prefix-command 'ctrl-c-mark-map)
(defun qjp-mode-define-ctrl-c-mark-map ())
(qjp-mode-define-ctrl-c-mark-map)

(defvar qjp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; redefine basic movement
    (define-key map (kbd "C-h") #'backward-char)
    (define-key map (kbd "C-l") #'forward-char)
    (define-key map (kbd "C-b") #'backward-word)
    (define-key map (kbd "C-f") #'forward-word)
    (define-key map (kbd "C-c l") #'recenter-top-bottom)
    (define-key map (kbd "C-<") #'beginning-of-buffer)
    (define-key map (kbd "C->") #'end-of-buffer)
    ;; Bindings for `qjp-defuns-edit'
    (define-key map (kbd "M-s r") #'replace-string)
    (define-key map [remap move-beginning-of-line]
      #'qjp-back-to-indentation-or-beginning)
    (define-key map (kbd "M-;") #'qjp-comment-dwim-line)
    (define-key map (kbd "C-o") #'qjp-open-new-line)
    (define-key map [S-up] #'qjp-move-line-up)
    (define-key map [S-down] #'qjp-move-line-down)
    (define-key map "%" #'qjp-goto-match-paren)

    ;; ------------------ ;;
    ;; Other key bindings ;;
    ;; ------------------ ;;
    (define-key map [remap list-buffers] #'ibuffer)
    (define-key map [remap just-one-space] #'cycle-spacing)
    (define-key map [remap count-words-region] #'count-words)
    (define-key map (kbd "C-c /") #'hippie-expand)
    (define-key map (kbd "C-z") #'qjp-switch-to-scratch-or-back)

    ;; -------------------- ;;
    ;; ctrl-c-extension-map ;;
    ;; -------------------- ;;
    (define-key map (kbd "C-c f") 'ctrl-c-extension-map)
    ;; --------------- ;;
    ;; ctrl-c-mark-map ;;
    ;; --------------- ;;
    (define-key map (kbd "C-c SPC") 'ctrl-c-mark-map)

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
        (define-key key-translation-map (kbd "ESC") (kbd "C-c"))
        (define-key key-translation-map (kbd "<menu>") (kbd "C-x")))
    (define-key key-translation-map (kbd "ESC") (kbd "ESC"))
    (define-key key-translation-map (kbd "C-<escape>") (kbd "C-<escape>"))))

;; `M-o' prefix keymap
(define-prefix-command 'meta-i-map)
;; (global-unset-key (kbd "M-o"))
;; (global-unset-key (kbd "M-i"))
;; (define-key qjp-mode-map (kbd "M-o") 'meta-o-map)
;; (define-key qjp-mode-map (kbd "M-i") 'meta-i-map)
;; (define-key meta-i-map (kbd "o") #'occur)
;; (define-key meta-i-map (kbd "M-s") #'center-line)
;; (define-key meta-i-map (kbd "M-S") #'center-paragraph)

;; key remapping
(defmacro qjp-push-event (key-str)
  `(lambda () (interactive)
     (setq unread-command-events
           (listify-key-sequence (kbd ,key-str)))))

(defun qjp-mode--keychord-remap (keychord key-str)
  (key-chord-define-global
   keychord
   (qjp-push-event key-str)))

(defun qjp-mode-define-keychords ()
  (with-eval-after-load 'qjp-misc
    (qjp-key-chord-define qjp-mode-map "bb" #'helm-mini)
    (qjp-key-chord-define qjp-mode-map "xf" #'helm-find-files)
    (qjp-key-chord-define qjp-mode-map "xs" #'save-buffer)
    (qjp-mode--keychord-remap ";s" "C-s")
    (qjp-mode--keychord-remap ";g" "C-g")
    (qjp-mode--keychord-remap ";e" "C-e")
    (qjp-mode--keychord-remap ";a" "C-a")
    (qjp-mode--keychord-remap ";h" "C-/")
    (qjp-mode--keychord-remap ";y" "C-y")
    (qjp-mode--keychord-remap ";w" "C-w")
    (qjp-mode--keychord-remap ";1" "C-x 1")
    (qjp-mode--keychord-remap ";2" "C-x 2")
    (qjp-mode--keychord-remap ";3" "C-x 3")
    (qjp-mode--keychord-remap ";o" "C-x o")
    (qjp-mode--keychord-remap ";;" ";")))

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
  (qjp-mode +1))

(defun qjp-mode-off ()
  "Turn off `qjp-mode'."
  (interactive)
  (qjp-mode-translate-keys nil)
  ;; Bindings `qjp-defuns-isearch'
  (qjp-mode-define-isearch nil)
  (qjp-mode -1))

(global-qjp-mode +1)

(provide 'qjp-mode)
;;; qjp-mode.el ends here
