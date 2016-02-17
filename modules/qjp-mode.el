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

(defvar qjp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Bindings for `qjp-defuns-edit'
    (define-key map (kbd "C-h") #'backward-delete-char)
    (define-key map (kbd "M-s r") #'replace-string)
    (define-key map [remap move-beginning-of-line]
      #'qjp-back-to-indentation-or-beginning)
    (define-key map (kbd "M-;") #'qjp-comment-dwim-line)
    (define-key map (kbd "C-o") #'qjp-open-new-line)
    (define-key map [S-up] #'qjp-move-line-up)
    (define-key map [S-down] #'qjp-move-line-down)
    ;; Useless since rebinding "M-m"
    ;; (define-key map (kbd "M-m") #'qjp-kill-back-to-indentation)
    ;; (define-key map (kbd "C-j") #'qjp-join-next-line)
    ;; (define-key map (kbd "M-j") #'next-line)
    ;; (define-key map (kbd "M-k") #'previous-line)
    ;; (define-key map (kbd "M-h") #'backward-char)
    ;; (define-key map (kbd "M-l") #'forward-char)

    ;; ------------------ ;;
    ;; Other key bindings ;;
    ;; ------------------ ;;
    (define-key map [remap list-buffers] #'ibuffer)
    ;; (define-key map (kbd "C-x C-r") #'revert-buffer)
    (define-key map [remap just-one-space] #'cycle-spacing)
    (define-key map [remap count-words-region] #'count-words)
    (define-key map (kbd "M-/") #'hippie-expand)
    (define-key map (kbd "C-z") #'qjp-switch-to-scratch-or-back)

    ;; Mouse clicks are annoying. Globally set mouse 1's single-click events to
    ;; `qjp-nop'. Navigating using doulbe-click is still available.
    (define-key map [down-mouse-1] nil)
    (define-key map [mouse-1] #'qjp-nop)
    (define-key map (kbd "<C-mouse-1>") #'qjp-nop)
    (define-key map (kbd "<C-down-mouse-1>") #'qjp-nop)
    map))

(defmacro qjp-meta-translate (map fake-prefix-key real-prefix-key keys &optional doc)
  "Easily translate KEYS and add `which-key' DOC."
  (let ((new-keys (replace-regexp-in-string "C-" "M-" keys)))
    `(progn
       (define-key ,map (kbd ,new-keys)
         (µ (setq unread-command-events (listify-key-sequence (kbd (concat ,real-prefix-key " " ,keys))))))
       (and ,doc
            (which-key-add-key-based-replacements (concat ,fake-prefix-key " " ,new-keys) ,doc)))))

(define-key key-translation-map (kbd "ESC") (kbd "C-c"))
(define-key key-translation-map (kbd "C-c") (kbd "ESC"))
;; --------------------- ;;
;; Make M-m a prefix-map ;;
;; --------------------- ;;
(define-prefix-command 'meta-m-map)
(define-key qjp-mode-map (kbd "M-m") 'meta-m-map)
(defmacro qjp-meta-m-translate (keys &optional doc)
  "Easily translate KEYS and add `which-key' DOC to meta-m."
  `(qjp-meta-translate meta-m-map "M-m" "C-x" ,keys ,doc))
(defmacro qjp-meta-m-remap (keys)
  "Easily remap KEYS to meta-m."
  (let ((new-keys (replace-regexp-in-string "C-" "M-" keys)))
    `(define-key meta-m-map (kbd ,new-keys)
       (key-binding (kbd (concat "C-x " ,keys))))))
;; -------------------------- ;;
;; bind standard C-x commands ;;
;; -------------------------- ;;
(qjp-meta-m-translate "C-f" "find files")
(qjp-meta-m-translate "b" "switch buffers")
(qjp-meta-m-translate "r" "rectangle & registers")
(qjp-meta-m-translate "8" "unicode")
(qjp-meta-m-translate "n")
(qjp-meta-m-remap "k")
(qjp-meta-m-remap "o")
(qjp-meta-m-remap "d")
(qjp-meta-m-remap "0")
(qjp-meta-m-remap "1")
(qjp-meta-m-remap "2")
(qjp-meta-m-remap "3")
(qjp-meta-m-remap "C-q")
(qjp-meta-m-remap "C-s")
(qjp-meta-m-remap "C-e")
;; -------------------- ;;
;; bind custom commands ;;
;; -------------------- ;;
(define-key meta-m-map (kbd "M-r") #'revert-buffer)
(define-key meta-m-map (kbd "O") (µ (other-window -1)))
(define-key meta-m-map (kbd "M-m") #'exchange-point-and-mark)
(define-key meta-m-map "H" #'mark-whole-buffer)
;;; prefix for editting functions
(define-prefix-command 'meta-m-funcs-map)
(define-key meta-m-map "f" 'meta-m-funcs-map)
(define-key meta-m-funcs-map "d" #'qjp-duplicate-line-or-region)
(define-key meta-m-funcs-map "e" #'qjp-eval-and-replace)
(define-key meta-m-funcs-map "=" #'qjp-calc-eval-and-replace)
(define-key meta-m-funcs-map "s" #'qjp-isearch-other-window)
;; --------------------- ;;
;; Make M-j a prefix-map ;;
;; --------------------- ;;
;; (define-prefix-command 'meta-j-map)
;; (define-key qjp-mode-map (kbd "M-j") 'meta-j-map)
;; (defmacro qjp-meta-j-translate (keys &optional doc)
;;   "Easily translate KEYS and add `which-key' DOC to meta-m."
;;   `(qjp-meta-translate meta-j-map "M-j" "C-c" ,keys ,doc))
;; (qjp-meta-j-translate "C-s")
;; (qjp-meta-j-translate "C-e")
;; (qjp-meta-j-translate "C-r")

;; `M-o' prefix keymap
(define-prefix-command 'meta-i-map)
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-i"))
(define-key qjp-mode-map (kbd "M-o") 'meta-o-map)
(define-key qjp-mode-map (kbd "M-i") 'meta-i-map)
(define-key meta-i-map (kbd "o") #'occur)
(define-key meta-i-map (kbd "M-s") #'center-line)
(define-key meta-i-map (kbd "M-S") #'center-paragraph)

;; key translation
(defun qjp--key-translate-swap (k1 k2)
  (define-key key-translation-map (kbd k1) (kbd k2))
  (define-key key-translation-map (kbd k2) (kbd k1)))

;; (define-key key-translation-map (kbd "M-h") (kbd "M-h"))

;; key remapping
(defmacro qjp-push-event (key-str)
  `(lambda () (interactive)
     (setq unread-command-events
           (listify-key-sequence (kbd ,key-str)))))

(defun qjp-mode--keychord-remap (keychord key-str)
  (key-chord-define-global
   keychord
   (qjp-push-event key-str)))

(defmacro qjp-define-key-combos (prefix-key keymap combo-alist)
  `(progn
     (define-prefix-command ',keymap)
     (define-key qjp-mode-map ,prefix-key ,keymap)
     (dolist (combo ,combo-alist)
       (define-key ,keymap (car combo) (qjp-push-event (cdr combo))))
     (define-key ,keymap [t] #'self-insert-command)))

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
  ;; key-chord-mode
  (with-eval-after-load 'qjp-misc
    (qjp-key-chord-define qjp-mode-map "bb" #'helm-mini)
    (qjp-key-chord-define qjp-mode-map "xf" #'helm-find-files)
    (qjp-key-chord-define qjp-mode-map "xs" #'save-buffer)
    ;;(qjp-key-chord-define qjp-mode-map "bn" #')
    (qjp-mode--keychord-remap ";x" "C-x")
    (qjp-mode--keychord-remap ";c" "C-c")
    (qjp-mode--keychord-remap ";s" "C-s")
    (qjp-mode--keychord-remap ";g" "C-g")
    (qjp-mode--keychord-remap ";e" "C-e")
    (qjp-mode--keychord-remap ";a" "C-a")
    (qjp-mode--keychord-remap ";f" "C-f")
    (qjp-mode--keychord-remap ";b" "C-b")
    (qjp-mode--keychord-remap ";k" "C-k")
    (qjp-mode--keychord-remap ";u" "C-/")
    (qjp-mode--keychord-remap ";y" "C-y")
    (qjp-mode--keychord-remap ";w" "C-w")
    (qjp-mode--keychord-remap ";1" "C-x 1")
    (qjp-mode--keychord-remap ";2" "C-x 2")
    (qjp-mode--keychord-remap ";3" "C-x 3")
    (qjp-mode--keychord-remap ";0" "C-x 0")
    (qjp-mode--keychord-remap ";o" "C-x o")
    (qjp-mode--keychord-remap ";;" ";")
    ;; (qjp-define-key-combos ";" semi-colon-map '(("x" . "C-x")
    ;;                                             ("c" . "C-c")
    ;;                                             ("s" . "C-s")))
    )

  ;; Bindings for `isearch'
  (define-key isearch-mode-map (kbd "C-p") #'isearch-yank-x-selection)
  (define-key isearch-mode-map (kbd "C-o") #'qjp-isearch-occur)
  (define-key isearch-mode-map (kbd "C-k") #'qjp-kill-isearch-match)
  (define-key isearch-mode-map (kbd "M-z") #'qjp-zap-to-isearch)
  (define-key isearch-mode-map [(control return)] #'qjp-isearch-exit-other-end)
  (qjp-mode +1))

(defun qjp-mode-off ()
  "Turn off `qjp-mode'."
  (interactive)
  ;; Bindings `qjp-defuns-isearch'
  (define-key isearch-mode-map (kbd "C-p") nil)
  (define-key isearch-mode-map (kbd "C-o") nil)
  (define-key isearch-mode-map (kbd "C-k") nil)
  (define-key isearch-mode-map (kbd "M-z") nil)
  (define-key isearch-mode-map [(control return)] nil)
  (qjp-mode -1))

(global-qjp-mode +1)

(provide 'qjp-mode)
;;; qjp-mode.el ends here
