;;; qjp-misc-evil.el --- Settings for evil-mode

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

;;

;;; Code:

;; --------------- ;;
;; The evil beast! ;;
;; --------------- ;;

;; Hope this to be awesome:)

(evil-mode +1)
;; unset all insert mode mappings
(setcdr evil-insert-state-map nil)
;; ------------ ;;
;; evil plugins ;;
;; ------------ ;;

;; evil surround
(global-evil-surround-mode +1)

;; evil-paredit
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-change . change))
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-delete . delete))

;; visual star
(global-evil-visualstar-mode +1)
(setq evil-visualstar/persistent t)

;; indent object
(define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
(define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
(define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
(define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
(define-key evil-inner-text-objects-map "j" 'evil-indent-plus-i-indent-up-down)
(define-key evil-outer-text-objects-map "j" 'evil-indent-plus-a-indent-up-down)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; evil exchange
(evil-exchange-install)

;; evil nerd commenter
(autoload 'evilnc-comment-operator "evil-nerd-commenter-operator" nil t)
(with-eval-after-load 'evil-nerd-commenter-operator
  (require 'evil-nerd-commenter))
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)

;; ----------------------- ;;
;; Make it more emacs-like ;;
;; ----------------------- ;;
(setq evil-cross-lines t
      evil-move-beyond-eol t
      evil-move-cursor-back nil)
(setq-default evil-symbol-word-search t)

;; show-paren-function in emacs way
(defun qjp-evil-disable-show-paren-advice (&rest args)
  (ad-disable-advice 'show-paren-function 'around 'evil)
  (ad-activate 'show-paren-function))
(advice-add 'evil-mode :after #'qjp-evil-disable-show-paren-advice)
(qjp-evil-disable-show-paren-advice)

;; ---------------- ;;
;; `evil-god-state' ;;
;; ---------------- ;;
;; universal key binding
(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "M-SPC") 'evil-execute-in-god-state))
;; evil mode key bindings
(define-key evil-normal-state-map (kbd "SPC") 'evil-execute-in-god-state)
(define-key evil-visual-state-map (kbd "SPC") 'evil-execute-in-god-state)
(define-key evil-motion-state-map (kbd "SPC") 'evil-execute-in-god-state)
(qjp-key-chord-define evil-insert-state-map "kk" 'evil-execute-in-god-state)
(with-eval-after-load 'evil-god-state
  (defun evil-god-fix-last-command ()
    "Change `last-command' to be the command before `evil-execute-in-god-state'."
    (setq last-command evil-god-last-command)
    (setq last-repeatable-command last-command))
  (define-key evil-god-state-map [escape] 'evil-god-state-bail)
  ;; fix visual mode problem in evil-god-state
  (defun evil-execute-in-god-state ()
    "Execute the next command in God state."
    (interactive)
    (add-hook 'pre-command-hook #'evil-god-fix-last-command t)
    (add-hook 'post-command-hook #'evil-stop-execute-in-god-state t)
    (setq evil-execute-in-god-state-buffer (current-buffer))
    (setq evil-god-last-command last-command)
    (cond
     ((evil-visual-state-p)
      (let ((mrk (mark))
            (pnt (point)))
        (evil-god-state)
        (set-mark mrk)
        (goto-char (1- pnt))))
     (t
      (evil-god-state)))
    (evil-echo "Switched to God state for the next command ..."))
  (setq evil-god-state-tag
        (format " %s " (propertize "<G>" 'face '((:background "white" :foreground "black"))))))

(autoload 'evil-god-state-p "evil-god-state")
(defun evil-visual-activate-hook (&optional command)
  "Enable Visual state if the region is activated."
  (unless (evil-visual-state-p)
    (evil-delay nil
        ;; the activation may only be momentary, so re-check
        ;; in `post-command-hook' before entering Visual state
        '(unless (or (evil-visual-state-p)
                     (evil-insert-state-p)
                     (evil-emacs-state-p)
                     (evil-god-state-p))
           (when (and (region-active-p)
                      (not deactivate-mark))
             (evil-visual-state)))
      'post-command-hook nil t
      "evil-activate-visual-state")))

;; ---------------------------------- ;;
;; `god-mode' evil cursor integration ;;
;; ---------------------------------- ;;
(defun qjp-emacs-state-god-mode-cursor ()
  (make-local-variable 'evil-emacs-state-cursor)
  (setq evil-emacs-state-cursor 'hollow))
(defun qjp-emacs-state-restore-cursor ()
  (setq evil-emacs-state-cursor 'bar))
(with-eval-after-load 'god-mode
  (add-hook 'god-mode-enabled-hook #'qjp-emacs-state-god-mode-cursor)
  (add-hook 'god-mode-disabled-hook #'qjp-emacs-state-restore-cursor))

;; -------------------------------- ;;
;; Let evil respect `subword-mode'. ;;
;; From spacemacs.                  ;;
;; -------------------------------- ;;
(unless (category-docstring ?U)
  (define-category ?U "Uppercase")
  (define-category ?u "Lowercase"))
(modify-category-entry (cons ?A ?Z) ?U)
(modify-category-entry (cons ?a ?z) ?u)
(make-variable-buffer-local 'evil-cjk-word-separating-categories)
(defun qjp-evil-subword-mode-hook ()
  (if subword-mode
      (push '(?u . ?U) evil-cjk-word-separating-categories)
    (setq evil-cjk-word-separating-categories
          (default-value 'evil-cjk-word-separating-categories))))
(add-hook 'subword-mode-hook #'qjp-evil-subword-mode-hook)

;; --------------------------------- ;;
;; fix for edebug and macrostep-mode ;;
;; --------------------------------- ;;
(add-hook 'edebug-mode-hook #'evil-normalize-keymaps)
(defun qjp-macrostep-setup ()
  (evil-make-overriding-map macrostep-keymap 'normal)
  (evil-normalize-keymaps))
(with-eval-after-load 'macrostep
  (add-hook 'macrostep-mode-hook #'qjp-macrostep-setup))

;; ---------------------------- ;;
;; multiple cursors integration ;;
;; ---------------------------- ;;
(defun qjp-evil-mc-enabled-hook ()
  (and (bound-and-true-p evil-mode)
       (not (evil-emacs-state-p))
       (evil-emacs-state)))
(add-hook 'multiple-cursors-mode-enabled-hook 'qjp-evil-mc-enabled-hook)
(add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state)
(defun qjp-evil-maybe-disable-mc ()
  (when (bound-and-true-p multiple-cursors-mode)
    (multiple-cursors-mode -1)))
(add-hook 'evil-emacs-state-exit-hook #'qjp-evil-maybe-disable-mc)
;; use Ctrl version to mark
(define-key evil-visual-state-map (kbd "C-a") #'mc/mark-all-like-this)
(define-key evil-visual-state-map (kbd "C-p") #'mc/mark-previous-like-this)
(define-key evil-visual-state-map (kbd "C-n") #'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "C-e") #'mc/edit-lines)
(define-key evil-visual-state-map (kbd "C-s") #'mc/mark-all-symbols-like-this)
(define-key evil-visual-state-map (kbd "C-S") #'mc/mark-all-symbols-like-this-in-defun)
(define-key evil-visual-state-map (kbd "C-d") #'mc/mark-all-like-this-dwim)

;; ----------------- ;;
;; magit integration ;;
;; ----------------- ;;
(add-hook 'with-editor-mode-hook #'evil-insert-state)

(add-hook 'view-mode-hook 'evil-motion-state)

;; ---- ;;
;; helm ;;
;; ---- ;;
;; From Spacemacs
(defun qjp-helm-window-hide-cursor ()
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(add-hook 'helm-after-initialize-hook #'qjp-helm-window-hide-cursor)

;; ------------------------------ ;;
;; pp-eval buffer in insert state ;;
;; ------------------------------ ;;
(defadvice pp-display-expression (after qjp-pp-eval-make-emacs-state (expression out-buffer-name) activate)
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (evil-insert-state))))

;; ----------------------- ;;
;; haskell-mode interation ;;
;; ----------------------- ;;
(add-to-list 'evil-motion-state-modes 'haskell-error-mode)

;; ------------- ;;
;; Misc settings ;;
;; ------------- ;;

;; mode line face for different states
(setq evil-normal-state-tag
      (format " %s " (propertize "<N>" 'face '((:background "green" :foreground "black"))))
      evil-emacs-state-tag
      (format " %s " (propertize "<E>" 'face '((:background "red" :foreground "white"))))
      evil-insert-state-tag
      (format " %s " (propertize "<I>" 'face '((:background "cyan" :foreground "black"))))
      evil-motion-state-tag
      (format " %s " (propertize "<M>" 'face '((:background "blue"))))
      evil-visual-state-tag
      (format " %s " (propertize "<V>" 'face '((:background "grey80" :foreground "black"))))
      evil-operator-state-tag
      (format " %s " (propertize "<O>" 'face '((:background "purple")))))

;; ------------------------------------- ;;
;; insert mode to use emacs key bindings ;;
;; ------------------------------------- ;;
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
;; and `C-z' goes to emacs state
(define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
;; map "jj" to "ESC"
(qjp-key-chord-define evil-insert-state-map "jj" 'evil-normal-state);

;; ------------------------------- ;;
;; key chords in insert/emacs mode ;;
;; ------------------------------- ;;
(dolist (keymap (list evil-insert-state-map evil-emacs-state-map))
  (qjp-key-chord-define keymap "bb" #'helm-mini)
  (qjp-key-chord-define keymap "xf" #'helm-find-files)
  (qjp-key-chord-define keymap "xs" #'save-buffer)
  ;; avy in insert-mode
  (qjp-key-chord-define keymap "jk" #'avy-goto-word-1)
  (qjp-key-chord-define keymap "jl" #'avy-goto-word-0-in-line)
  (qjp-key-chord-define keymap "j;" #'avy-goto-char))

;; ------ ;;
;; normal ;;
;; ------ ;;
(define-key evil-normal-state-map [down-mouse-1] 'qjp-nop)
(define-key evil-normal-state-map "H" #'evil-first-non-blank)
(define-key evil-normal-state-map "L" #'evil-end-of-line)
(define-key evil-normal-state-map "," #'repeat)
(define-key evil-normal-state-map (kbd "M-.") nil)
;; avoid ctrl
(define-key evil-normal-state-map "gz" #'evil-emacs-state)
;; Y yank to end of line
(evil-add-command-properties 'evil-yank-line :motion 'evil-end-of-line)

;; ------ ;;
;; visual ;;
;; ------ ;;
(setq evil-visual-state-cursor 'hbar)
(define-key evil-visual-state-map "," #'repeat)
;; avoid ctrl
(defun qjp-evil-emacs-state ()
  "Use wrapper command. I don't know why."
  (interactive)
  (evil-emacs-state))
(define-key evil-visual-state-map "gz" #'qjp-evil-emacs-state)

;; ------ ;;
;; motion ;;
;; ------ ;;
(define-key evil-motion-state-map "H" #'evil-first-non-blank)
(define-key evil-motion-state-map "L" #'evil-end-of-line)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map (kbd "TAB") nil)
;; unbind C-e/C-y
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)

;; ----- ;;
;; emacs ;;
;; ----- ;;
;; more consistent feel
(define-key evil-emacs-state-map (kbd "<escape>") #'evil-exit-emacs-state)
(qjp-key-chord-define evil-emacs-state-map "jj" #'evil-exit-emacs-state)
;; M-SPC and kk make no sense to only execute god once in emacs state, so
(define-key evil-emacs-state-map (kbd "M-SPC") #'god-local-mode)
(qjp-key-chord-define evil-emacs-state-map "kk" #'god-local-mode)
(add-to-list 'evil-emacs-state-modes 'dired-mode)
(setq evil-emacs-state-cursor 'bar)
;; fix visual/emacs state transition
(defun qjp-evil-emacs-state-entry-hook-from-visual ()
  (when (eq evil-previous-state 'visual)
    (let ((mk (mark))
          (pt (point)))
      (if (> mk pt)
          (progn
            (set-mark mk)
            (goto-char pt))
        (set-mark mk)
        (goto-char pt)))))
(add-hook 'evil-emacs-state-entry-hook
          #'qjp-evil-emacs-state-entry-hook-from-visual)
(defun qjp-evil-emacs-state-exit-hook ()
  (and (use-region-p)
       (> (point) (mark))
       (backward-char)))
(add-hook 'evil-emacs-state-exit-hook #'qjp-evil-emacs-state-exit-hook)

(provide 'qjp-misc-evil)
;;; qjp-misc-evil.el ends here
