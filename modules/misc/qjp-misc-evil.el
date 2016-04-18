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

;; ------------ ;;
;; evil plugins ;;
;; ------------ ;;

;; evil surround
(global-evil-surround-mode +1)
;; visual star
(global-evil-visualstar-mode +1)
(setq evil-visualstar/persistent t)
;; indent object
(evil-indent-plus-default-bindings)
;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-normal-state-map "K" 'evil-jump-out-args)
;; evil exchange
(evil-exchange-install)
;; evil nerd commenter
(define-key evil-normal-state-map "gc" 'evilnc-comment-or-uncomment-lines)

;; ------------------------------------- ;;
;; insert mode to use emacs key bindings ;;
;; ------------------------------------- ;;
(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
;; map "jj" to "ESC"
(qjp-key-chord-define evil-insert-state-map "jj" 'evil-normal-state);

;; ------------------------- ;;
;; key chords in insert-mode ;;
;; ------------------------- ;;
(qjp-key-chord-define evil-insert-state-map "bb" #'helm-mini)
(qjp-key-chord-define evil-insert-state-map "xf" #'helm-find-files)
(qjp-key-chord-define evil-insert-state-map "xs" #'save-buffer)
;; avy in insert-mode
(qjp-key-chord-define evil-insert-state-map "jk" #'avy-goto-word-1)
(qjp-key-chord-define evil-insert-state-map "jl" #'avy-goto-word-0-in-line)
(qjp-key-chord-define evil-insert-state-map "j;" #'avy-goto-char)

;; ----------------------- ;;
;; Make it more emacs-like ;;
;; ----------------------- ;;
(setq evil-cross-lines t
      evil-move-beyond-eol t
      evil-move-cursor-back nil)

;; show-paren-function in emacs way
(ad-deactivate 'show-paren-function)

;; ---------------- ;;
;; `evil-god-state' ;;
;; ---------------- ;;
;; universal key binding
(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "M-m") 'evil-execute-in-god-state))
;; evil mode key bindings
(define-key evil-normal-state-map (kbd "SPC") 'evil-execute-in-god-state)
(define-key evil-visual-state-map (kbd "SPC") 'evil-execute-in-god-state)
(qjp-key-chord-define evil-insert-state-map "kk" 'evil-execute-in-god-state)
(with-eval-after-load 'evil-god-state
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
    (evil-echo "Switched to God state for the next command ...")))
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

;; ------------- ;;
;; Misc settings ;;
;; ------------- ;;

;; normal mode
(define-key evil-normal-state-map [down-mouse-1] 'qjp-nop)
(define-key evil-normal-state-map "H" #'evil-first-non-blank)
(define-key evil-motion-state-map "H" #'evil-first-non-blank)
(define-key evil-normal-state-map "L" #'evil-end-of-line)
(define-key evil-motion-state-map "L" #'evil-end-of-line)
(define-key evil-normal-state-map "," ctrl-c-extension-map)
;; visual mode
(setq evil-visual-state-cursor 'hbar)
;; emacs state
(define-key evil-emacs-state-map (kbd "<escape>") #'god-local-mode)
(setq evil-emacs-state-cursor 'bar)

(provide 'qjp-misc-evil)
;;; qjp-misc-evil.el ends here
