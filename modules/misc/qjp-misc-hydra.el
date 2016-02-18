;;; qjp-misc-hydra.el --- Hydra configuration  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(setq hydra-is-helpful nil)

;; Replace keychord with hydra
(defmacro qjp-define-keychord-with-hydra (k1 k2 cmd)
  (let* ((hydra-name (intern (format "%s-%s-%S" k1 k2 (cadr cmd))))
         (hydra-pre-name (intern (format "%S-pre" hydra-name)))
         (hydra-body-name (intern (format "%S/body" hydra-name))))
    `(progn
       (defun ,hydra-pre-name ()
         (insert ,k1)
         (hydra-timeout 0.5))
       (defhydra ,hydra-name (:body-pre ,hydra-pre-name
                                        :color blue
                                        :hint nil)
         (,k2 (progn (zap-to-char -1 (string-to-char,k1))
                     (call-interactively ,cmd))))
       (global-set-key ,k1 #',hydra-body-name))))

;; ------------------------ ;;
;; My own vi-style bindings ;;
;; ------------------------ ;;
(defhydra hydra-vi
  (:body-pre (progn
               (set-cursor-color "#dfe030")
               (setq cursor-type 'box)
               (message "Enter hydra-vi"))
             :post (progn
                     (set-cursor-color "#ffffff")
                     (setq cursor-type 'bar)
                     (message "Leave hydra-vi")))
  "vi"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("J" join-line)
  ("k" previous-line)
  ("K" kill-buffer)
  ("n" (scroll-up-command))
  ("p" (scroll-up-command '-))
  ("f" forward-word)
  ("b" backward-word)
  ("x" delete-char)
  ("a" qjp-back-to-indentation-or-beginning)
  ("A" paredit-backward)
  ("e" end-of-line)
  ("E" (paredit-forward))
  ("d" whole-line-or-region-kill-region)
  ("D" kill-rectangle)
  ("w" whole-line-or-region-copy-region-as-kill)
  ("W" copy-rectangle-as-kill)
  ("c" qjp-duplicate-line-or-region)
  ("s" (progn (exchange-point-and-mark) (activate-mark)))
  ("o" qjp-open-new-line)
  ("O" (qjp-open-new-line 1))
  ("y" yank)
  ("Y" helm-show-kill-ring)
  ("u" undo)
  ("v" set-mark-command)
  ("V" rectangle-mark-mode)
  ("=" er/expand-region)
  ;; Must call `isearch-forward' non-interactively
  ("/" (progn
         (isearch-forward)))
  ("g" avy-goto-word-1)
  ("G" avy-goto-char-in-line)
  ("M-g" avy-goto-line)
  ("SPC f" helm-find-files)
  ("SPC b" (progn (hydra-keyboard-quit)
                  (helm-mini)))
  ("SPC s" save-buffer)
  ("i" nil "quit")
  ("q" nil "quit"))

(with-eval-after-load 'qjp-mode
  ;;(qjp-define-keychord-with-hydra "j" "i" #'hydra-vi/body)
  (qjp-key-chord-define qjp-mode-map "jj" #'hydra-vi/body))

;; This reserves the behavior for M-g M-g
(defhydra hydra-goto-line (goto-map ""
                                    :pre (nlinum-mode 1)
                                    :post (nlinum-mode -1))
  "goto-line/goto-error"
  ("g" avy-goto-line "go")
  ("SPC" set-mark-command "mark" :bind nil)
  ("n" next-error)
  ("p" previous-error)
  ("q" nil "quit"))

;; ----------------------------- ;;
;; Hydra for rectangle operation ;;
;; ----------------------------- ;;
(defun ora-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defhydra hydra-rectangle (:body-pre (progn
                                       (rectangle-mark-mode 1)
                                       (setq hydra-is-helpful t))
                                     :color pink
                                     :post
                                     (progn (deactivate-mark)
                                            (setq hydra-is-helpful nil)))
  "
  ^_k_^     _d_elete    s_t_ring
_h_   _l_   _o_pen      _y_ank
  ^_j_^     _r_eset     _u_ndo
^^^^        _e_xchange  _q_uit
^^^^        _c_opy
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" ora-ex-point-mark nil)
  ("c" copy-rectangle-as-kill nil)
  ("d" kill-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("t" string-rectangle nil)
  ("o" open-rectangle nil)
  ("q" nil nil))

(with-eval-after-load 'qjp-mode
  (define-key ctrl-c-extension-map (kbd "SPC") 'hydra-rectangle/body))

;; ---------------------- ;;
;; Hydra for evil numbers ;;
;; ---------------------- ;;
(defhydra hydra-number
  (qjp-mode-map "C-c")
  "hydra increase/decrease number"
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt))

;; -------------------------- ;;
;; Hydra for goto-last-change ;;
;; -------------------------- ;;
(defhydra hydra-goto-last-change
  (qjp-mode-map "C-c")
  "hydra goto previous/next change"
  ("\\" goto-last-change)
  ("|" goto-last-change-reverse))

;; --------------- ;;
;; Hydra for rebox ;;
;; --------------- ;;
(defhydra hydra-rebox
  (qjp-mode-map "C-c")
  "hydra rebox-cycle"
  (";" rebox-cycle))

;; ------------------------ ;;
;; Hydra for `other-window' ;;
;; ------------------------ ;;
(defhydra hydra-manage-window
  (:body-pre (setq hydra-is-helpful t)
             :post (setq hydra-is-helpful))
  "Window"
  ("o" (other-window 1) "next window")
  ("d" delete-window "delete window")
  ("r" delete-other-windows "delete other windows")
  ("h" split-window-horizontally "split horizontally")
  ("v" split-window-vertically "split vertically")
  ("b" helm-mini "switch buffer"))

(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "C-c w") #'hydra-manage-window/body))

(defhydra hydra-winner
  (:body-pre (winner-undo))
  "Hydra winner"
  ("<left>" winner-undo)
  ("<right>" winner-redo))

(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "C-c <left>") #'hydra-winner/body))

(provide 'qjp-misc-hydra)
;;; qjp-misc-hydra.el ends here
