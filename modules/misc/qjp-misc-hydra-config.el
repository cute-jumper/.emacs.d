;;; qjp-misc-hydra-config.el --- Hydra configuration  -*- lexical-binding: t; -*-

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

(setq hydra-is-helpful)

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
  ("J" qjp-fast-forward-lines)
  ("k" previous-line)
  ("K" qjp-fast-backward-lines)
  ("n" (scroll-up-command))
  ("p" (scroll-up-command '-))
  ("f" forward-word)
  ("b" backward-word)
  ("x" delete-char)
  ("a" qjp-back-to-indentation-or-beginning)
  ("e" end-of-line)
  ("d" whole-line-or-region-kill-region)
  ("D" kill-rectangle)
  ("w" whole-line-or-region-copy-region-as-kill)
  ("W" copy-rectangle-as-kill)
  ("c" qjp-duplicate-line-or-region)
  ("s" (progn (exchange-point-and-mark) (activate-mark)))
  ("o" qjp-open-new-line)
  ("O" (qjp-open-new-line 1))
  ("y" yank)
  ("Y" yank-pop)
  ("u" undo)
  ("v" set-mark-command)
  ("V" rectangle-mark-mode)
  ("=" er/expand-region)
  ;; Must call `isearch-forward' non-interactively
  ("/" (progn
         (isearch-forward)))
  ("i" nil "quit")
  ("q" nil "quit"))

(with-eval-after-load 'key-chord
  (key-chord-define-global "jj" #'hydra-vi/body))

;; This reserves the behavior for M-g M-g
(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
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

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
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
  ("s" string-rectangle nil)
  ("o" open-rectangle nil)
  ("q" nil nil))

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

;; ---------------------- ;;
;; Hydra for evil numbers ;;
;; ---------------------- ;;
;; (maybe-require 'evil-numbers)
(defhydra hydra-number
  (global-map "C-c")
  "hydra increase/decrease number"
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt))

(provide 'qjp-misc-hydra-config)
;;; qjp-misc-hydra-config.el ends here
