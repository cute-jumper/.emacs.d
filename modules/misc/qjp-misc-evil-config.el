;;; qjp-misc-evil-config.el --- Settings for evil-mode

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

;; Hope this to be awesome:)
(require 'evil)
(evil-mode 1)

;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil) 

;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; map "jj" to "ESC"
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; map "c-e" to end-of-line because it's kind of more convenient than $
(define-key evil-normal-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-f" 'forward-char)
(define-key evil-normal-state-map "\C-b" 'backward-char)
(define-key evil-normal-state-map [down-mouse-1] 'qjp-NOP)

(provide 'qjp-misc-evil-config)
;;; qjp-misc-evil-config.el ends here
