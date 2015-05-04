;;; qjp-programming-ruby.el --- Settings for Ruby

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

;; Ruby
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'ruby-block-mode)
(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key [(return)] 'electrify-return-if-match)
                            (require 'ruby-block)
                            ;; do overlay
                            (setq ruby-block-highlight-toggle 'overlay)
                            (setq ruby-block-highlight-toggle t)))
(add-hook 'enh-ruby-mode-hook 'ruby-end-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-block-mode)
(add-hook 'enh-ruby-mode-hook (lambda ()
                                (local-set-key [(return)] 'electrify-return-if-match)
                                (local-set-key (kbd "C-c {") 'ruby-toggle-block)
                                (require 'ruby-block)
                                ;; do overlay
                                (setq ruby-block-highlight-toggle 'overlay)
                                (setq ruby-block-highlight-toggle t)))

(provide 'qjp-programming-ruby)
;;; qjp-programming-ruby.el ends here
