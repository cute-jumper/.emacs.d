;;; qjp-misc-flycheck.el --- Settings for flycheck   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords:

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

(with-eval-after-load 'flycheck
  (define-fringe-bitmap 'qjp-flycheck-error-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b01100110
            #b01111110
            #b00111100
            #b00011000
            #b00111100
            #b01111110
            #b01100110
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (define-fringe-bitmap 'qjp-flycheck-info-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'qjp-flycheck-error-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'exclamation-mark
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'qjp-flycheck-info-indicator
    :fringe-face 'flycheck-fringe-info)

  ;; use line for warning & info
  (set-face-underline
   'flycheck-warning
   `(:color
     ,(plist-get (face-attribute 'flycheck-warning :underline) :color)))

  (set-face-underline
   'flycheck-info
   `(:color
     ,(plist-get (face-attribute 'flycheck-info :underline) :color)))

  ;; key bindings
  (define-key qjp-mode-map (kbd "M-N") #'flycheck-next-error)
  (define-key qjp-mode-map (kbd "M-P") #'flycheck-previous-error)

  ;; flycheck in pos-tip
  (flycheck-pos-tip-mode +1))

(provide 'qjp-misc-flycheck)
;;; qjp-misc-flycheck.el ends here
