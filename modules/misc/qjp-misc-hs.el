;;; qjp-misc-hs.el --- Settings for hideshow related packages

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

;; --------------------- ;;
;; hideshow, hideshowvis ;;
;; --------------------- ;;

;; I don't need hideshow-org.el any more with this one
(global-set-key [(control tab)] 'hs-toggle-hiding)

;; Expand when goto-line
(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a
collapsed buffer"
  (save-excursion
    (hs-show-block)))

;; Displaying overlay content in echo area or tooltip
(defun qjp-hideshow-overlay-enhanced ()
  "Defines the things necessary to get a + symbol in the fringe
and a yellow marker indicating the number of hidden lines at the
end of the line for hidden regions, also the hidden content. I
hacked this by combining the functions of hideshowvis-symbol and
EmacsWiki's content about hideshow-mode. <2012-10-25 Thu>"
  (interactive)
  (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)

  (defface hs-fringe-face
    '((t (:foreground
          "#888"
          :box
          (:line-width 2 :color "grey75" :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)

  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
             (marker-length (length marker-string))
             (display-string (format "(%d)..." (count-lines
                                                (overlay-start ov)
                                                (overlay-end ov)))))
        (overlay-put ov 'help-echo (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))
        (put-text-property 0 marker-length 'display
                           (list 'left-fringe 'hs-marker 'hs-fringe-face)
                           marker-string)
        (overlay-put ov 'before-string marker-string)
        (overlay-put ov 'display display-string))))
  (setq hs-set-up-overlay 'display-code-line-counts))

(qjp-hideshow-overlay-enhanced)

(provide 'qjp-misc-hs)
;;; qjp-misc-hs.el ends here
