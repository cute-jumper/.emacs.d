;;; qjp-org-src.el --- Settings for literature programming support in org-mode

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

;; --------- ;;
;; org babel ;;
;; --------- ;;
;; Basic settings
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (R . t)
   (python . t)
   (ruby . t)
   (haskell . t)
   (gnuplot . t)
   (clojure . t)
   (shell . t)
   (ledger . t)
   (org . t)
   (plantuml . t)
   (octave . t)
   (latex . t)))

(setq org-src-lang-modes
      '(("ocaml" . tuareg)
        ("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . graphviz-dot)
        ("sqlite" . sql)
        ("calc" . fundamental)
        ("C" . c)))

(setq org-ditaa-jar-path "/usr/bin/ditaa") ;; set this only because I install Org-mode manually
(setq org-plantuml-jar-path "/usr/local/bin/plantuml.jar")

;; TODO:Fix some bugs for python. I don't know for sure what cause this!
(add-hook 'org-src-mode-hook (lambda () (remove-hook 'python-mode-hook 'wisent-python-default-setup)))

;; Hightlight Org-mode src block, from
;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html.
;; It must be loaded before Org-mode.
(defface org-block-begin-line
    '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
      "Face used for the line delimiting the begin of source blocks.")
(defface org-block-background
    '((t (:background "#FFFFEA")))
      "Face used for the source block background.")
(defface org-block-end-line
    '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
      "Face used for the line delimiting the end of source blocks.")

(provide 'qjp-org-src)
;;; qjp-org-src.el ends here
