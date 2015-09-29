;;; qjp-tex.el --- Settings for TeX

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
;; AUCTeX settings ;;
;; --------------- ;;
(with-eval-after-load 'latex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil)
  (setq preview-scale-function 2)
  (setq TeX-save-query nil)
  (setq TeX-show-compilation nil)

  ;; Add option `-file-line-error' to avoid `TeX-next-error' error
  ;; See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=695282 for details
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex -file-line-error -shell-escape%(mode)%' %t"
                 TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list
               '("pdflatex" "%`pdflatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list
               '("latexmk" "latexmk -pdf %s && latexmk -c" TeX-run-TeX nil t
                 :help "Run Latexmk on file"))
  ;; Tricks: let synctex work with Okular
  (push '("%(masterdir)" (lambda nil (expand-file-name (TeX-master-directory))))
        TeX-expand-list)
  (push '("Okular" "okular --unique %o#src:%n%(masterdir)./%b")
        TeX-view-program-list)
  (push '(output-pdf "Okular") TeX-view-program-selection))

;; ------------------ ;;
;; Local key bindings ;;
;; ------------------ ;;
(defun qjp-tex-set-local-key-bindings ()
  (local-set-key [(return)] #'newline-and-indent)
  (local-set-key (kbd "C-c ,") #'LaTeX-mark-section))

;; ------ ;;
;; reftex ;;
;; ------ ;;
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography `(,qjp-bibtex-database-file))

;; ---------------------- ;;
;; User Defined Functions ;;
;; ---------------------- ;;

;; Auto update when saving
(defvar qjp-latex-auto-command "latexmk")
(defvar qjp-latex-auto-command-options '("-shell-escape" "-pdf"))
(defun qjp-latex-auto-update ()
  (interactive)
  (when (and (eq major-mode 'latex-mode)
             (string-match "\\.tex$" buffer-file-name))
    (let* ((latex-process-name "latex-auto-compile")
           (latex-buffer-name "*latex-auto-buffer*")
           (latex-process (get-process latex-process-name)))
      (and latex-process
           (delete-process latex-process))
      (when (get-buffer latex-buffer-name)
        (save-excursion
          (set-buffer latex-buffer-name)
          (erase-buffer)))
      (apply #'start-process latex-process-name latex-buffer-name qjp-latex-auto-command
             (append qjp-latex-auto-command-options (list (TeX-master-file)))))))

;; Insert \usepackage in the front of the file
(defun qjp-latex-add-pkg (pkg-name pkg-options)
  (interactive "sPackage: \nsOptions: ")
  (save-match-data
    (save-excursion
      (goto-char (point-max))
      (unless (search-backward "\\usepackage" 0 t)
        (search-backward "\\documentclass" 0 t))
      (end-of-line)
      (newline-and-indent)
      (if (string= pkg-options "")
          (insert "\\usepackage{" pkg-name "}")
        (insert "\\usepackage[" pkg-options "]{" pkg-name "}")))))

;; Facility for \maketitle
(defun qjp-latex-maketitle (title author date)
  (interactive "sTitle: \nsAuthor: \nsDate: ")
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\\begin{document}" 0 t)
        (progn
          (save-excursion
            (previous-line)
            (end-of-line)
            (newline-and-indent)
            (insert "\\title{" title "}\n\\author{" author "}\n\\date{" date "}\n"))
          (end-of-line)
          (newline-and-indent)
          (insert "\\maketitle"))
      (unless (search-backward "\\usepackage" 0 t)
        (search-backward "\\documentclass" 0 t))
      (end-of-line)
      (newline-and-indent)
      (insert "\\title{" title "}\n\\author{" author "}\n\\date{" date "}\n\n\\begin{document}\n\\maketitle"))))

;; ----- ;;
;; Hooks ;;
;; ----- ;;
(defun qjp-tex-mode-hook ()
  (qjp-tex-set-local-key-bindings)
  (turn-on-cdlatex)
  (turn-on-reftex)
  (latex-extra-mode +1)
  (magic-latex-buffer +1)
  (flyspell-mode +1)
  (add-hook 'after-save-hook #'qjp-latex-auto-update nil t))

(add-hook 'LaTeX-mode-hook #'qjp-tex-mode-hook)
(add-hook 'latex-mode-hook #'qjp-tex-mode-hook)

(provide 'qjp-tex)
;;; qjp-tex.el ends here
