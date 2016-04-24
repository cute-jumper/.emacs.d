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
  (setq-default TeX-master nil)
  (setq preview-scale-function 2)
  (setq TeX-save-query nil)
  (setq TeX-show-compilation nil)
  (setq TeX-source-correlate-start-server t)
  ;; Add option `-file-line-error' to avoid `TeX-next-error' error
  ;; See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=695282 for details
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex -file-line-error -shell-escape%(mode)%' %t"
                 TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list
               '("pdflatex" "%`pdflatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list
               '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                 :help "Run LaTeXMK on file"))
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk")
  ;; Tricks: let synctex work with Okular
  (add-to-list 'TeX-expand-list '("%(masterdir)" (lambda nil (expand-file-name (TeX-master-directory)))))
  (add-to-list 'TeX-view-program-list '("Okular" "okular --unique %o#src:%n%(masterdir)./%b"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))

  ;; Add hook for pdf-view-mode
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
            #'TeX-revert-document-buffer)

  ;; major mode bindings
  (define-key LaTeX-mode-map (kbd "C-c e e") #'LaTeX-environment)
  (define-key LaTeX-mode-map (kbd "C-c e s") #'LaTeX-section)
  (define-key LaTeX-mode-map (kbd "C-c e ]") #'LaTeX-close-environment)
  (define-key LaTeX-mode-map (kbd "C-c e p") #'qjp-latex-add-usepackage)
  (define-key LaTeX-mode-map (kbd "C-c e m") #'qjp-latex-maketitle)
  (define-key LaTeX-mode-map (kbd "C-c e a") #'latex/compile-commands-until-done)
  (define-key LaTeX-mode-map (kbd "C-c e v") #'TeX-view)
  (define-key LaTeX-mode-map (kbd "C-c e f") #'TeX-font)

  ;; ------ ;;
  ;; reftex ;;
  ;; ------ ;;
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography `(,qjp-bibtex-database-file))

  ;; ---------------------- ;;
  ;; User Defined Functions ;;
  ;; ---------------------- ;;

  (defvar qjp-latex-from-buffer nil)
  (make-variable-buffer-local 'qjp-latex-from-buffer)

  (defun qjp-latex-switch-to-pdf-view ()
    (interactive)
    (let* ((from-buffer (current-buffer))
           (master-file-name (TeX-master-file))
           (pdf-buffer (find-file-noselect
                        (concat master-file-name "." (TeX-output-extension)))))
      (switch-to-buffer pdf-buffer)
      (setq qjp-latex-from-buffer from-buffer)))

  (defun qjp-pdf-view-switch-to-tex-source ()
    (interactive)
    (if qjp-latex-from-buffer
        (switch-to-buffer qjp-latex-from-buffer)
      (let* ((pdf-file-name (buffer-file-name))
             (tex-buffer (find-file-noselect
                          (replace-regexp-in-string "\\.pdf" ".tex" pdf-file-name))))
        (switch-to-buffer tex-buffer))))
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "C-x C-z") #'qjp-pdf-view-switch-to-tex-source))

  ;; Insert \usepackage in the front of the file
  (defun qjp-latex-add-usepackage (pkg-name pkg-options)
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

  ;; ----------------------------- ;;
  ;; Add some frequently used envs ;;
  ;; ----------------------------- ;;
  (defun qjp-tex-add-LaTeX-environments ()
    (LaTeX-add-environments
     '("align" LaTeX-env-label)
     '("align*")
     '("matrix")
     '("bmatrix")
     '("Bmatrix")
     '("pmatrix")
     '("vmatrix")
     '("Vmatrix")
     '("smallmatrix")
     '("cases")
     '("code" "Programming language")
     '("comment")))

  ;; ------------------ ;;
  ;; Local key bindings ;;
  ;; ------------------ ;;
  (defun qjp-tex-set-local-key-bindings ()
    (local-set-key (kbd "C-c ,") #'LaTeX-mark-section)
    (local-set-key (kbd "C-x C-z") #'qjp-latex-switch-to-pdf-view))

  (defun qjp-turn-on-cdlatex ()
    (turn-on-cdlatex)
    ;; Disable cdlatex-pbb to use smartparens
    (define-key cdlatex-mode-map  "(" nil)
    (define-key cdlatex-mode-map  "{" nil)
    (define-key cdlatex-mode-map  "[" nil)
    (define-key cdlatex-mode-map  "|" nil)
    (define-key cdlatex-mode-map  "<" nil))

  ;; -------------------- ;;
  ;; Third-party packages ;;
  ;; -------------------- ;;

  ;; Setup smartparens for latex
  (require 'smartparens-latex)

  ;; Add company-auctex backend
  (with-eval-after-load 'company
    (company-auctex-init))

  ;; Settings for magic-latex-buffer
  (setq magic-latex-enable-block-align nil)

  ;; latex-extra
  (setq latex/view-after-compile nil)
  (setq latex/no-fill-environments '("equation"
                                     "equation*"
                                     "align"
                                     "align*"
                                     "tabular"
                                     "tikzpicture"
                                     "verbatim"
                                     "verbatim*"))
  (with-eval-after-load 'reftex
    (dolist (x '(("t"  . reftex-toc)
                 ("T"  . reftex-toc-recenter)
                 ("l"  . reftex-label)
                 ("r"  . reftex-reference)
                 ("c"  . reftex-citation)
                 ("i"  . reftex-index)
                 ("I"  . reftex-display-index)
                 ("/"  . reftex-index-selection-or-word)
                 ("\\" . reftex-index-phrase-selection-or-word)
                 ("|"  . reftex-index-visit-phrases-buffer)
                 ("x"  . reftex-view-crossref)))
      (define-key reftex-mode-map (kbd (format "C-c e %s" (car x))) (cdr x)))))

;; ----- ;;
;; Hooks ;;
;; ----- ;;
(defun qjp-tex-mode-hook ()
  (qjp-tex-set-local-key-bindings)
  (turn-on-reftex)
  (TeX-source-correlate-mode +1)
  (qjp-turn-on-cdlatex)
  (latex-extra-mode +1)
  (magic-latex-buffer +1)
  (flyspell-mode +1)
  (flycheck-mode +1)
  (smartparens-mode +1)
  (qjp-tex-add-LaTeX-environments))

(add-hook 'LaTeX-mode-hook #'qjp-tex-mode-hook)
(add-hook 'latex-mode-hook #'qjp-tex-mode-hook)

(provide 'qjp-tex)
;;; qjp-tex.el ends here
