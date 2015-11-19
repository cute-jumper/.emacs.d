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

  ;; Setup smartparens for latex
  (require 'smartparens-latex)

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
  (push "\\.fdb_latexmk" LaTeX-clean-intermediate-suffixes)
  ;; Tricks: let synctex work with Okular
  (push '("%(masterdir)" (lambda nil (expand-file-name (TeX-master-directory))))
        TeX-expand-list)
  (push '("Okular" "okular --unique %o#src:%n%(masterdir)./%b")
        TeX-view-program-list)
  (push '(output-pdf "Okular") TeX-view-program-selection)

  ;; ------ ;;
  ;; reftex ;;
  ;; ------ ;;
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography `(,qjp-bibtex-database-file))

  ;; ------------------------ ;;
  ;; Auto compile when saving ;;
  ;; ------------------------ ;;
  (defvar qjp-latex-auto-compile-command "latexmk")
  (defvar qjp-latex-auto-compile-command-options '("-shell-escape" "-pdf"))
  ;;(make-variable-buffer-local 'qjp-latex-auto-compile-command-options)
  (defvar qjp-latex-auto-compile-p t)
  ;;(make-variable-buffer-local 'qjp-latex-auto-compile-p)
  (defvar qjp-latex-auto-compile-buffer-name "*latex-auto-compile-buffer*")
  (defvar qjp-latex-auto-compile-from-buffer nil)
  (defvar qjp-latex-error-buffer-font-lock
    '(("--- .* ---" 0 font-lock-keyword-face)
      ("^l\\.[0-9]+" 0 'underline)
      ("^\\([[:alpha:]]+\\):\\(.*\\)$"
       (1 'compilation-warning) (2 font-lock-constant-face))
      ("^\\(<recently read>\\) \\(.*\\)$"
       (1 'compilation-warning) (2 font-lock-constant-face)))
    "Font lock rules used in \"*latex-auto-compile-buffer*\" buffers.
Steal from latex-extra: https://github.com/Malabarba/latex-extra.")

  (defun qjp-latex--get-or-create-auto-compile-buffer ()
    (let ((latex-buffer-name qjp-latex-auto-compile-buffer-name))
      (or (get-buffer latex-buffer-name)
          (with-current-buffer (get-buffer-create latex-buffer-name)
            (special-mode)
            (setq buffer-read-only)
            (font-lock-add-keywords nil qjp-latex-error-buffer-font-lock)
            (if (fboundp 'font-lock-ensure)
                (font-lock-ensure)
              (with-no-warnings
                (font-lock-fontify-buffer)))
            (local-set-key (kbd "C-x C-z") #'qjp-latex-switch-from-auto-compile-buffer)
            (current-buffer)))))

  (defun qjp-latex-auto-compile ()
    (interactive)
    (and qjp-latex-auto-compile-p
         (eq major-mode 'latex-mode)
         (string-match "\\.tex\\'" buffer-file-name)
         (let* ((latex-process-name "latex-auto-compile")
                (latex-buffer-name qjp-latex-auto-compile-buffer-name)
                (latex-process (get-process latex-process-name)))
           (and latex-process
                (delete-process latex-process))
           (with-current-buffer (qjp-latex--get-or-create-auto-compile-buffer)
             (erase-buffer))
           (let ((delete-exited-processes t))
             (apply #'start-process latex-process-name latex-buffer-name qjp-latex-auto-compile-command
                    (append qjp-latex-auto-compile-command-options (list (TeX-master-file))))))))

  (defun qjp-latex-disable-auto-compile ()
    (interactive)
    (add-file-local-variable 'qjp-latex-auto-compile-p
                             (setq qjp-latex-auto-compile-p)))

  (defun qjp-latex-enable-auto-compile ()
    (interactive)
    (add-file-local-variable 'qjp-latex-auto-compile-p
                             (setq qjp-latex-auto-compile-p t)))

  (defun qjp-latex-set-auto-compile-command-options (opt-str)
    (interactive
     (list
      (read-string
       (format "New value(current value is \"%s\"): "
               (mapconcat 'identity qjp-latex-auto-compile-command-options " ")))))
    (add-file-local-variable 'qjp-latex-auto-compile-command-options
                             (setq qjp-latex-auto-compile-command-options
                                   (split-string opt-str " "))))

  (defun qjp-latex-switch-to-auto-compile-buffer ()
    (interactive)
    (let ((from-buffer (current-buffer)))
      (and (eq major-mode 'latex-mode)
           (switch-to-buffer (qjp-latex--get-or-create-auto-compile-buffer))
           (setq qjp-latex-auto-compile-from-buffer from-buffer))))

  (defun qjp-latex-switch-from-auto-compile-buffer ()
    (interactive)
    (if qjp-latex-auto-compile-from-buffer
        (switch-to-buffer qjp-latex-auto-compile-from-buffer)
      (message "No previous buffer for switching back.")))

  ;; ---------------------- ;;
  ;; User Defined Functions ;;
  ;; ---------------------- ;;

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
    (local-set-key [(return)] #'newline-and-indent)
    (local-set-key (kbd "C-c ,") #'LaTeX-mark-section)
    (local-set-key (kbd "C-x C-z") #'qjp-latex-switch-to-auto-compile-buffer))

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

  ;; Add company-auctex backend
  (with-eval-after-load 'company
    (company-auctex-init))

  ;; Settings for magic-latex-buffer
  (setq magic-latex-enable-block-align)

  ;; latex-extra
  (setq latex/no-fill-environments '("equation"
                                     "equation*"
                                     "align"
                                     "align*"
                                     "tabular"
                                     "tikzpicture"
                                     "verbatim"
                                     "verbatim*")))

;; ----- ;;
;; Hooks ;;
;; ----- ;;
(defun qjp-tex-mode-hook ()
  (qjp-tex-set-local-key-bindings)
  (turn-on-reftex)
  (qjp-turn-on-cdlatex)
  (latex-extra-mode +1)
  (magic-latex-buffer +1)
  (flyspell-mode +1)
  (flycheck-mode +1)
  (smartparens-mode +1)
  (qjp-tex-add-LaTeX-environments)
  (make-local-variable 'qjp-latex-auto-compile-command-options)
  (make-local-variable 'qjp-latex-auto-compile-p)
  (add-hook 'after-save-hook #'qjp-latex-auto-compile nil t))

(add-hook 'LaTeX-mode-hook #'qjp-tex-mode-hook)
(add-hook 'latex-mode-hook #'qjp-tex-mode-hook)

(provide 'qjp-tex)
;;; qjp-tex.el ends here
