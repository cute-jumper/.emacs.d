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
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-master nil)
(setq preview-scale-function 2)

;; Add option `-file-line-error' to avoid `TeX-next-error' error
;; See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=695282 for details
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -file-line-error -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
            ;; (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query  nil)))

;; Add `latexmk' to command list
(add-hook 'LaTeX-mode-hook 
          (lambda ()
            (add-to-list 'TeX-command-list
                         '("latexmk" "latexmk -pdf %s && latexmk -c" TeX-run-TeX nil t
                           :help "Run Latexmk on file"))
            (setq TeX-command-default "latexmk")
            (setq TeX-save-query nil)))

;; Add `pdflatex' to command list
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("pdflatex" "%`pdflatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-save-query  nil )
            (setq TeX-show-compilation nil)))

;; ------------------ ;;
;; Local key bindings ;;
;; ------------------ ;;
;; (add-hook 'LaTeX-mode-hook (lambda () (local-set-key [(control tab)] 'TeX-complete-symbol)))
;; (add-hook 'latex-mode-hook (lambda () (local-set-key [(control tab)] 'TeX-complete-symbol)))
(add-hook 'LaTeX-mode-hook 
          (lambda ()
            (local-set-key [(return)] 'newline-and-indent)
            (local-set-key (kbd "C-c ,") 'LaTeX-mark-section)))
(add-hook 'latex-mode-hook
          (lambda () 
            (local-set-key [(return)] 'newline-and-indent)
            (local-set-key (kbd "C-c ,") 'LaTeX-mark-section)))

;; ------- ;;
;; cdlatex ;;
;; ------- ;;
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode

;; ------ ;;
;; reftex ;;
;; ------ ;;
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)

;; ---------------------- ;;
;; User Defined Functions ;;
;; ---------------------- ;;

;; Auto update when saving
(setq qjp-latex-auto-command "latexmk")
(setq qjp-latex-auto-command-options '("-shell-escape" "-pdf"))
(defun qjp-latex-auto-update ()
  (interactive)
  (when (and (eq major-mode 'latex-mode)
             (string-match "\\.tex$" buffer-file-name))
    (let* ((latex-process-name "latex-auto-compile")
           (latex-buffer-name "*latex-auto-buffer*")
           (latex-process (get-process latex-process-name)))
      (when latex-process
        (delete-process latex-process))
      (when (get-buffer latex-buffer-name)
        (save-excursion
          (set-buffer latex-buffer-name)
          (erase-buffer)))
      (message "%s" latex-buffer-name)
      (apply 'start-process latex-process-name latex-buffer-name qjp-latex-auto-command
             (append qjp-latex-auto-command-options (list buffer-file-name))))))
(add-hook 'after-save-hook 'qjp-latex-auto-update)

;; Insert \usepackage in the front of the file
(defun qjp-latex-add-pkg (pkg-name pkg-options)
  (interactive "sPackage name: \nsOptions: ")
  (save-excursion
    (goto-char (point-max))
    (unless (search-backward "\\usepackage" 0 t)
      (search-backward "\\documentclass" 0 t))
    (end-of-line)
    (newline-and-indent)
    (if (string= pkg-options "")
        (insert "\\usepackage{" pkg-name "}")
      (insert "\\usepackage[" pkg-options "]{" pkg-name "}"))))


(provide 'qjp-tex)
;;; qjp-tex.el ends here
