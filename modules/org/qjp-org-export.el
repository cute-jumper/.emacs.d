;;; qjp-org-export.el --- Settings for export in org-mode

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

;; ---------- ;;
;; org export ;;
;; ---------- ;;
(setq org-export-backends '(latex
                            html
                            beamer
                            md
                            ascii))
(setq org-latex-pdf-process
      '("latexmk -pdflatex=xelatex -pdf -silent -f %f && latexmk -c"))
(setq org-latex-packages-alist nil)
(setq org-latex-listings t)
(setq org-latex-listings-options
      ;; avoid having listings get entangled in the other package’s extended-character treatment
      '(("extendedchars" "false")
        ("basicstyle" "\\ttfamily\\footnotesize")
        ("escapechar" "`")
        ("breaklines" "")))

;; --------------------------------- ;;
;; TODO: LaTeX templates             ;;
;; Use my own LaTeX packages instead ;;
;; --------------------------------- ;;
(require 'ox-latex)
(add-to-list 'org-latex-classes
      '("zh-article"
        "\\documentclass[a4paper]{article}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]
"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
      '("en-article"
        "\\documentclass[a4paper]{article}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]
"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(provide 'qjp-org-export)
;;; qjp-org-export.el ends here
