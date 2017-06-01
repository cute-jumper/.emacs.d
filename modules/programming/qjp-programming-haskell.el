;;; qjp-programming-haskell.el --- Settings for Haskell

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

;; ------------------ ;;
;; haskell-cabal-mode ;;
;; ------------------ ;;
(with-eval-after-load 'haskell-cabal-mode-map
  (define-key haskell-cabal-mode-map
    (kbd "C-c C-z") 'haskell-interactive-switch))

;; ----------- ;;
;; company-ghc ;;
;; ----------- ;;
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code)))

;; --------- ;;
;; variables ;;
;; --------- ;;
(setq haskell-process-auto-import-loaded-modules t
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-stylish-on-save t
      haskell-process-type 'stack-ghci
      haskell-interactive-popup-errors nil)
;; (setq company-ghc-show-info t)

;; ----------- ;;
;; align rules ;;
;; ----------- ;;
(with-eval-after-load 'align
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes . '(haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes . '(haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes . '(haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes . '(haskell-mode)))))

;; ---------------------------- ;;
;; symbols for prettify-symbols ;;
;; ---------------------------- ;;
(defvar qjp-programming-haskell-prettify-symbols-alist
  '(("::"     . ?∷)
    ("forall" . ?∀)
    ("exists" . ?∃)
    ("->"     . ?→)
    ("<-"     . ?←)
    ("=>"     . ?⇒)
    ("~>"     . ?⇝)
    ("<~"     . ?⇜)
    ;; ("."      . ?∘)
    ("<>"     . ?⨂)
    ("msum"   . ?⨁)
    ("\\"     . ?λ)
    ("not"    . ?¬)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("/="     . ?≠)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("<<<"    . ?⋘)
    (">>>"    . ?⋙)

    ("`elem`"             . ?∈)
    ("`notElem`"          . ?∉)
    ("`member`"           . ?∈)
    ("`notMember`"        . ?∉)
    ("`union`"            . ?∪)
    ("`intersection`"     . ?∩)
    ("`isSubsetOf`"       . ?⊆)
    ("`isProperSubsetOf`" . ?⊂)
    ("undefined"          . ?⊥)))

;; ----------------- ;;
;; haskell-mode hook ;;
;; ----------------- ;;
(defun qjp-haskell-mode-basic-hook ()
  "Basic hook for haskell-mode."
  (subword-mode +1)
  (haskell-doc-mode +1))

(defun qjp-haskell-mode-hook ()
  "My mode hook for haskell-mode."
  (qjp-haskell-mode-basic-hook)
  (haskell-indentation-mode +1)
  ;; prettify-symbols
  (setq-local prettify-symbols-alist
              qjp-programming-haskell-prettify-symbols-alist)
  (prettify-symbols-mode +1)
  ;; init ghc-mod
  (ghc-init)
  ;; Don't enable interactive-haskell-mode because we have ghc-mod
  ;; But these bindings are missing
  (define-key haskell-mode-map (kbd "C-c C-l") #'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "M-.") #'haskell-mode-jump-to-def-or-tag)
  (define-key haskell-mode-map (kbd "C-c C-z") #'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "M-N") #'ghc-goto-next-error)
  (define-key haskell-mode-map (kbd "M-P") #'ghc-goto-prev-error)
  ;; NOTE: hindent-mode and shm-mode can be considered
  ;; (hindent-mode +1)
  ;; (structured-haskell-mode +1)
  (setq-local helm-dash-docsets '("Haskell"))
  ;; intero
  ;; (intero-mode +1)
  (eldoc-mode +1))

(add-hook 'inferior-haskell-mode-hook #'qjp-haskell-mode-basic-hook)
(add-hook 'haskell-interactive-mode-hook #'qjp-haskell-mode-basic-hook)
(add-hook 'haskell-mode-hook #'qjp-haskell-mode-hook)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'qjp-programming-haskell)
;;; qjp-programming-haskell.el ends here
