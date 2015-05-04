;;; qjp-programming-elisp.el --- Settings for Emacs Lisp

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

;; Remove ellipsis when evaling sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; --------------- ;;
;; emacs lisp mode ;;
;; --------------- ;;
(require 'eldoc)

;; Source: http://emacswiki.org/emacs/ElDoc
;; Since `flet' is obsolete, use `qjp--flet' instead(dynamic scoping needed)
(defadvice eldoc-highlight-function-argument
    (around my-formatting (sym args index) compile activate preactivate)
  "Replace original to apply my style of formatting."
  ;; HACK: intercept the call to eldoc-docstring-format-sym-doc at the
  ;; end of the adviced function. This is obviously brittle, but the
  ;; alternative approach of copy/pasting the original also has
  ;; downsides...
  (qjp--flet (eldoc-docstring-format-sym-doc
              (sym doc face)
              (let* ((function-name (propertize (symbol-name sym)
                                                'face face))
                     (spec (format "%s %s" function-name doc))
                     (docstring (or (eldoc-docstring-first-line
                                     (documentation sym t))
                                    "Undocumented."))
                     (docstring (propertize docstring
                                            'face 'font-lock-doc-face))
                     ;; TODO: currently it strips from the start of spec by
                     ;; character instead of whole arguments at a time.
                     (fulldoc (format "%s %s" spec docstring))
                     (ea-width (1- (window-width (minibuffer-window))))
                     (diff (- (length fulldoc) ea-width)))
                (cond ((or (<= (length fulldoc) ea-width)
                           (eq eldoc-echo-area-use-multiline-p t)
                           (and eldoc-echo-area-use-multiline-p
                                (> (length docstring) ea-width)))
                       fulldoc)
                      ((> (length docstring) ea-width)
                       (substring docstring 0 ea-width))
                      ((>= (- (length fulldoc) (length spec)) ea-width)
                       docstring)
                      ((<= diff (1+ (length function-name)))
                       ;; Show the end of the partial symbol name, rather
                       ;; than the beginning, since the former is more likely
                       ;; to be unique given package namespace conventions.
                       (setq spec (substring spec (- (length fulldoc) ea-width)))
                       (format "%s %s" spec docstring))
                      (t
                       (format "%s %s" doc docstring)))))
    ad-do-it))

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round
 'electrify-return-if-match)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (hl-line-mode -1)
                                  (eldoc-mode 1)
                                  (highlight-parentheses-mode)
                                  (paredit-mode)
                                  (hideshowvis-minor-mode)
                                  (local-set-key (kbd "RET") 'electrify-return-if-match)))

(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook (lambda () (eldoc-mode) (paredit-mode)))
(define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)

(with-eval-after-load 'paredit
  (defun paredit-barf-all-the-way-backward ()
     (interactive)
     (paredit-split-sexp)
     (paredit-backward-down)
     (paredit-splice-sexp))

  (defun paredit-barf-all-the-way-forward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-forward-down)
    (paredit-splice-sexp)
    (if (eolp) (delete-horizontal-space)))

  (defun paredit-slurp-all-the-way-backward ()
    (interactive)
    (catch 'done
      (while (not (bobp))
        (save-excursion
          (paredit-backward-up)
          (if (eq (char-before) ?\()
              (throw 'done t)))
        (paredit-backward-slurp-sexp))))

  (defun paredit-slurp-all-the-way-forward ()
    (interactive)
    (catch 'done
      (while (not (eobp))
        (save-excursion
          (paredit-forward-up)
          (if (eq (char-after) ?\))
              (throw 'done t)))
        (paredit-forward-slurp-sexp))))

  (nconc paredit-commands
         '("Extreme Barfage & Slurpage"
           (("C-M-)")
            paredit-slurp-all-the-way-forward
            ("(foo (bar |baz) quux zot)"
             "(foo (bar |baz quux zot))")
            ("(a b ((c| d)) e f)"
             "(a b ((c| d)) e f)"))
           (("C-M-}" "M-F")
            paredit-barf-all-the-way-forward
            ("(foo (bar |baz quux) zot)"
             "(foo (bar|) baz quux zot)"))
           (("C-M-(")
            paredit-slurp-all-the-way-backward
            ("(foo bar (baz| quux) zot)"
             "((foo bar baz| quux) zot)")
            ("(a b ((c| d)) e f)"
             "(a b ((c| d)) e f)"))
           (("C-M-{" "M-B")
            paredit-barf-all-the-way-backward
            ("(foo (bar baz |quux) zot)"
             "(foo bar baz (|quux) zot)"))))
  (define-key paredit-mode-map (kbd "C-M-)") 'paredit-slurp-all-the-way-forward)
  (define-key paredit-mode-map (kbd "C-M-}") 'paredit-barf-all-the-way-forward)
  (define-key paredit-mode-map (kbd "M-F") 'paredit-barf-all-the-way-forward)
  (define-key paredit-mode-map (kbd "C-M-(") 'paredit-slurp-all-the-way-backward)
  (define-key paredit-mode-map (kbd "C-M-{") 'paredit-barf-all-the-way-backward)
  (define-key paredit-mode-map (kbd "M-B") 'paredit-barf-all-the-way-backward))

(provide 'qjp-programming-elisp)
;;; qjp-programming-elisp.el ends here
