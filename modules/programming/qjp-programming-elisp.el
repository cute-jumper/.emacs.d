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

;; - `eldoc-eval' seems unnecessary because of
;;   `eval-expression-minibuffer-setup-hook'
;; - pp-print
;; - ipretty
;; - Maybe ielm in other window like purcell or prelude?
;; - rainbow-delimiters-mode vs highlight-parentheses-mode
;; - red-shank
;; - conditionally enable rainbow-mode
;; - hl-sexp? Maybe. A little intrusive though.
;; - lively

;;; Code:

;; load newer code
(setq load-prefer-newer t)

;; ---------- ;;
;; eldoc mode ;;
;; ---------- ;;
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

;; ------------------ ;;
;; Evaluation related ;;
;; ------------------ ;;
;; Remove ellipsis when evaling sexp in minibuffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Minibuffer hook
(defun qjp-eval-expression-minibuffer-setup-hook ()
  (eldoc-mode +1)
  (paredit-mode +1))

(add-hook 'eval-expression-minibuffer-setup-hook
          #'qjp-eval-expression-minibuffer-setup-hook)

;; Use pp-eval-expression
(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "M-:") 'pp-eval-expression))

(defadvice pp-display-expression (after sanityinc/make-read-only (expression out-buffer-name) activate)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

;; Make C-x C-e run `eval-region' if the region is active
(defun qjp-eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map
    (kbd "C-x C-e")
    'qjp-eval-last-sexp-or-region))

;; ----------------------- ;;
;; hippie-expand for elisp ;;
;; ----------------------- ;;
(defun qjp-emacs-lisp-module-name ()
  "Search the buffer for `provide' declaration."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^(provide '" nil t)
      (symbol-name (symbol-at-point)))))

(defun qjp-try-complete-lisp-symbol-without-namespace (old)
  "Hippie expand \"try\" function which expands \"-foo\" to \"modname-foo\" in elisp."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (when (string-prefix-p "-" he-search-string)
      (let ((mod-name (qjp-emacs-lisp-module-name)))
        (when mod-name
          (setq he-expand-list (list (concat mod-name he-search-string)))))))
  (when he-expand-list
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list nil)
    t))

(defun qjp-hippie-expand-setup-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'qjp-try-complete-lisp-symbol-without-namespace t))

;; -------- ;;
;; redshank ;;
;; -------- ;;
;; Must set before redshank is loaded
(setq redshank-prefix-key "C-c C-r")

;; ------------------- ;;
;; Emacs lisp and ielm ;;
;; ------------------- ;;
(defun qjp-emacs-lisp-mode-hook ()
  (qjp-ielm-mode-hook)
  ;; Unique settings for emacs-lisp-mode
  (hideshowvis-minor-mode +1)
  (redshank-mode +1)
  (add-hook 'after-save-hook #'check-parens nil t))

(defun qjp-ielm-mode-hook ()
  (eldoc-mode +1)
  (paredit-mode +1)
  (highlight-parentheses-mode +1)
  (elisp-slime-nav-mode +1)
  (aggressive-indent-mode +1)
  (qjp-hippie-expand-setup-for-elisp))

(add-hook 'emacs-lisp-mode-hook #'qjp-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook #'qjp-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'qjp-ielm-mode-hook)

;; macrostep
(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand))

;; switch between ielm and emacs lisp buffer
(defvar qjp-ielm-from-buffer nil
  "From where we switch to ielm")
(make-variable-buffer-local 'qjp-ielm-from-buffer)

(defun qjp-switch-to-ielm ()
  (interactive)
  (let ((from-buffer (current-buffer)))
    (ielm)
    (setq qjp-ielm-from-buffer from-buffer)))

(defun qjp-switch-from-ielm ()
  (interactive)
  (if qjp-ielm-from-buffer
      (switch-to-buffer qjp-ielm-from-buffer)
    (message "No previous buffer for switching back.")))

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-z") #'qjp-switch-to-ielm)
  (define-key lisp-interaction-mode-map (kbd "C-z") #'qjp-switch-to-ielm))
(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "C-z") #'qjp-switch-from-ielm))

;; ------------ ;;
;; company-mode ;;
;; ------------ ;;
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elisp))

;; ---------------- ;;
;; flycheck-package ;;
;; ---------------- ;;
(with-eval-after-load 'flycheck
  (flycheck-package-setup))

;; -------------- ;;
;; Paredit addons ;;
;; -------------- ;;
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
