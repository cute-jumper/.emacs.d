;;; qjp-programming-ruby.el --- Settings for Ruby

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

;; Remove ruby-end and ruby-block because of smartparens

;;; Code:

;; auto-mode-alist for Ruby
(qjp-add-auto-mode 'ruby-mode
                   "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
                   "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'"
                   "\\.builder\\'" "\\.ru\\'" "\\.gemspec\\'"
                   "Gemfile\\'" "Kirkfile\\'")

(defun qjp-start-inf-ruby (&optional command name)
  "Modified from ``run-ruby'.'"
  (interactive)
  (setq command (or command (cdr (assoc inf-ruby-default-implementation
                                        inf-ruby-implementations))))
  (setq name (or name "ruby"))

  (if (not (comint-check-proc inf-ruby-buffer))
      (let ((commandlist (split-string-and-unquote command))
            (buffer (current-buffer))
            (process-environment process-environment))
        ;; http://debbugs.gnu.org/15775
        (setenv "PAGER" (executable-find "cat"))
        (with-current-buffer (apply 'make-comint name (car commandlist)
                                    nil (cdr commandlist))
          (inf-ruby-mode)
          (ruby-remember-ruby-buffer buffer))))
  (setq inf-ruby-buffer (format "*%s*" name)))

(defun qjp-ruby-mode-hook ()
  "My mode hook for Ruby."
  (subword-mode +1)
  ;; inf-ruby
  (inf-ruby-minor-mode +1)
  (when (executable-find "pry")
    (setq inf-ruby-default-implementation "pry"))
  ;; set rvm before robe starts
  (rvm-activate-corresponding-ruby)
  ;; robe
  (robe-mode +1)
  (unless (inf-ruby-console-match
           (locate-dominating-file default-directory
                                   #'inf-ruby-console-match))
    (qjp-start-inf-ruby))
  ;; robe can provide doc via eldoc
  (eldoc-mode +1)
  ;; Ruby tools
  (ruby-tools-mode +1)
  ;; key bindings
  (define-key ruby-mode-map (kbd "C-c C-c") #'ruby-compilation-this-buffer)
  ;; add company-backends
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-robe)
  (add-to-list 'company-backends 'company-inf-ruby))

(add-hook 'ruby-mode-hook #'qjp-ruby-mode-hook)

(provide 'qjp-programming-ruby)
;;; qjp-programming-ruby.el ends here
