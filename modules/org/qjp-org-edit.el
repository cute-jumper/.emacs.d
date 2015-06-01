;;; qjp-org-edit.el --- Settings for editing in org-mode

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

;; ---------------------------------- ;;
;; org structure movement and editing ;;
;; ---------------------------------- ;;

;; ------------- ;;
;; Basic setting ;;
;; ------------- ;;
(setq org-use-speed-commands t)
(setq org-cycle-separator-lines 0)      ;get rid of the blank lines when cycling
(setq org-list-allow-alphabetical t)         ;enable a. b. c. as plain lists

;; Show next/prev heading tidily
;; From: http://orgmode.org/worg/org-hacks.html
(defun qjp-org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun qjp-org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(add-to-list 'org-speed-commands-user
             '("N" qjp-org-show-next-heading-tidily))
(add-to-list 'org-speed-commands-user
             '("P" qjp-org-show-previous-heading-tidily))

;; -------- ;;
;; org TODO ;;
;; -------- ;;
(setq org-todo-keyword-faces
      '(("STARTED". "yellow") ("CANCELED" . "red")))

;; ------- ;;
;; org tag ;;
;; ------- ;;
(setq org-tag-alist
      '(("c" . ?c)
        ("cpp")
        ("emacs" . ?e)
        ("firefox" . ?f)
        ("git" . ?g)
        ("interesting" . ?i)
        ("idea")
        ("java" . ?j)
        ("linux" . ?l)
        ("latex")
        ("lisp")
        ("misc" . ?m)
        ("python" . ?p)
        ("shell" . ?s)
        ("scala")))

;; ------------ ;;
;; org capture. ;;
;; ------------ ;;
;; seems adding empty-lines argument to "j" would cause error:
;; "Capture template `j': Invalid search bound (wrong side of point)"
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline (concat qjp-document-dir "Notes/TODO_DONE/TODO.org") "TODO")
         "* TODO %?\n  %T\n" :empty-lines 1 :unnarrowed t :kill-buffer t)
        ("p" "Problems" entry (file+headline (concat qjp-document-dir "Notes/TODO_DONE/Problems.org") "Problems")
         "* %?\n  %T\n" :empty-lines 1 :unnarrowed t :kill-buffer t)
        ("j" "Journal" plain (file+datetree (concat qjp-document-dir "Journal/journal.org.gpg"))
         "" :unnarrowed t :kill-buffer t)
        ("a" "Task" entry (file+headline (concat qjp-document-dir "Agenda/tasks.org") "Tasks")
         "* TODO %?\n %t\n" :empty-lines 1 :kill-buffer t)
        ("c" "Capture" entry (file+headline (concat qjp-document-dir "capture.org") "Capture")
         "* %^{Title}\n  Source: %u, %c\n  %i" :empty-lines 1 :kill-buffer t)
        ("l" "Timeline" plain (file+datetree (concat qjp-document-dir "Journal/timeline.org"))
         "" :unnarrowed t :kill-buffer t)))

;; ------------- ;;
;; Org-mode hook ;;
;; ------------- ;;
(defun qjp-org-mode-hook ()
  ;; CDLaTeX support
  (turn-on-org-cdlatex)
  ;; highlight current line
  (hl-line-mode +1))

(add-hook 'org-mode-hook #'qjp-org-mode-hook)

(provide 'qjp-org-edit)
;;; qjp-org-edit.el ends here
