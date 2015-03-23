;;; qjp-org-notes.el --- Use org-mode to manage notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

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

;; -------------------- ;;
;; org search for notes ;;
;; -------------------- ;;
(defvar qjp-notes-dir (concat qjp-document-dir "Notes/")
  "Root directory of all notes")
(defvar qjp-notes-file-list (file-expand-wildcards (concat qjp-notes-dir "*/*.org"))
  "A list of .org files serving as notes")

;; Use agenda to search
(setq org-agenda-custom-commands
      '(("n" . "Search in notes")
        ("nt" "Note tags search" tags ""
         ((org-agenda-files qjp-notes-file-list))) 
        ("ns" "Note full text search" search ""
         ((org-agenda-files qjp-notes-file-list)))))

;; Copy and modify `helm-org-agenda-files-headings' to search the headings in
;; notes
(defun qjp-helm-notes-headings ()
  (interactive)
  (helm :sources (helm-source-org-headings-for-files qjp-notes-file-list)
        :candidate-number-limit 99999
        :buffer "*helm org headings*"))

;; FIX: This function needs to be polished. Currently I don't know a better
;; implementation. 
;; Copy and modify implementation of `helm-projectile-grep-or-ack'
(defun qjp-helm-notes-search ()
  (interactive)
  (let* ((default-directory qjp-notes-dir)
         (helm-ff-default-directory qjp-notes-dir)
         (follow (and helm-follow-mode-persistent
                      (assoc-default 'follow helm-source-grep)))
         (helm-grep-in-recurse t)
         (grep-find-ignored-files (-union projectile-globally-ignored-files  grep-find-ignored-files))
         (grep-find-ignored-directories (-union projectile-globally-ignored-directories grep-find-ignored-directories))
         (helm-grep-default-command "grep -a -r %e -n%cH -e %p %f .")
         (helm-grep-default-recurse-command helm-grep-default-command)
         (helm-source-grep
          (helm-build-async-source
              (capitalize (helm-grep-command t))
            :header-name (lambda (name)
                           (let ((name "Helm Projectile Grep"))
                             (concat name " " "(C-c ? Help)")))
            :candidates-process 'helm-grep-collect-candidates
            :filter-one-by-one 'helm-grep-filter-one-by-one
            :candidate-number-limit 9999
            :nohighlight t
            :mode-line helm-grep-mode-line-string
            ;; We need to specify keymap here and as :keymap arg [1]
            ;; to make it available in further resuming.
            :keymap helm-grep-map
            :history 'helm-grep-history
            :action (helm-make-actions
                     "Find file" 'helm-grep-action
                     "Find file other frame" 'helm-grep-other-frame
                     (lambda () (and (locate-library "elscreen")
                                     "Find file in Elscreen"))
                     'helm-grep-jump-elscreen
                     "Save results in grep buffer" 'helm-grep-save-results
                     "Find file other window" 'helm-grep-other-window)
            :persistent-action 'helm-grep-persistent-action
            :persistent-help "Jump to line (`C-u' Record in mark ring)"
            :requires-pattern 2)))
    (helm
     :sources 'helm-source-grep
     :input (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol))
     :buffer (format "*helm %s*" "grep")
     :default-directory qjp-notes-dir
     :keymap helm-grep-map
     :history 'helm-grep-history
     :truncate-lines t)))


(provide 'qjp-org-notes)
;;; qjp-org-notes.el ends here
