;;; qjp-org-publish.el --- Settings for org-mode publishing

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

(defvar qjp-personal-publish-base (expand-file-name "~/Documents/org-publish/") "Base directory for publishing")

;; Add a link type to do jekyll hacking
;; From:
;; http://stackoverflow.com/questions/14684263/how-to-org-mode-image-absolute-path-of-export-html
(defun qjp-org-jekyll-post-link-follow (path)
  (org-open-file-with-emacs path))

(defun qjp-org-jekyll-post-link-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<a href=\"{%% post_url %s %%}\">%s</a>" path desc))))

(org-add-link-type "jekyll-post" 'qjp-org-jekyll-post-link-follow 'qjp-org-jekyll-post-link-export)

(setq org-publish-project-alist
      `(("org-localhost-sources" ;; settings for sources of localhost
         :base-directory ,(concat qjp-document-dir "Sites/localhost")
         :base-extension "org"
         :publishing-directory ,(concat qjp-personal-publish-base "localhost")
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :auto-sitemap t
         :html-extension "html")
        ("org-localhost-assets" ;; settings for assets of localhost
         :base-directory ,(concat qjp-document-dir "Sites/localhost")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,(concat qjp-personal-publish-base "localhost")
         :recursive t
         :publishing-function org-publish-attachment)
        ;; Format is controlled by ourselves
        ("org-localhost" :components ("org-localhost-sources" "org-localhost-assets"))
        ;; Only export org to html. Format is controlled by jekyll directory
        ("jekyll-cute-jumper-github-io" ;; settings for cute-jumper.github.io
         :base-directory ,(concat qjp-document-dir "Sites/cute-jumper.github.io")
         :base-extension "org"
         :publishing-directory ,(concat qjp-personal-publish-base "cute-jumper.github.io")
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-toc nil
         :headline-levels 4
         :auto-preamble nil
         :auto-sitemap nil
         :html-extension "html"
         :body-only t)))

;; Add timeline to cute-jumper.github.io project
(defun qjp-github-io-copy-timeline-org()
  (interactive)
  (let ((src-file (expand-file-name (concat qjp-document-dir "Journal/timeline.org")))
        (dest-file (expand-file-name (concat qjp-document-dir "Sites/cute-jumper.github.io/timeline.org"))))
    (cond ((file-exists-p src-file)
           (with-temp-file dest-file
             (insert "#+BEGIN_HTML\n---\nlayout: page\ntitle: Timeline\ngroup: navigation\n---\n#+END_HTML\n")
             (insert-file-contents src-file))
           (message "Copy %s to %s" src-file dest-file))
          (t (message "%s not exists!" src-file)))))

;; Helper functions
;; Steal from
;; http://www.gorgnegre.com/linux/using-emacs-orgmode-to-blog-with-jekyll.html
;; Make slight modification
(defvar qjp-jekyll-directory (expand-file-name (concat qjp-personal-publish-base "Sites/cute-jumper.github.io/"))
  "Path to Jekyll blog.")
(defvar qjp-jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar qjp-jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar qjp-jekyll-post-ext ".org"
  "File extension of Jekyll posts.")
(defvar qjp-jekyll-post-template
  "BEGIN_HTML\n---\nlayout: post\ntitle: %s\nexcerpt: \ncategories:\n  -  \ntags:\n  -  \n---\n#+END_HTML\n\n* "
  "Default template for Jekyll posts. %s will be replace by the post title.")

(defun qjp-jekyll-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun qjp-jekyll-yaml-escape (s)
  "Escape a string for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun qjp-jekyll-draft-post (title)
  "Create a new Jekyll blog post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat qjp-jekyll-directory qjp-jekyll-drafts-dir
                            (jekyll-make-slug title)
                            qjp-jekyll-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format qjp-jekyll-post-template (jekyll-yaml-escape title))))))

(defun qjp-jekyll-publish-post ()
  "Move a draft post to the posts directory, and rename it so that it
 contains the date."
  (interactive)
  (cond
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (concat qjp-jekyll-directory qjp-jekyll-drafts-dir)))
    (message "This is not a draft post."))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat qjp-jekyll-directory qjp-jekyll-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

(provide 'qjp-org-publish)
;;; qjp-org-publish.el ends here
