;;; qjp-misc-tabbar.el --- Settings for tabbar-mode

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

;; ----------- ;;
;; tabbar-mode ;;
;; ----------- ;;
(require 'tabbar)
(tabbar-mode +1)

;; Set faces
(set-face-attribute 'tabbar-default nil
                    :family "Vera Sans YuanTi Mono"
                    :background "gray80"
                    :foreground "gray30"
                    :height 70)
(set-face-attribute 'tabbar-selected nil
                    :background "gray95"
                    :foreground "gray20"
                    :inherit 'tabbar-default
                    :box '(:line-width 3 :color "grey95" :style nil))
(set-face-attribute 'tabbar-unselected nil
                    :inherit 'tabbar-default
                    :background "gray80"
                    :box '(:line-width 2 :color "grey80" :style nil))

;; Define custome group function
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's tabbar-buffer-groups.
  This function group all buffers into 3 groups:
  Those Dired, those user buffer, and those emacs buffer.
  Emacs buffer are those starting with “*”."
  (list
   (cond
    ((eq major-mode 'dired-mode)
     "Dired & Emacs Buffers")
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Dired & Emacs Buffers")
    (t "User Buffer"))))
;; From Xah Lee, at wiki
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; Add a buffer modification state indicator in the tab label,
;; and place a space around the label to make it looks less crowd
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; first-change-hook is called BEFORE the change is made
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))

(add-hook 'after-save-hook 'ztl-modification-state-change)

;; this doesn't work for revert, I don't know
(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(defun tabbar-add-tab (tabset object &optional append_ignored)
  "Add to TABSET a tab with value OBJECT if there isn't one there yet.
 If the tab is added, it is added at the beginning of the tab list,
 unless the optional argument APPEND is non-nil, in which case it is
 added at the end."
  (let ((tabs (tabbar-tabs tabset)))
    (if (tabbar-get-tab object tabset)
        tabs
      (let ((tab (tabbar-make-tab object tabset)))
        (tabbar-set-template tabset nil)
        (set tabset (sort (cons tab tabs)
                          (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

(provide 'qjp-misc-tabbar)
;;qjp-misc-tabbar.el ends here
