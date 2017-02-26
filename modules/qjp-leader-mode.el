;;; qjp-leader-mode.el --- A leader mode based on qjp-leader-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords:

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

(require 'cl-lib)
(require 'evil)

(defvar qjp-leader-special nil)

(add-hook 'after-change-major-mode-hook 'qjp-leader-mode-maybe-activate)

(defvar qjp-leader-mode-paused nil)
(make-variable-buffer-local 'qjp-leader-mode-paused)

(defcustom qjp-leader-mod-alist
  '((nil . "C-")
    ("g" . "C-M-")
    ("G" . "M-"))
  "List of keys and their associated modifer."
  :group 'qjp-leader
  :type '(alist))

(defcustom qjp-leader-literal-key
  " "
  "The key used for literal interpretation."
  :group 'qjp-leader
  :type 'string)

(defcustom qjp-leader-exempt-major-modes
  '(dired-mode
    grep-mode
    vc-annotate-mode
    git-commit-mode  ; For versions prior to Magit 2.1.0
    magit-popup-mode)
  "List of major modes that should not start in qjp-leader-local-mode."
  :group 'qjp-leader
  :type '(function))

(defcustom qjp-leader-exempt-predicates
  (list #'qjp-leader-exempt-mode-p
        #'qjp-leader-comint-mode-p
        #'qjp-leader-git-commit-mode-p
        #'qjp-leader-view-mode-p
        #'qjp-leader-special-mode-p)
  "List of predicates checked before enabling qjp-leader-local-mode.
All predicates must return nil for qjp-leader-local-mode to start."
  :group 'qjp-leader
  :type '(repeat function))

(defvar qjp-leader-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'qjp-leader-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'qjp-leader-mode-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL") nil))
    map))

(defvar qjp-leader-mode-universal-argument-map
  (let ((map (copy-keymap universal-argument-map)))
    (define-key map (kbd "u") 'universal-argument-more)
    map)
  "Keymap used while processing \\[universal-argument] with qjp-leader-mode on.")

;;;###autoload
(define-minor-mode qjp-leader-local-mode
  "Minor mode for running commands."
  nil " QLeader" qjp-leader-local-mode-map
  (if qjp-leader-local-mode
      (run-hooks 'qjp-leader-mode-enabled-hook)
    (run-hooks 'qjp-leader-mode-disabled-hook)))

(defun qjp-leader-local-mode-pause ()
  "Pause qjp-leader-mode local to the buffer, if it's
enabled. See also `qjp-leader-local-mode-resume'."
  (when qjp-leader-local-mode
    (qjp-leader-local-mode -1)
    (setq qjp-leader-mode-paused t)))

(defun qjp-leader-local-mode-resume ()
  "Will re-enable qjp-leader-mode, if it was active when
`qjp-leader-local-mode-pause' was called. If not, nothing happens."
  (when (bound-and-true-p qjp-leader-mode-paused)
    (setq qjp-leader-mode-paused nil)
    (qjp-leader-local-mode 1)))

(defvar qjp-leader-global-mode nil
  "Activate QLeader mode on all buffers?")

(defvar qjp-leader-literal-sequence nil
  "Activated after space is pressed in a command sequence.")

;;;###autoload
(defun qjp-leader-mode ()
  "Toggle global QLeader mode."
  (interactive)
  (setq qjp-leader-global-mode (not qjp-leader-global-mode))
  (if qjp-leader-global-mode
      (qjp-leader-local-mode 1)
    (qjp-leader-local-mode -1)))

;;;###autoload
(defun qjp-leader-mode-all ()
  "Toggle QLeader mode in all buffers."
  (interactive)
  (let ((new-status (if (bound-and-true-p qjp-leader-local-mode) -1 1)))
    (setq qjp-leader-global-mode t)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (qjp-leader-mode-activate new-status)))
          (buffer-list))
    (setq qjp-leader-global-mode (= new-status 1))))

(defadvice save&set-overriding-map
    (before qjp-leader-mode-add-to-universal-argument-map (map) activate compile)
  "This is used to set special keybindings after C-u is
pressed. When qjp-leader-mode is active, intercept the call to add in
our own keybindings."
  (if (and qjp-leader-local-mode (equal universal-argument-map map))
      (setq map qjp-leader-mode-universal-argument-map)))

(defun qjp-leader-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (binding (qjp-leader-mode-lookup-key-sequence initial-key)))
    (when (qjp-leader-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq qjp-leader-literal-sequence nil)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun qjp-leader-mode-upper-p (char)
  "Is the given char upper case?"
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun qjp-leader-mode-sanitized-key-string (key)
  "Convert any special events to textual."
  (cl-case key
    (tab "TAB")
    (?\  "SPC")
    (left "<left>")
    (right "<right>")
    (prior "<prior>")
    (next "<next>")
    (backspace "DEL")
    (return "RET")
    (t (char-to-string key))))

(defun qjp-leader-key-string-after-consuming-key (key key-string-so-far)
  "Interpret qjp-leader-mode special keys for key (consumes more keys if
appropriate). Append to keysequence."
  (let ((key-consumed t) next-modifier next-key)
    (message key-string-so-far)
    (setq next-modifier
          (cond
           ((string= key qjp-leader-literal-key)
            (setq qjp-leader-literal-sequence t)
            "")
           (qjp-leader-literal-sequence
            (setq key-consumed nil)
            "")
           ((and
             (stringp key)
             (not (eq nil (assoc key qjp-leader-mod-alist)))
             (not (eq nil key)))
            (cdr (assoc key qjp-leader-mod-alist)))
           (t
            (setq key-consumed nil)
            (cdr (assoc nil qjp-leader-mod-alist))
            )))
    (setq next-key
          (if key-consumed
              (qjp-leader-mode-sanitized-key-string (read-event key-string-so-far))
            key))
    (if key-string-so-far
        (concat key-string-so-far " " next-modifier next-key)
      (concat next-modifier next-key))))

(defun qjp-leader-mode-lookup-command (key-string)
  "Execute extended keymaps such as C-c, or if it is a command,
call it."
  (let* ((key-vector (read-kbd-macro key-string t))
         (binding (key-binding key-vector)))
    (cond ((commandp binding)
           (setq last-command-event (aref key-vector (- (length key-vector) 1)))
           binding)
          ((keymapp binding)
           (qjp-leader-mode-lookup-key-sequence nil key-string))
          (:else
           (error "QLeader: Unknown key binding for `%s`" key-string)))))

;;;###autoload
(defun qjp-leader-mode-maybe-activate (&optional status)
  "Activate QLeader mode locally on individual buffers when appropriate."
  (when (not (minibufferp))
    (qjp-leader-mode-activate status)))

(defun qjp-leader-mode-activate (&optional status)
  "Activate QLeader mode locally on individual buffers when appropriate."
  (when (and qjp-leader-global-mode
             (qjp-leader-passes-predicates-p))
    (qjp-leader-local-mode (if status status 1))))

(defun qjp-leader-exempt-mode-p ()
  "Return non-nil if major-mode is exempt.
Members of the `qjp-leader-exempt-major-modes' list are exempt."
  (memq major-mode qjp-leader-exempt-major-modes))

(defun qjp-leader-mode-child-of-p (major-mode parent-mode)
  "Return non-nil if MAJOR-MODE is derived from PARENT-MODE."
  (let ((parent (get major-mode 'derived-mode-parent)))
    (cond ((eq parent parent-mode))
          ((not (null parent))
           (qjp-leader-mode-child-of-p parent parent-mode))
          (t nil))))

(defun qjp-leader-comint-mode-p ()
  "Return non-nil if major-mode is child of comint-mode."
  (qjp-leader-mode-child-of-p major-mode 'comint-mode))

(defun qjp-leader-special-mode-p ()
  "Return non-nil if major-mode is child of special-mode."
  (qjp-leader-mode-child-of-p major-mode 'special-mode))

(defun qjp-leader-view-mode-p ()
  "Return non-nil if view-mode is enabled in current buffer."
  view-mode)

(defun qjp-leader-git-commit-mode-p ()
  "Return non-nil if a `git-commit-mode' will be enabled in this buffer."
  (and (bound-and-true-p global-git-commit-mode)
       ;; `git-commit-filename-regexp' defined in the same library as
       ;; `global-git-commit-mode'.  Expression above maybe evaluated
       ;; to true because of autoload cookie.  So we perform
       ;; additional check.
       (boundp 'git-commit-filename-regexp)
       buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)))

(defun qjp-leader-passes-predicates-p ()
  "Return non-nil if all `qjp-leader-exempt-predicates' return nil."
  (not
   (catch 'disable
     (let ((preds qjp-leader-exempt-predicates))
       (while preds
         (when (funcall (car preds))
           (throw 'disable t))
         (setq preds (cdr preds)))))))

;; here is the only difference from god-mode'
(defun qjp-leader-mode-lookup-key-sequence (&optional key key-string-so-far)
  (interactive)
  (let ((sanitized-key
         (if key-string-so-far (char-to-string (or key (read-event key-string-so-far)))
           (qjp-leader-mode-sanitized-key-string (or key (read-event key-string-so-far))))))
    (qjp-leader-mode-lookup-command
     (if (and key (member key qjp-leader-special) (null key-string-so-far))
         (progn
           (setq qjp-leader-literal-sequence t)
           (format "C-c %c" key))
       (qjp-leader-key-string-after-consuming-key sanitized-key key-string-so-far)))))

;; evil integration

(evil-define-state qjp-leader
  "Qjp-leader state."
  :tag " <Q> "
  :message "-- QJP-LEADER MODE --"
  :entry-hook (evil-qjp-leader-start-hook)
  :exit-hook (evil-qjp-leader-stop-hook)
  :input-method t
  :intercept-esc nil)

(defun evil-qjp-leader-start-hook ()
  "Run before entering `evil-qjp-leader-state'."
  (qjp-leader-local-mode 1)
  (evil-visual-contract-region))

(defun evil-qjp-leader-stop-hook ()
  "Run before exiting `evil-qjp-leader-state'."
  (qjp-leader-local-mode -1))

(defvar evil-execute-in-qjp-leader-state-buffer nil)

(defvar evil-qjp-leader-last-command nil)

(defun evil-qjp-leader-fix-last-command ()
  "Change `last-command' to be the command before `evil-execute-in-qjp-leader-state'."
  (setq last-command evil-qjp-leader-last-command)
  (setq last-repeatable-command last-command))

(defun evil-execute-in-qjp-leader-state ()
  "Execute the next command in qjp-leader state."
  (interactive)
  (add-hook 'pre-command-hook #'evil-qjp-leader-fix-last-command t)
  (add-hook 'post-command-hook #'evil-stop-execute-in-qjp-leader-state t)
  (setq evil-execute-in-qjp-leader-state-buffer (current-buffer))
  (setq evil-qjp-leader-last-command last-command)
  (cond
   ((evil-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (evil-qjp-leader-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (evil-qjp-leader-state)))
  (evil-echo "Switched to qjp-leader state for the next command ..."))

(defun evil-stop-execute-in-qjp-leader-state ()
  "Switch back to previous evil state."
  (unless (or (eq this-command #'evil-execute-in-qjp-leader-state)
              (eq this-command #'universal-argument)
              (eq this-command #'universal-argument-minus)
              (eq this-command #'universal-argument-more)
              (eq this-command #'universal-argument-other-key)
              (eq this-command #'digit-argument)
              (eq this-command #'negative-argument)
              (minibufferp))
    (remove-hook 'pre-command-hook 'evil-qjp-leader-fix-last-command)
    (remove-hook 'post-command-hook 'evil-stop-execute-in-qjp-leader-state)
    (when (buffer-live-p evil-execute-in-qjp-leader-state-buffer)
      (with-current-buffer evil-execute-in-qjp-leader-state-buffer
        (if (and (eq evil-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (evil-change-to-previous-state)
              (evil-exit-visual-state))
          ;; qjp: fix wierd back-to-visual-state cursor behavior
          (when (and (use-region-p) (= (point) (region-end)))
            (backward-char))
          (evil-change-to-previous-state))))
    (setq evil-execute-in-qjp-leader-state-buffer nil)))

;;; Unconditionally exit Evil-qjp-leader state.
(defun evil-qjp-leader-state-bail ()
  "Stop current qjp-leader command and exit qjp-leader state."
  (interactive)
  (evil-stop-execute-in-qjp-leader-state)
  (evil-qjp-leader-stop-hook)
  (evil-normal-state))

;; which-key
(defvar which-key--qjp-leader-mode-key-string nil)
(defvar which-key--qjp-leader-mode-support-enabled t)

(defadvice qjp-leader-mode-lookup-command
    (around which-key--qjp-leader-mode-lookup-command-advice disable)
  (setq which-key--qjp-leader-mode-key-string (ad-get-arg 0))
  (unwind-protect
      ad-do-it
    (when (bound-and-true-p which-key-mode)
      (which-key--hide-popup))))
(with-eval-after-load 'which-key
  (defun which-key--update ()
    "Function run by timer to possibly trigger `which-key--create-buffer-and-show'."
    (let ((prefix-keys (this-single-command-keys))
          delay-time)
      ;; (when (> (length prefix-keys) 0)
      ;;  (message "key: %s" (key-description prefix-keys)))
      ;; (when (> (length prefix-keys) 0)
      ;;  (message "key binding: %s" (key-binding prefix-keys)))
      ;; Taken from guide-key
      (when (and (equal prefix-keys [key-chord])
                 (bound-and-true-p key-chord-mode))
        (setq prefix-keys
              (condition-case nil
                  (let ((rkeys (recent-keys)))
                    (vector 'key-chord
                            ;; Take the two preceding the last one, because the
                            ;; read-event call in key-chord seems to add a
                            ;; spurious key press to this list. Note this is
                            ;; different from guide-key's method which didn't work
                            ;; for me.
                            (aref rkeys (- (length rkeys) 3))
                            (aref rkeys (- (length rkeys) 2))))
                (error (progn
                         (message "which-key error in key-chord handling")
                         [key-chord])))))
      (when (and which-key--qjp-leader-mode-support-enabled
                 (bound-and-true-p qjp-leader-local-mode)
                 (eq this-command 'qjp-leader-mode-self-insert))
        (setq prefix-keys (when which-key--qjp-leader-mode-key-string
                            (kbd which-key--qjp-leader-mode-key-string))))
      (cond ((and (> (length prefix-keys) 0)
                  (or (keymapp (key-binding prefix-keys))
                      ;; Some keymaps are stored here like iso-transl-ctl-x-8-map
                      (keymapp (which-key--safe-lookup-key
                                key-translation-map prefix-keys))
                      ;; just in case someone uses one of these
                      (keymapp (which-key--safe-lookup-key
                                function-key-map prefix-keys)))
                  (not which-key-inhibit)
                  (or (null which-key-allow-regexps)
                      (which-key--any-match-p
                       which-key-allow-regexps (key-description prefix-keys)))
                  (or (null which-key-inhibit-regexps)
                      (not
                       (which-key--any-match-p
                        which-key-inhibit-regexps (key-description prefix-keys))))
                  ;; Do not display the popup if a command is currently being
                  ;; executed
                  (or (and which-key-allow-evil-operators
                           (bound-and-true-p evil-this-operator))
                      (and which-key--qjp-leader-mode-support-enabled
                           (bound-and-true-p qjp-leader-local-mode)
                           (eq this-command 'qjp-leader-mode-self-insert))
                      (null this-command)))
             (when (and (not (equal prefix-keys which-key--current-prefix))
                        (or (null which-key-delay-functions)
                            (null (setq delay-time (run-hook-with-args-until-success
                                                    'which-key-delay-functions
                                                    (key-description prefix-keys)
                                                    (length prefix-keys))))
                            (sit-for delay-time)))
               (which-key--create-buffer-and-show prefix-keys)
               (when (and which-key-idle-secondary-delay
                          (not which-key--secondary-timer-active))
                 (which-key--start-timer which-key-idle-secondary-delay t))))
            ((and which-key-show-operator-state-maps
                  (bound-and-true-p evil-state)
                  (eq evil-state 'operator)
                  (not which-key--using-show-operator-keymap))
             (which-key--show-evil-operator-keymap))
            ((and which-key--current-page-n
                  (not which-key--using-top-level)
                  (not which-key--using-show-operator-keymap)
                  (not which-key--using-show-keymap))
             (which-key--hide-popup)))))
  (ad-enable-advice
   'qjp-leader-mode-lookup-command
   'around 'which-key--qjp-leader-mode-lookup-command-advice)
  (ad-activate 'qjp-leader-mode-lookup-command))

(provide 'qjp-leader-mode)
;;; qjp-leader-mode.el ends here
