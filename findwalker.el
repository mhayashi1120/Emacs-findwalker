;;; findwalker.el --- find file utilities -*- lexical-binding: t -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: todo
;; URL: http://github.com/mhayashi1120/Emacs-findwalker/raw/master/findwalker.el
;; Version: 0.1.2

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; You can use `find' option like S Expression.
;; Provides easy way of editing find complex arguments and to display
;; full command-line to echo area.
;;

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'findwalker)

;;; Usage:

;; * Following command open editable buffer.
;;
;;    M-x findwalker
;;
;; * You can edit `find' command-line option by s-expression like following.
;;
;; (or (name "HOGE") (type d)) (type f)
;;   => find . \( -name HOGE -or -type d \) -type f 
;;
;; (prune (or (name ".svn") (name ".git"))) (type f) (name "*.el")
;;   => find . \( \( -name .svn -or -name .git \) -prune -or -true \) -type f -name \*.el

;; Type C-j testing execute above command and display command output.
;; Type C-c C-c execute command and switch to that buffer.
;; Type C-c C-q quit editing.
;; Type M-n, M-p move history when exists.
;;
;; * TODO in result buffer

;;; History:


;;; TODO:

;; * Can call function.
;; * Describe how to call command.
;; * cleanup buffer.
;; * describe how to use. (command sequence)
;; * findwalker step back
;; * can't keep `+' before number.
;; * show help

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup findwalker nil
  "Find command line user interface extensions"
  :prefix "findwalker-"
  :group 'applications)

(require 'compile)
(require 'find-cmd)

(defvar find-program)
(defvar xargs-program)

(defface findwalker-file-face
  '((t :inherit compilation-info))
  "Face used to highlight compiler information."
  :group 'findwalker)

(defcustom findwalker-setup-hook nil
  "TODO"
  :group 'findwalker
  :type 'hook)

(defcustom findwalker-grep-program "grep"
  ;; Don't use `grep-program' that may indicate sub-species `lgrep' `ack'
  "Path to grep program."
  :group 'findwalker
  :type 'file)

(defun findwalker-read-grep-command ()
  (progn
    (grep-compute-defaults)
    (let ((default (grep-default-command)))
      (list (read-shell-command
             "Run grep to listing files (like this): "
             (if current-prefix-arg default grep-command)
             'grep-history
             (if current-prefix-arg nil default))))))

;;;
;;; Listing find
;;;

(defvar findwalker-list-mode-map nil)

(let ((map (or findwalker-list-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\C-f" 'findwalker-list-find-more)
  (define-key map "\C-c\el" 'findwalker-list-ungrep-more)
  (define-key map "\C-c\C-l" 'findwalker-list-grep-more)
  (define-key map "\C-c\eg" 'findwalker-list-call-grep)
  
  ;; (define-key map "\C-c!" 'findwalker-list-invoke-xargs-more)
  (define-key map "\C-c\e|" 'findwalker-list-shell-command)
  ;; (define-key map "\C-c\eg" 'findwalker-list-invoke-grep)

  (setq findwalker-list-mode-map map))

(defun findwalker-list-call-grep (grep-command)
  (interactive (findwalker-read-grep-command))
  (findwalker-list-invoke-grep grep-command))

(defun findwalker-list-call-shell ()
  (interactive)
  (error "Not implement yet"))

(defun findwalker-list-call-function ()
  (interactive "aFunction: ")
  (error "Not implement yet"))

(defun findwalker-list-grep-more (regexp)
  (interactive "sGrep regexp: \nP")
  (findwalker-list-invoke-xargs-more
   (format "%s -l -e %s" 
           findwalker-grep-program
           (shell-quote-argument regexp))))

(defun findwalker-list-ungrep-more (regexp)
  (interactive "sUngrep regexp: ")
  (findwalker-list-invoke-xargs-more 
   (format "%s -L -e %s"
           findwalker-grep-program
           (shell-quote-argument regexp))))

;;TODO
(defun findwalker-list-find-more ()
  (interactive)
  (error "Not implement yet")
  (findwalker-list-invoke-xargs-more 
   (format "%s " find-program regexp)))

;;TODO
(defun findwalker-list-shell-more ()
  (interactive)
  (error "Not implement yet"))

(defun findwalker-list-invoke-grep (grep)
  "Execute `grep' on listed files."
  (let* ((file (findwalker-list--create-local-file))
         (infile (shell-quote-argument file))
         (command (format "%s -e %s < %s" xargs-program grep infile))
         (buffer
          (save-window-excursion
            (grep command))))
    (findwalker-list--cleanup-temp-file buffer file)
    (set-window-buffer (selected-window) buffer)))

(defun findwalker-list-invoke-xargs-more (command &optional xargs-replace)
  (let ((file (findwalker-list--create-local-file)))
    (unless file
      (error "No valid file"))
    (let* ((infile (shell-quote-argument file))
           (xcommand (if xargs-replace
                         (format "%s --replace=%s -e %s < %s" 
                                 xargs-program xargs-replace command 
                                 infile)
                       (format "%s -e %s < %s" xargs-program command infile)))
           (buffer (compilation-start xcommand 'findwalker-mode))
           (proc (get-buffer-process buffer)))
      (findwalker-list--cleanup-temp-file buffer file)
      (set-window-buffer (selected-window) buffer))))

(defun findwalker-list--cleanup-temp-file (buffer temp-file)
  (with-current-buffer buffer
    (add-hook 'compilation-finish-functions
              (if (< emacs-major-version 24)
                  (lexical-let ((file temp-file))
                    (lambda (&rest _ignore)
                      (when (file-exists-p file)
                        (delete-file file))))
                (lambda (&rest _ignore)
                  (when (file-exists-p temp-file)
                    (delete-file temp-file)))) nil t)))

(defun findwalker-list--create-local-file ()
  (let* ((ovs (findwalker--get-overlays (point-min) (point-max)))
         (files (mapcar
                (lambda (ov)
                  (buffer-substring (overlay-start ov) (overlay-end ov)))
                ovs)))
    (when files
      (with-temp-buffer
        (dolist (file files)
          (insert file "\n"))
        (let ((tmp (make-temp-file "findwalker-"))
              (coding-system-for-write file-name-coding-system))
          (write-region (point-min) (point-max) tmp nil 'no-msg)
          tmp)))))

(defvar findwalker-list-inhibit-preparation nil)

(define-minor-mode findwalker-list-mode
  "TODO"
  :init-value nil
  :lighter " findwalker File List"
  :keymap findwalker-list-mode-map
  (let ((flag (buffer-modified-p))
        (inhibit-read-only t))
    (cond
     (findwalker-list-mode
      (unless findwalker-list-inhibit-preparation
        (findwalker-list--prepare-overlays)))
     (t
      (findwalker-list--remove-overlays)))
    (set-buffer-modified-p flag))
  (add-hook 'after-change-functions 
            'findwalker-list--after-change nil t))

(defun findwalker-list--after-change (start end _)
  (save-excursion
    (goto-char start)
    (let ((beg (line-beginning-position))
          (fin (save-excursion
                 (goto-char end)
                 (line-end-position))))
      (save-restriction
        (narrow-to-region beg fin)
        (findwalker-list--prepare-overlays)))))

(defun findwalker-list--prepare-overlays ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((start (line-beginning-position))
             (end (line-end-position))
             (line (buffer-substring start end))
             ovs)
        (cond
         ((file-exists-p line)
          (findwalker--create-overlay start end line))
         ((setq ovs (findwalker--get-overlays start end))
          (dolist (o ovs)
            (delete-overlay o)))))
      (forward-line 1))))

(defun findwalker-list--remove-overlays ()
  (let ((ovs (findwalker--get-overlays
              (point-min) (point-max))))
    (dolist (ov ovs)
      (delete-overlay ov))))

(defun findwalker--get-overlays (start end)
  (let ((ovs (overlays-in start end))
        res)
    (mapc
     (lambda (ov)
       (when (overlay-get ov 'findwalker-filename)
         (setq res (cons ov res))))
     ovs)
    (nreverse res)))

(defun findwalker--create-overlay (start end file)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'findwalker-filename file)
    (overlay-put ov 'face 'findwalker-file-face)
    ov))

;;;
;;; Editing find args
;;;

(defun findwalker-edit--all-methods ()
  (mapcar 
   (lambda (x) (symbol-name (car x)))
   find-constituents))

(defvar findwalker-edit-mode-map nil)

(let ((map (or findwalker-edit-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\C-j" 'findwalker-edit-find-dired)
  (define-key map "\C-c\C-k" 'findwalker-edit-quit)
  (define-key map "\C-c\C-q" 'findwalker-edit-quit)
  (define-key map "\C-c\C-c" 'findwalker-edit-done)
  (define-key map "\C-c\ew" 'findwalker-edit-kill-command)
  (define-key map "\C-j" 'findwalker-edit-try)
  (define-key map "\C-c\C-e" 'findwalker-edit-try-last-sexp)
  (define-key map "\M-p" 'findwalker-edit-previous-history)
  (define-key map "\M-n" 'findwalker-edit-next-history)

  ;;TODO
  (define-key map "\C-c?" 'findwalker-help)
  
  (setq findwalker-edit-mode-map map))

(defvar findwalker-edit-font-lock-keywords
  `(
    (,(concat "(" (regexp-opt (findwalker-edit--all-methods) t) "\\b") 
     (1 font-lock-function-name-face))
    ))

(defvar findwalker-edit-font-lock-defaults
  '(
    (findwalker-edit-font-lock-keywords)
    nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
    (font-lock-mark-block-function . mark-defun)
    (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function)
    ))

(defvar findwalker-edit--configuration-stack nil)
(defvar findwalker-edit--history nil)
(defvar findwalker-edit--history-position nil)
(defvar findwalker-edit--tried nil)
(defvar findwalker-edit--compile-buffer nil)
(defvar findwalker-edit--previous-buffer nil)
(defvar findwalker-edit-history-alist nil)

(define-derived-mode findwalker-edit-mode lisp-mode "findwalker Edit"
  "Major mode to build `find' command args by using `fsvn-cmd'"
  (set (make-local-variable 'after-change-functions) nil)
  (set (make-local-variable 'kill-buffer-hook) nil)
  (set (make-local-variable 'window-configuration-change-hook) nil)
  (set (make-local-variable 'completion-at-point-functions)
       (list 'findwalker-edit-completion-at-point))
  (set (make-local-variable 'font-lock-defaults)
       findwalker-edit-font-lock-defaults)
  (set (make-local-variable 'compilation-buffer-name-function)
       'findwalker-mode-buffer-name)
  (set (make-local-variable 'findwalker-edit--history-position) nil)
  (set (make-local-variable 'findwalker-edit--tried) nil)
  (set (make-local-variable 'findwalker-edit--compile-buffer) nil)
  (set (make-local-variable 'findwalker-edit--previous-buffer) nil)
  (set (make-local-variable 'eldoc-documentation-function)
       'findwalker-edit--print-command)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (findwalker-edit--ac-initialize)
  (add-hook 'after-change-functions 'findwalker-edit--show-command nil t)
  (add-hook 'kill-buffer-hook 'findwalker-edit--cleanup nil t)
  (use-local-map findwalker-edit-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

(defun findwalker-edit-previous-history ()
  "Replace current editing contents to previous history."
  (interactive)
  (findwalker-edit-goto-history t))

(defun findwalker-edit-next-history ()
  "Replace current editing contents to next history."
  (interactive)
  (findwalker-edit-goto-history nil))

(defun findwalker-edit-done ()
  "Execute `find' with editing args."
  (interactive)
  (let* ((find-args (findwalker-edit--args t)))
    (cond
     ((or (null find-args)
          (not (equal findwalker-edit--tried find-args)))
      (findwalker-edit--start
       (format "%s %s" find-program 
               (findwalker--join find-args)) t)))
    (findwalker-edit--done-window)
    (findwalker-edit--add-history)))

(defun findwalker-edit-quit ()
  "Quit editing."
  (interactive)
  (findwalker-edit--add-history)
  (mapc
   (lambda (buffer)
     (when (and buffer (buffer-live-p buffer))
       (kill-buffer buffer)))
   (list (current-buffer)
         ;;TODO no need to kill?
         findwalker-edit--compile-buffer))
  (findwalker-edit--restore-window))

(defun findwalker-edit-try ()
  "Execute `find' with editing args."
  (interactive)
  (let ((find-args (findwalker-edit--args t)))
    (findwalker-edit--start
     (format "%s %s" find-program 
             (findwalker--join find-args)) t)
    (setq findwalker-edit--tried find-args)
    (findwalker-edit--try-window)
    (findwalker-edit--add-history)))

(defun findwalker-edit-try-last-sexp ()
  "Try last sexp before point."
  (interactive)
  (let ((sexp (preceding-sexp)))
    (unless (and sexp (listp sexp))
      (error "Invalid sexp `%s'" sexp))
    (let* ((stringified (findwalker--stringify sexp))
           (find-args (findwalker-edit--compile-args (list stringified))))
      (findwalker-edit--start
       (format "%s %s" find-program
               (findwalker--join find-args)) t)
      (findwalker-edit--try-window))))

(defun findwalker-edit-find-dired ()
  "Execute `find-dired'."
  (interactive)
  (let ((edit-buffer (current-buffer))
	(find-args (findwalker-edit--args-string t)))
    (save-window-excursion
      (find-dired default-directory find-args))
    (findwalker-edit--done-window t)))

(defun findwalker-edit-kill-command ()
  "Append current command line to `kill-ring'"
  (interactive)
  (let ((command (findwalker-edit--current-command)))
    (kill-new command)
    (message "%s" command)))

(defun findwalker-edit--add-history ()
  (let* ((contents
          (buffer-substring-no-properties
           (point-min) (point-max)))
         (key (directory-file-name default-directory))
         (pair (or (assoc key findwalker-edit-history-alist)
                   (let ((tmp (cons key nil)))
                     (setq findwalker-edit-history-alist
                           (cons tmp findwalker-edit-history-alist))
                     tmp))))
    (unless (string= contents "")
      (add-to-history 'findwalker-edit--history contents)
      (setcdr pair contents))
    contents))

(defun findwalker-edit--start (command &optional force-kill)
  (when force-kill
    (let* ((buffer findwalker-edit--compile-buffer)
           (proc (and buffer (get-buffer-process buffer))))
      (when proc
        (kill-process proc)
        (delete-process proc))))
  (save-window-excursion
    (let* ((buf (compilation-start command 'findwalker-mode)))
      (setq findwalker-edit--compile-buffer buf)
      buf)))

(defun findwalker-edit--clear-window-settings ()
  (setq findwalker-edit--configuration-stack nil))

(defun findwalker-edit--pop-window-setting ()
  (let ((top (car findwalker-edit--configuration-stack)))
    (when top
      (setq findwalker-edit--configuration-stack
            (cdr findwalker-edit--configuration-stack))
      top)))

(defun findwalker-edit--push-window-setting (setting)
  (setq findwalker-edit--configuration-stack
        (cons setting findwalker-edit--configuration-stack)))

(defun findwalker-edit--done-window (&optional find-dired?)
  (let* ((buffer findwalker-edit--compile-buffer)
         (win (and buffer (get-buffer-window buffer))))
    (when win
      (delete-window win))
    (set-window-buffer 
     (selected-window)
     (if find-dired?
         (get-buffer "*Find*")
       buffer))))

(defun findwalker-edit--try-window ()
  (let* ((ewin (selected-window))
         (buffer findwalker-edit--compile-buffer)
         (rwin (and buffer (get-buffer-window buffer))))
    (unless rwin
      (setq rwin (split-window))
      (set-window-buffer rwin buffer)
      (set-window-text-height ewin window-min-height))))

;;TODO how to handle undo tree
(defun findwalker-edit-goto-history (previous)
  (let ((n (funcall (if previous '1+ '1-) (or findwalker-edit--history-position -1))))
    (cond
     ((or (null findwalker-edit--history)
	  (< n 0))
      (message "No more history"))
     ((> n (1- (length findwalker-edit--history)))
      (message "No more history"))
     (t
      (erase-buffer)
      (insert (nth n findwalker-edit--history))
      (setq findwalker-edit--history-position n)))))



(defun findwalker-edit--cleanup ()
  (unless (minibufferp)
    (findwalker-edit--restore-window)))

(defun findwalker-edit--restore-window ()
  (let ((setting (findwalker-edit--pop-window-setting)))
    (when (window-configuration-p setting)
      (set-window-configuration setting))))


;;TODO concatenate other program output to find.
;; ex:
;; dpkg -L some-package | xargs --max-args=1 -I \{\} -e find \{\} -type f -maxdepth 0 -print0 | xargs -0 -e grep -nH -e "word"

;; TODO use shell command output as file list.
;; ex:
;; dpkg -L some-package

;; (defvar findwalker-select-grep-history nil)

;; (defun findwalker-select-read-grep-command (prompt)
;;   (let ((merged
;;          (append
;;           (mapcar 
;;            (lambda (x)
;;              (and (string-match "grep\\b.*" x)
;;                   (match-string 0 x)))
;;            grep-find-history)
;;           grep-history
;;           findwalker-select-grep-history)))
;;     (setq findwalker-select-grep-history merged)
;;   (read-from-minibuffer prompt
;;                         (car findwalker-select-grep-history) nil nil 
;;                         '(findwalker-select-grep-history . 1))))

;; (defun findwalker-select-functions (arg-length)
;;   (let (res)
;;     (mapatoms
;;      (lambda (x)
;;        (when (functionp x)
;;          (let ((func (symbol-function x)))
;;            (cond
;;             ((symbolp func)
;;              ;;TODO
;;              (indirect-function func))
;;             ;;TODO
;;             ;; ((functionp func)
;;             ;;  (setq res (cons x res)))
;;             ((subrp func)
;;              (let ((arity (subr-arity func)))
;;                (when (and (<= (car arity) arg-length)
;;                           (or (eq (cdr arity) 'many)
;;                               (<= arg-length (cdr arity))))
;;                  (setq res (cons x res)))))))))
;;      obarray)
;;     res))

;; (defun findwalker-select-read-function ()
;;   (let (collection tmp)
;;     (mapatoms
;;      (lambda (s)
;;        (when (fboundp s)
;;          (when (= (findwalker-select-function-min-arg s) 1)
;;            (setq collection (cons s collection)))))
;;      obarray)
;;     (setq tmp (completing-read "Function (one arg): " collection nil t nil nil))
;;     (if tmp
;; 	(intern tmp)
;;       'identity)))

;; (defun findwalker-select-function-min-arg (symbol)
;;   (let* ((f (symbol-function symbol))
;;          (len 0))
;;     (cond
;;      ((subrp f)
;;       (setq len (car (subr-arity f))))
;;      (t
;;       (catch 'done
;;         (let ((args (help-function-arglist f)))
;;           (when (listp args)
;;             (mapc
;;              (lambda (a)
;;                (when (memq a '(&optional &rest))
;;                  (throw 'done t))
;;                (setq len (1+ len)))
;;              args))))))
;;     len))

(defun findwalker-edit--current-command ()
  (let ((dir (abbreviate-file-name default-directory))
        (args (findwalker-edit--args-string)))
    (format "%s %s %s"
            find-program dir args)))

(defun findwalker-edit--show-command (&rest dummy)
  (condition-case nil
      (let ((dir (abbreviate-file-name default-directory))
	    (rwin (get-buffer-window findwalker-edit--compile-buffer))
            (ewin (selected-window))
            args parse-error)
	(condition-case err
	    (setq args (findwalker-edit--args-string))
	  (error (setq parse-error err)))
        (let (message-log-max)
          (cond
           ((plusp (length args))
            (message "%s %s %s"
                     (propertize find-program 'face font-lock-function-name-face)
                     (propertize dir 'face font-lock-constant-face)
                     (propertize args 'face font-lock-variable-name-face)))
           (parse-error
            (message "%s"
                     (propertize (format "%s" parse-error)
                                 'face font-lock-warning-face))))))
    ;; ignore all
    (error nil)))

(defun findwalker-edit--print-command ()
  (when (derived-mode-p 'findwalker-edit-mode)
    (unless (current-message)
      (findwalker-edit--show-command))))

(defun findwalker-edit--args-string (&optional inhibit-partial)
  (let ((args (findwalker-edit--args inhibit-partial)))
    (findwalker--join args)))

(defun findwalker-edit--args (&optional inhibit-partial)
  (let* ((subfinds (findwalker-edit--read-expressions inhibit-partial)))
    (findwalker-edit--compile-args subfinds)))

(defun findwalker-edit--compile-args (args)
  (apply 
   'append
   (mapcar
    (lambda (arg)
      ;; item is a string (ex: "-type f" "-name \\*\\ \\*.el")
      (let* ((item (find-to-string arg))
             (sarg (split-string item " " t)))
        (list
         (car sarg)
         (findwalker--join (cdr sarg)))))
    args)))

(defun findwalker-edit--read-expressions (&optional inhibit-partial)
  (let (exp subfinds)
    (save-excursion
      (goto-char (point-min))
      (condition-case err
	  (while (and (progn 
                        (skip-chars-forward "[ \t\n]")
                        (not (eobp)))
                      (setq exp (read (current-buffer))))
            (unless (listp exp)
              (signal 'invalid-read-syntax
                      (list (format "Non list `%s' is not allowed" exp))))
            (let ((stringified (findwalker--stringify exp)))
              (setq subfinds (cons stringified subfinds))))
	(end-of-file
         (when inhibit-partial
           (signal (car err) (cdr err))))))
    (nreverse subfinds)))

(defun findwalker--join (args)
  (mapconcat 'identity args " "))

(defvar findwalker-number-plus-notation
  ;; append `+' if following expression is a number.
  '(amin atime cmin ctime mmin mtime size))

;; stringify argument
(defun findwalker--stringify (sexp)
  (cond
   ((and (listp sexp)
         (listp (cdr sexp)))
    (let ((cmd (car sexp)))
      (cons
       ;; find arg (ex -type -mindepth)
       cmd
       (mapcar
        (lambda (s)
          (cond
           ((numberp s)
            (cond
             ((and (plusp s)
                   (memq cmd findwalker-number-plus-notation))
              (concat "+" (number-to-string s)))
             (t
              (number-to-string s))))
           ((symbolp s)
            (symbol-name s))
           ((listp s)
            (findwalker--stringify s))
           (t s)))
        (cdr sexp)))))
   (t sexp)))

(defun findwalker-edit-completion-at-point ()
  (with-syntax-table lisp-mode-syntax-table
    (let* ((pos (point))
           (beg (condition-case nil
                    (save-excursion
                      (backward-sexp 1)
                      (skip-syntax-forward "'")
                      (point))
                  (scan-error pos)))
           (end (point))
           (col (mapcar 
                 (lambda (x)
                   (cons (symbol-name (car x)) (cdr x)))
                 find-constituents)))
      `(,beg ,end ,col))))

;;TODO not works?
(defun findwalker-edit--ac-initialize ()
  (dont-compile
    (when (featurep 'auto-complete)

      (ac-define-source findwalker-constituents
        '((candidates . findwalker-edit--all-methods)
          (symbol . "s")
          (prefix . "(\\(?:\\(?:\\sw\\|\\s_\\)*\\)")
          (requires . 1)
          (cache)))

      (setq ac-sources '(ac-source-findwalker-constituents))
      (set (make-local-variable 'ac-modes)
           `(,major-mode))
      (auto-complete-mode 1))))

;;;###autoload
(defun findwalker ()
  ;; execute find and display command-line to buffer.
  ;; -> electric mode?
  ;; execute buffer buffer with call-process-region
  ;;TODO clear stack
  (interactive)
  (unless (eq major-mode 'findwalker-edit-mode)
    (let ((buffer (get-buffer-create "*Findwalker Edit*"))
          (prev (current-buffer))
          (setting (current-window-configuration))
          (dir default-directory))
      (findwalker-edit--clear-window-settings)
      (with-current-buffer buffer
        (cd dir)
        (findwalker-edit-mode)
        (setq findwalker-edit--previous-buffer prev)
        (findwalker-edit--push-window-setting setting))
      (delete-other-windows)
      (let ((new-win (split-window)))
        (set-window-buffer new-win buffer)
        (select-window new-win))
      (message (substitute-command-keys 
                (concat "Type \\[findwalker-edit-try] to test expression, "
                        "\\[findwalker-edit-done] to execute find, "
                        "\\[findwalker-edit-quit] to quit edit."))))))

;; TODO save buffer contents to history
;; * some shell command buffer
;; **  *shell command* dpkg -L some-package
;;    to narrow the find result
;;   clear stack
;; * findwalker-mode buffer
;; * output result is nothing message

(defun findwalker-mode-buffer-name (dummy)
  (or (and findwalker-edit--compile-buffer
           (buffer-name findwalker-edit--compile-buffer))
      (generate-new-buffer-name "findwalker")))

;;;
;;; compilation variables (before `define-compilation-mode')
;;;

(defvar findwalker-mode-font-lock-keywords
  `(("^Find started at.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^Find finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 compilation-info-face nil t)
     (2 compilation-warning-face nil t))
    (,(concat "^Find \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)"
              "\\(?:.*with code \\([0-9]+\\)\\)?.*")
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 compilation-warning-face)
     (2 compilation-warning-face nil t))
    ;; buffer header file-local-variable
    ("\\`-\\*- mode:.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ;; error messages
    ("^find:\\(.*\\)"
     (1 '(face compilation-error compilation-message nil help-echo nil mouse-face nil)))
    )
  "Additional things to highlight in find output.
This gets tacked on the end of the generated expressions.")

(defun findwalker-process-setup ()
  "Setup compilation variables and buffer for `find'.
Set up `compilation-exit-message-function' and run `findwalker-setup-hook'."
  ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     ;; This relies on the fact that `compilation-start'
	     ;; sets buffer-modified to nil before running the command,
	     ;; so the buffer is still unmodified if there is no output.
	     (cond ((and (zerop code) (buffer-modified-p))
		    '("finished (matches found)\n" . "matched"))
		   ((not (buffer-modified-p))
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code))))
  (setq findwalker--filterd-point (point))
  (run-hooks 'findwalker-setup-hook))

(defvar findwalker--filterd-point nil
  "TODO `compilation-filter-start' is prepared after version 24.")

(defun findwalker--compilation-filter ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char findwalker--filterd-point)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        (while (looking-at ".*\n")
          (let* ((start (line-beginning-position))
                 (fin (line-end-position))
                 (line (buffer-substring start fin)))
            (cond
             ((and (file-exists-p line)
                   (not (findwalker--get-overlays start fin)))
              (findwalker--create-overlay start fin line))))
          (forward-line 1))
        (setq findwalker--filterd-point (point))))))

(defun findwalker-process-finish (buffer &rest ignore)
  (kill-local-variable 'findwalker--filterd-point)
  (let ((findwalker-list-inhibit-preparation t))
    (findwalker-list-mode 1))
  (set-buffer-modified-p nil)
  (setq buffer-read-only nil))

(define-compilation-mode findwalker-mode "Find"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  ;; (setq grep-last-buffer (current-buffer))
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-error-face)
       'findwalker-file-face)
  ;; to point the filename `compile-goto-error'
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^\\([.].+\\)" 1) ("^\\(/.+\\)" 1)))
  (set (make-local-variable 'compilation-process-setup-function)
       'findwalker-process-setup)
  (if (< emacs-major-version 23)
      (set (make-local-variable 'compilation-finish-function)
           'findwalker-process-finish)
    (set (make-local-variable 'compilation-finish-functions)
         (list 'findwalker-process-finish)))
  (set (make-local-variable 'buffer-undo-list) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'findwalker--filterd-point) nil)
  (add-hook 'compilation-filter-hook 'findwalker--compilation-filter nil t))



(defvar findwalker--man-buffer nil)
(defcustom findwalker-man-locale nil
  "todo"
  :group 'findwalker
  :type 'string)

(defun findwalker--invoke-man ()
  (let ((process-environment (copy-sequence process-environment)))
    (when findwalker-man-locale
      (setenv "LANG" findwalker-man-locale))
    ;;TODO
    (require 'man)
    (call-process manual-program
                  nil t nil (file-name-nondirectory find-program))))

(defun findwalker--get-man-buffer ()
  (or (and findwalker--man-buffer
           (buffer-live-p findwalker--man-buffer)
           findwalker--man-buffer)
      (let ((buf (generate-new-buffer " *findwalker Man find* ")))
        (with-current-buffer buf
          (findwalker--invoke-man))
        (setq findwalker--man-buffer buf))))

(defun findwalker--popup-loop (buf)
  (let* ((win-config (current-window-configuration))
         (win (split-window-horizontally))
         (echo-keystrokes 0)
         done)
    (set-window-buffer win buf)
    (while (not done)
      (let* ((orig-event
              (let ((inhibit-quit t))
                (read-event))))
        (cond
         ((memq orig-event '(space ?\s))
          (save-selected-window
            (select-window win)
            (if (pos-visible-in-window-p (point-max))
                (goto-char (point-min))
              (scroll-up))))
         ((memq orig-event '(backspace ?\d))
          (save-selected-window
            (select-window win)
            (if (pos-visible-in-window-p (point-min))
                (goto-char (point-max))
              (scroll-down))))
         (t
          (set-window-configuration win-config)
          (if (memq orig-event '(space 32))
              (bury-buffer buf)
            (setq unread-command-events (list orig-event)))
          (setq done t)))))))

(defun findwalker--popup-colorize (finders)
  (save-excursion
    (remove-overlays)
    (dolist (f finders)
      (goto-char (point-min))
      (destructuring-bind (regexp subexp) f
        (while (re-search-forward regexp nil t)
          ;;TODO subexp
          (let* ((beg (match-beginning subexp))
                 (fin (match-end subexp))
                 (ov (make-overlay beg fin)))
            (overlay-put ov 'face 'match)))))))

(defun findwalker--popup-man (arg)
  (let ((buf (findwalker--get-man-buffer)))
    (with-current-buffer buf
      (findwalker--popup-colorize 
       (list
        (list (concat "\\(-" (regexp-quote arg) "\\)") 1)
        ))
      (goto-char (point-min))
      (re-search-forward (format "^ \\{7\\}-%s" arg) nil t))
    (findwalker--popup-loop buf)))

(defun findwalker--read-man-page ()
  (with-temp-buffer
    (findwalker--invoke-man)
    (findwalker--parse-man-page)))

(defun findwalker--parse-man-page ()
  ;;TODO
  )


(defun findwalker-help ()
  (interactive)
  ;;TODO
  (findwalker--popup-man
   (thing-at-point 'word)))



;; TODO after load?
(when (boundp 'session-globals-include)
  (add-to-list 'session-globals-include
               '(findwalker-edit-history-alist 50)))

(provide 'findwalker)

;;; findwalker.el ends here
