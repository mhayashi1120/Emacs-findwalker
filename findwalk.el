;;; findwalk.el --- find file utilities -*- lexical-binding: t -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: find command result xargs
;; URL: TODO http://github.com/mhayashi1120/Emacs-find-select/raw/master/findwalk.el
;; Version: 0.1.1

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
;;     (require 'findwalk)

;; ** In Emacs 22 or earlier **
;; Not tested. But to install find-cmd.el from following url may work.
;; http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/find-cmd.el

;;; Usage:

;; * Following command open editable buffer.
;;
;;    M-x findwalk
;;
;; * You can edit `find' command-line option by s-expression like following.
;;
;; (or (name "HOGE") (type "d")) (type "f")
;;
;; This expand to 
;;
;; find . \( -name HOGE -or -type d \) -type f 
;;
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
;; * Can complete symbol. auto-complete.el?
;; * cleanup buffer.
;; * describe how to use. (command sequence)
;; * findwalk step back

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup findwalk nil
  "Find command line user interface extensions"
  ;;TODO
  :group 'applications)

(require 'compile)
(require 'find-cmd)

(defvar find-program)
(defvar xargs-program)

(defface findwalk-file-face
  '((t :inherit compilation-info))
  "Face used to highlight compiler information."
  :group 'findwalk)

(defcustom findwalk-setup-hook nil
  "TODO"
  :group 'findwalk
  :type 'hook)

(defcustom findwalk-grep-program "grep"
  ;; Don't use `grep-program' that may indicate sub-species `lgrep' `ack'
  "TODO grep program path"
  :group 'findwalk
  :type 'file)

(defun findwalk-read-grep-command ()
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

(defvar findwalk-list-mode-map nil)

(let ((map (or findwalk-list-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\C-f" 'findwalk-list-find-more)
  (define-key map "\C-c\el" 'findwalk-list-ungrep-more)
  (define-key map "\C-c\C-l" 'findwalk-list-grep-more)
  (define-key map "\C-c\eg" 'findwalk-list-call-grep)
  
  ;; (define-key map "\C-c!" 'findwalk-list-invoke-xargs-more)
  (define-key map "\C-c\e|" 'findwalk-list-shell-command)
  ;; (define-key map "\C-c\eg" 'findwalk-list-invoke-grep)

  (setq findwalk-list-mode-map map))

(defun findwalk-list-call-grep (grep-command)
  (interactive (findwalk-read-grep-command))
  (findwalk-list-invoke-grep grep-command))

(defun findwalk-list-call-shell ()
  (interactive)
  (error "Not implement yet"))

(defun findwalk-list-call-function ()
  (interactive "aFunction: ")
  (error "Not implement yet"))

(defun findwalk-list-grep-more (regexp)
  (interactive "sGrep regexp: \nP")
  (findwalk-list-invoke-xargs-more
   (format "%s -l -e %s" 
           findwalk-grep-program
           (shell-quote-argument regexp))))

(defun findwalk-list-ungrep-more (regexp)
  (interactive "sUngrep regexp: ")
  (findwalk-list-invoke-xargs-more 
   (format "%s -L -e %s"
           findwalk-grep-program
           (shell-quote-argument regexp))))

;;TODO
(defun findwalk-list-find-more ()
  (interactive)
  (error "Not implement yet")
  (findwalk-list-invoke-xargs-more 
   (format "%s " find-program regexp)))

;;TODO
(defun findwalk-list-shell-more ()
  (interactive)
  (error "Not implement yet"))

(defun findwalk-list-invoke-grep (grep)
  "Execute `grep' on listed files."
  (let* ((file (findwalk-list--create-local-file))
         (infile (shell-quote-argument file))
         (command (format "%s -e %s < %s" xargs-program grep infile))
         (buffer
          (save-window-excursion
            (grep command))))
    (findwalk-list--cleanup-temp-file buffer file)
    (set-window-buffer (selected-window) buffer)))

(defun findwalk-list-invoke-xargs-more (command &optional xargs-replace)
  (let ((file (findwalk-list--create-local-file)))
    (unless file
      (error "No valid file"))
    (let* ((infile (shell-quote-argument file))
           (xcommand (if xargs-replace
                         (format "%s --replace=%s -e %s < %s" 
                                 xargs-program xargs-replace command 
                                 infile)
                       (format "%s -e %s < %s" xargs-program command infile)))
           (buffer (compilation-start xcommand 'findwalk-mode))
           (proc (get-buffer-process buffer)))
      (findwalk-list--cleanup-temp-file buffer file)
      (set-window-buffer (selected-window) buffer))))

(defun findwalk-list--cleanup-temp-file (buffer temp-file)
  (with-current-buffer buffer
    (add-hook 'compilation-finish-functions
              (if (< emacs-major-version 24)
                  (lexical-let ((file temp-file))
                    (lambda (&rest _ignore)
                      (delete-file file)))
                (lambda (&rest _ignore)
                  (delete-file temp-file))))))

(defun findwalk-list--create-local-file ()
  (let* ((ovs (findwalk--get-overlays (point-min) (point-max)))
         (files (mapcar
                (lambda (ov)
                  (buffer-substring (overlay-start ov) (overlay-end ov)))
                ovs)))
    (when files
      (with-temp-buffer
        (dolist (file files)
          (insert file "\n"))
        (let ((tmp (make-temp-file "findwalk-"))
              (coding-system-for-write file-name-coding-system))
          (write-region (point-min) (point-max) tmp nil 'no-msg)
          tmp)))))

(defvar findwalk-list-inhibit-preparation nil)

(define-minor-mode findwalk-list-mode
  "TODO"
  :init-value nil
  :lighter " Findwalk File List"
  :keymap findwalk-list-mode-map
  (let ((flag (buffer-modified-p))
        (inhibit-read-only t))
    (cond
     (findwalk-list-mode
      (unless findwalk-list-inhibit-preparation
        (findwalk-list--prepare-overlays)))
     (t
      (findwalk-list--remove-overlays)))
    (set-buffer-modified-p flag))
  (add-hook 'after-change-functions 
            'findwalk-list--after-change nil t))

(defun findwalk-list--after-change (start end _)
  (save-excursion
    (goto-char start)
    (let ((beg (line-beginning-position))
          (fin (save-excursion
                 (goto-char end)
                 (line-end-position))))
      (save-restriction
        (narrow-to-region beg fin)
        (findwalk-list--prepare-overlays)))))

(defun findwalk-list--prepare-overlays ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((start (line-beginning-position))
             (end (line-end-position))
             (line (buffer-substring start end))
             ovs)
        (cond
         ((file-exists-p line)
          (findwalk--create-overlay start end line))
         ((setq ovs (findwalk--get-overlays start end))
          (dolist (o ovs)
            (delete-overlay o)))))
      (forward-line 1))))

(defun findwalk-list--remove-overlays ()
  (let ((ovs (findwalk--get-overlays
              (point-min) (point-max))))
    (dolist (ov ovs)
      (delete-overlay ov))))

(defun findwalk--get-overlays (start end)
  (let ((ovs (overlays-in start end))
        res)
    (mapc
     (lambda (ov)
       (when (overlay-get ov 'findwalk-filename)
         (setq res (cons ov res))))
     ovs)
    (nreverse res)))

(defun findwalk--create-overlay (start end file)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'findwalk-filename file)
    (overlay-put ov 'face 'findwalk-file-face)
    ov))

;;;
;;; Editing find args
;;;

(defun findwalk-edit--all-methods ()
  (mapcar 
   (lambda (x) (symbol-name (car x)))
   find-constituents))

(defvar findwalk-edit-mode-map nil)

(let ((map (or findwalk-edit-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\C-j" 'findwalk-edit-find-dired)
  (define-key map "\C-c\C-k" 'findwalk-edit-quit)
  (define-key map "\C-c\C-q" 'findwalk-edit-quit)
  (define-key map "\C-c\C-c" 'findwalk-edit-done)
  (define-key map "\C-c\ew" 'findwalk-edit-kill-command)
  (define-key map "\C-j" 'findwalk-edit-try)
  (define-key map "\C-c\C-e" 'findwalk-edit-try-last-sexp)
  (define-key map "\M-p" 'findwalk-edit-previous-history)
  (define-key map "\M-n" 'findwalk-edit-next-history)

  (setq findwalk-edit-mode-map map))

(defvar findwalk-edit-font-lock-keywords
  `(
    (,(concat "(" (regexp-opt (findwalk-edit--all-methods) t) "\\b") 
     (1 font-lock-function-name-face))
    ))

(defvar findwalk-edit-font-lock-defaults
  '(
    (findwalk-edit-font-lock-keywords)
    nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
    (font-lock-mark-block-function . mark-defun)
    (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function)
    ))

(defvar findwalk-edit--configuration-stack nil)
(defvar findwalk-edit--history nil)
(defvar findwalk-edit--history-position nil)
(defvar findwalk-edit--tried nil)
(defvar findwalk-edit--compile-buffer nil)
(defvar findwalk-edit--previous-buffer nil)

(define-derived-mode findwalk-edit-mode lisp-mode "Findwalk Edit"
  "Major mode to build `find' command args by using `fsvn-cmd'"
  (set (make-local-variable 'after-change-functions) nil)
  (set (make-local-variable 'kill-buffer-hook) nil)
  (set (make-local-variable 'window-configuration-change-hook) nil)
  (set (make-local-variable 'completion-at-point-functions)
       (list 'findwalk-edit-completion-at-point))
  (set (make-local-variable 'font-lock-defaults)
       findwalk-edit-font-lock-defaults)
  (set (make-local-variable 'compilation-buffer-name-function)
       'findwalk-mode-buffer-name)
  (set (make-local-variable 'findwalk-edit--history-position) nil)
  (set (make-local-variable 'findwalk-edit--tried) nil)
  (set (make-local-variable 'findwalk-edit--compile-buffer) nil)
  (set (make-local-variable 'findwalk-edit--previous-buffer) nil)
  (set (make-local-variable 'eldoc-documentation-function)
       'findwalk-edit--print-command)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (findwalk-edit--ac-initialize)
  (add-hook 'after-change-functions 'findwalk-edit--show-command nil t)
  (add-hook 'kill-buffer-hook 'findwalk-edit--cleanup nil t)
  (use-local-map findwalk-edit-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

(defun findwalk-edit-previous-history ()
  "Replace current editing contents to previous history."
  (interactive)
  (findwalk-edit-goto-history t))

(defun findwalk-edit-next-history ()
  "Replace current editing contents to next history."
  (interactive)
  (findwalk-edit-goto-history nil))

(defun findwalk-edit-done ()
  "Execute `find' with editing args."
  (interactive)
  (let* ((find-args (findwalk-edit--args t)))
    (cond
     ((or (null find-args)
          (not (equal findwalk-edit--tried find-args)))
      (findwalk-edit--start
       (format "%s %s" find-program 
               (findwalk--join find-args)) t)))
    (findwalk-edit--done-window)
    (let ((contents
           (buffer-substring-no-properties
            (point-min) (point-max))))
      (add-to-history 'findwalk-edit--history contents))))

(defun findwalk-edit-quit ()
  "Quit editing."
  (interactive)
  (mapc
   (lambda (buffer)
     (when (and buffer (buffer-live-p buffer))
       (kill-buffer buffer)))
   (list (current-buffer)
         ;;TODO no need to kill?
         findwalk-edit--compile-buffer))
  (findwalk-edit--restore-window))

(defun findwalk-edit-try ()
  "Execute `find' with editing args."
  (interactive)
  (let* ((edit-buffer (current-buffer))
         (find-args (findwalk-edit--args t)))
    (findwalk-edit--start
     (format "%s %s" find-program 
             (findwalk--join find-args)) t)
    (setq findwalk-edit--tried find-args)
    (findwalk-edit--try-window)))

(defun findwalk-edit-try-last-sexp ()
  "Try last sexp before point."
  (interactive)
  (let ((sexp (preceding-sexp)))
    (unless (and sexp (listp sexp))
      (error "Invalid sexp `%s'" sexp))
    (let* ((stringified (findwalk--stringify sexp))
           (find-args (findwalk-edit--compile-args (list stringified))))
      (findwalk-edit--start
       (format "%s %s" find-program
               (findwalk--join find-args)) t)
      (findwalk-edit--try-window))))

(defun findwalk-edit-find-dired ()
  "Execute `find-dired'."
  (interactive)
  (let ((edit-buffer (current-buffer))
	(find-args (findwalk-edit--args-string t)))
    (save-window-excursion
      (find-dired default-directory find-args))
    (findwalk-edit--done-window t)))

(defun findwalk-edit-kill-command ()
  "Append current command line to `kill-ring'"
  (interactive)
  (let ((command (findwalk-edit--current-command)))
    (kill-new command)
    (message "%s" command)))

(defun findwalk-edit--start (command &optional force-kill)
  (when force-kill
    (let* ((buffer findwalk-edit--compile-buffer)
           (proc (and buffer (get-buffer-process buffer))))
      (when proc
        (kill-process proc)
        (delete-process proc))))
  (save-window-excursion
    (let* ((buf (compilation-start command 'findwalk-mode)))
      (setq findwalk-edit--compile-buffer buf)
      buf)))

(defun findwalk-edit--clear-window-settings ()
  (setq findwalk-edit--configuration-stack nil))

(defun findwalk-edit--pop-window-setting ()
  (let ((top (car findwalk-edit--configuration-stack)))
    (when top
      (setq findwalk-edit--configuration-stack
            (cdr findwalk-edit--configuration-stack))
      top)))

(defun findwalk-edit--push-window-setting (setting)
  (setq findwalk-edit--configuration-stack
        (cons setting findwalk-edit--configuration-stack)))

(defun findwalk-edit--done-window (&optional find-dired?)
  (let* ((buffer findwalk-edit--compile-buffer)
         (win (and buffer (get-buffer-window buffer))))
    (when win
      (delete-window win))
    (set-window-buffer 
     (selected-window)
     (if find-dired?
         (get-buffer "*Find*")
       buffer))))

(defun findwalk-edit--try-window ()
  (let* ((ewin (selected-window))
         (buffer findwalk-edit--compile-buffer)
         (rwin (and buffer (get-buffer-window buffer))))
    (unless rwin
      (setq rwin (split-window))
      (set-window-buffer rwin buffer)
      (set-window-text-height ewin window-min-height))))

;;TODO how to handle undo tree
(defun findwalk-edit-goto-history (previous)
  (let ((n (funcall (if previous '1+ '1-) (or findwalk-edit--history-position -1))))
    (cond
     ((or (null findwalk-edit--history)
	  (< n 0))
      (message "No more history"))
     ((> n (1- (length findwalk-edit--history)))
      (message "No more history"))
     (t
      (erase-buffer)
      (insert (nth n findwalk-edit--history))
      (setq findwalk-edit--history-position n)))))



(defun findwalk-edit--cleanup ()
  (findwalk-edit--restore-window))

(defun findwalk-edit--restore-window ()
  (let ((setting (findwalk-edit--pop-window-setting)))
    (when (window-configuration-p setting)
      (set-window-configuration setting))))


;;TODO concatenate other program output to find.
;; ex:
;; dpkg -L some-package | xargs --max-args=1 -I \{\} -e find \{\} -type f -maxdepth 0 -print0 | xargs -0 -e grep -nH -e "word"

;; TODO use shell command output as file list.
;; ex:
;; dpkg -L some-package

;; (defvar findwalk-select-grep-history nil)

;; (defun findwalk-select-read-grep-command (prompt)
;;   (let ((merged
;;          (append
;;           (mapcar 
;;            (lambda (x)
;;              (and (string-match "grep\\b.*" x)
;;                   (match-string 0 x)))
;;            grep-find-history)
;;           grep-history
;;           findwalk-select-grep-history)))
;;     (setq findwalk-select-grep-history merged)
;;   (read-from-minibuffer prompt
;;                         (car findwalk-select-grep-history) nil nil 
;;                         '(findwalk-select-grep-history . 1))))

;; (defun findwalk-select-functions (arg-length)
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

;; (defun findwalk-select-read-function ()
;;   (let (collection tmp)
;;     (mapatoms
;;      (lambda (s)
;;        (when (fboundp s)
;;          (when (= (findwalk-select-function-min-arg s) 1)
;;            (setq collection (cons s collection)))))
;;      obarray)
;;     (setq tmp (completing-read "Function (one arg): " collection nil t nil nil))
;;     (if tmp
;; 	(intern tmp)
;;       'identity)))

;; (defun findwalk-select-function-min-arg (symbol)
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

(defun findwalk-edit--current-command ()
  (let ((dir (abbreviate-file-name default-directory))
        (args (findwalk-edit--args-string)))
    (format "%s %s %s"
            find-program dir args)))

(defun findwalk-edit--show-command (&rest dummy)
  (condition-case nil
      (let ((dir (abbreviate-file-name default-directory))
	    (rwin (get-buffer-window findwalk-edit--compile-buffer))
            (ewin (selected-window))
            args parse-error)
	(condition-case err
	    (setq args (findwalk-edit--args-string))
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

(defun findwalk-edit--print-command ()
  (when (derived-mode-p 'findwalk-edit-mode)
    (unless (current-message)
      (findwalk-edit--show-command))))

(defun findwalk-edit--args-string (&optional inhibit-partial)
  (let ((args (findwalk-edit--args inhibit-partial)))
    (findwalk--join args)))

(defun findwalk-edit--args (&optional inhibit-partial)
  (let* ((subfinds (findwalk-edit--read-expressions inhibit-partial)))
    (findwalk-edit--compile-args subfinds)))

(defun findwalk-edit--compile-args (args)
  (apply 
   'append
   (mapcar
    (lambda (arg)
      ;; item is a string (ex: "-type f" "-name \\*\\ \\*.el")
      (let* ((item (find-to-string arg))
             (sarg (split-string item " " t)))
        (list
         (car sarg)
         (findwalk--join (cdr sarg)))))
    args)))

(defun findwalk-edit--read-expressions (&optional inhibit-partial)
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
            (let ((stringified (findwalk--stringify exp)))
              (setq subfinds (cons stringified subfinds))))
	(end-of-file
         (when inhibit-partial
           (signal (car err) (cdr err))))))
    (nreverse subfinds)))

(defun findwalk--join (args)
  (mapconcat 'identity args " "))

;; stringify argument
(defun findwalk--stringify (sexp)
  (cond
   ((and (listp sexp)
         (listp (cdr sexp)))
    (cons
     ;; find arg (ex -type -mindepth)
     (car sexp)
     (mapcar
      (lambda (s)
        (cond
         ((numberp s)
          (number-to-string s))
         ((symbolp s)
          (symbol-name s))
         ((listp s)
          (findwalk--stringify s))
         (t s)))
      (cdr sexp))))
   (t sexp)))

(defun findwalk-edit-completion-at-point ()
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
(defun findwalk-edit--ac-initialize ()
  (dont-compile
    (when (featurep 'auto-complete)

      (ac-define-source findwalk-constituents
        '((candidates . findwalk-edit--all-methods)
          (symbol . "s")
          (prefix . "(\\(?:\\(?:\\sw\\|\\s_\\)*\\)")
          (requires . 1)
          (cache)))

      (setq ac-sources '(ac-source-findwalk-constituents))
      (set (make-local-variable 'ac-modes)
           `(,major-mode))
      (auto-complete-mode 1))))

(defun findwalk ()
  ;; execute find and display command-line to buffer.
  ;; -> electric mode?
  ;; execute buffer buffer with call-process-region
  ;;TODO clear stack
  (interactive)
  (unless (eq major-mode 'findwalk-edit-mode)
    (let ((buffer (get-buffer-create "*Findwalk Edit*"))
          (prev (current-buffer))
          (setting (current-window-configuration))
          (dir default-directory))
      (findwalk-edit--clear-window-settings)
      (with-current-buffer buffer
        (cd dir)
        (findwalk-edit-mode)
        (setq findwalk-edit--previous-buffer prev)
        (findwalk-edit--push-window-setting setting))
      (delete-other-windows)
      (let ((new-win (split-window)))
        (set-window-buffer new-win buffer)
        (select-window new-win))
      (message (substitute-command-keys 
                (concat "Type \\[findwalk-edit-try] to test expression, "
                        "\\[findwalk-edit-done] to execute find, "
                        "\\[findwalk-edit-quit] to quit edit."))))))

;; TODO save buffer contents to history
;; * some shell command buffer
;; **  *shell command* dpkg -L some-package
;;    to narrow the find result
;;   clear stack
;; * findwalk-mode buffer
;; * output result is nothing message

(defun findwalk-mode-buffer-name (dummy)
  (or (and findwalk-edit--compile-buffer
           (buffer-name findwalk-edit--compile-buffer))
      (generate-new-buffer-name "findwalk")))

;;;
;;; compilation variables (before `define-compilation-mode')
;;;

(defvar findwalk-mode-font-lock-keywords
   '(("^Find started at.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ("^Find finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Find \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 compilation-error-face)
      (2 compilation-error-face nil t))
     ;; buffer header file-local-variable
     ("\\`-\\*- mode:.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ;; error messages
     ("^find:\\(.*\\)"
      (1 '(face compilation-error compilation-message nil help-echo nil mouse-face nil)))
     )
   "Additional things to highlight in find output.
This gets tacked on the end of the generated expressions.")

(defun findwalk-process-setup ()
  "Setup compilation variables and buffer for `find'.
Set up `compilation-exit-message-function' and run `findwalk-setup-hook'."
  ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
  (setenv "LANG" "C")
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
  (setq findwalk--filterd-point (point))
  (run-hooks 'findwalk-setup-hook))

(defvar findwalk--filterd-point nil
  "TODO `compilation-filter-start' is prepared after version 24.")

(defun findwalk--compilation-filter ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char findwalk--filterd-point)
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
                   (not (findwalk--get-overlays start fin)))
              (findwalk--create-overlay start fin line))))
          (forward-line 1))
        (setq findwalk--filterd-point (point))))))

(defun findwalk-process-finish (buffer &rest ignore)
  (kill-local-variable 'findwalk--filterd-point)
  (let ((findwalk-list-inhibit-preparation t))
    (findwalk-list-mode 1))
  (set-buffer-modified-p nil)
  (setq buffer-read-only nil))

;;;###autoload
(define-compilation-mode findwalk-mode "Find"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  ;; (setq grep-last-buffer (current-buffer))
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-error-face)
       'findwalk-file-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^\\([.].+\\)" 1) ("^\\(/.+\\)" 1)))
  (set (make-local-variable 'compilation-process-setup-function)
       'findwalk-process-setup)
  (if (< emacs-major-version 23)
      (set (make-local-variable 'compilation-finish-function)
           'findwalk-process-finish)
    (set (make-local-variable 'compilation-finish-functions)
         (list 'findwalk-process-finish)))
  (set (make-local-variable 'buffer-undo-list) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'findwalk--filterd-point) nil)
  (add-hook 'compilation-filter-hook 'findwalk--compilation-filter nil t))



(provide 'findwalk)

;;; findwalk.el ends here
