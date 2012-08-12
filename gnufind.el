;;; gnufind.el --- find file utilities

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: find command result xargs
;; URL: TODO http://github.com/mhayashi1120/Emacs-find-select/raw/master/find-select.el
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

;; You can use `find' command-line option like S Expression.
;; Provides easy way of editing find complex arguments and to display
;; full command-line to small buffer.
;;

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'gnufind)

;; ** In Emacs 22 or earlier **
;; Not tested. But to install find-cmd.el from following url may work.
;; http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/find-cmd.el

;;; Usage:

;; * Following command open editable buffer.
;;
;;    M-x gnufind
;; 
;; * You can edit `find' command-line option by s-expression like following.
;;
;; (or (name "HOGE") (type "d")) (type "f")
;;
;; This expand to 
;;
;; find /wherever/default-directory \( -name HOGE -or -type d \) -type f 
;;
;; Type C-c C-c execute above command and display command output.
;;      With prefix-arguments, call `find-dired'
;; Type C-c C-q quit editing.
;; Type M-n, M-p move history when exists.
;;
;; * TODO in result buffer

;;; History:


;;; TODO:

;; * Can call function.
;; * Describe how to call command.
;; * Can complete symbol. auto-complete.el?
;; * in result buffer C-m open the file.
;; * cleanup buffer.
;; * refactor
;; * describe how to use. (command sequence)
;; * compilation

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'compile)
(require 'find-cmd)

(defvar find-program)
(defvar grep-program)
(defvar xargs-program)

(defun gnufind-list-with-xargs (command &optional xargs-replace)
  (interactive (let ((command 
		      (read-shell-command "Shell command: ")))
		 (list command)))
  (let* ((infile (gnufind-select-create-temp))
         (command (if xargs-replace
                      (format "%s --replace=%s -e %s < %s" 
                              xargs-program xargs-replace command infile)
                    (format "%s -e %s < %s" xargs-program command infile)))
         (proc (gnufind--start command)))
    (process-put proc 'delete-file infile)
    (set-window-buffer (selected-window) buffer)))

(defun gnufind-list--create-local-file ()
  (let ((tmp (make-temp-file "emacs-find-"))
        (coding-system-for-write file-name-coding-system))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'gnufind-filename)
          (write-region (line-beginning-position) (line-beginning-position 2) tmp t 'no-msg))
        (forward-line 1)))
    tmp))

;;TODO make obsolete
(defun gnufind-select-create-temp ()
  (let ((temp (make-temp-file "EmacsFind"))
	(coding-system-for-write file-name-coding-system))
    (write-region (point-min) (point-max) temp)
    temp))

;;TODO
(defun gnufind-list-shell-command ()
  (interactive)
  (error "Not implement yet"))

;;TODO
(defun gnufind-list-call-function ()
  (interactive "aFunction: ")
  (error "Not implement yet"))

(defun gnufind-list-invoke-grep ()
  "Execute `grep' on listed files."
  (interactive)
  (let* ((infile (gnufind-select-create-temp))
         (grep (gnufind-select-read-grep-command "Run grep on files: "))
         (command (format "%s -e %s < %s" xargs-program grep infile))
         (buffer 
          (save-window-excursion
            (grep command))))
    ;;TODO cleanup infile
    (set-window-buffer (selected-window) buffer)))

(defun gnufind-list-limit-by-grep (regexp)
  "Limit the listed files match to REGEXP."
  (interactive "sGrep regexp: ")
  (gnufind-list-with-xargs 
   (format "%s -l -e %s" grep-program regexp)))

(defun gnufind-list-limit-by-ungrep (regexp)
  "Limit the listed files unmatch to REGEXP."
  (interactive "sGrep regexp: ")
  (gnufind-list-with-xargs 
   (format "%s -L -e %s" grep-program regexp)))

;; TODO limit the result
(defun gnufind-list-limit-by-find ()
  (interactive)
  ;;todo open new edit buffer?
  (gnufind-list-with-xargs 
   (format "%s " find-program regexp)))

;;TODO concatenate other program output to find.
;; ex:
;; dpkg -L some-package | xargs --max-args=1 -I \{\} -e find \{\} -type f -maxdepth 0 -print0 | xargs -0 -e grep -nH -e "word"

;; TODO use shell command output as file list.
;; ex:
;; dpkg -L some-package

(defvar gnufind-select-grep-history nil)

(defun gnufind-select-read-grep-command (prompt)
  (let ((merged
         (append
          (mapcar 
           (lambda (x)
             (and (string-match "grep\\b.*" x)
                  (match-string 0 x)))
           grep-find-history)
          grep-history
          gnufind-select-grep-history)))
    (setq gnufind-select-grep-history merged)
  (read-from-minibuffer prompt
                        (car gnufind-select-grep-history) nil nil 
                        '(gnufind-select-grep-history . 1))))

;;TODO
(defun gnufind-select-clear-stack ()
  (mapc
   (lambda (setting)
     )
   gnufind-edit-configuration-stack)
  (setq gnufind-edit-configuration-stack nil))

(defun gnufind-select-pop-settings ()
  (let ((top (car gnufind-edit-configuration-stack)))
    (when top
      (setq gnufind-edit-configuration-stack
            (cdr gnufind-edit-configuration-stack))
      top)))

;;TODO rename
(defun gnufind-select-push-current-settings ()
  (let ((config (current-window-configuration)))
    (setq gnufind-edit-configuration-stack
          (cons config gnufind-edit-configuration-stack))))

(defun gnufind-select-functions (arg-length)
  (let (res)
    (mapatoms
     (lambda (x)
       (when (functionp x)
         (let ((func (symbol-function x)))
           (cond
            ((symbolp func)
             ;;TODO
             (indirect-function func))
            ;;TODO
            ;; ((functionp func)
            ;;  (setq res (cons x res)))
            ((subrp func)
             (let ((arity (subr-arity func)))
               (when (and (<= (car arity) arg-length)
                          (or (eq (cdr arity) 'many)
                              (<= arg-length (cdr arity))))
                 (setq res (cons x res)))))))))
     obarray)
    res))

;;
;; Listing find
;;

(defvar gnufind-mode-map nil)

(unless gnufind-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-f" 'gnufind-list-limit-by-find)
    (define-key map "\C-c\C-l" 'gnufind-list-limit-by-grep)
    (define-key map "\C-c!" 'gnufind-list-with-xargs)
    (define-key map "\C-c\e|" 'gnufind-list-shell-command)
    (define-key map "\C-c\eg" 'gnufind-list-invoke-grep)
    (define-key map "\C-c\el" 'gnufind-list-limit-by-ungrep)

    (setq gnufind-mode-map map)))

;;
;; Editing find args
;;

(defun gnufind-edit-all-methods ()
  (mapcar 
   (lambda (x) (symbol-name (car x)))
   find-constituents))

(defvar gnufind-edit-mode-map nil)

(let ((map (or gnufind-edit-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\C-k" 'gnufind-edit-quit)
  (define-key map "\C-c\C-q" 'gnufind-edit-quit)
  (define-key map "\C-c\C-c" 'gnufind-edit-execute)
  ;;TODO change keybind
  (define-key map "\C-c\C-d" 'gnufind-edit-execute-dired)
  (define-key map "\M-p" 'gnufind-edit-previous-history)
  (define-key map "\M-n" 'gnufind-edit-next-history)

  (setq gnufind-edit-mode-map map))

(defvar gnufind-edit-font-lock-keywords 
  `(
    (,(concat "(" (regexp-opt (gnufind-edit-all-methods) t) "\\b") 
     (1 font-lock-function-name-face))
    ))

(defvar gnufind-edit-font-lock-defaults
  '(
    (gnufind-edit-font-lock-keywords)
    nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
    (font-lock-mark-block-function . mark-defun)
    (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function)
    ))

(defvar gnufind-edit-buffer-name "*Find Edit*")
(defvar gnufind-edit-sub-buffer-name " *Find Edit Command-Line* ")
(defvar gnufind-edit-configuration-stack nil)
(defvar gnufind-edit-history nil)
(defvar gnufind-edit-history-position nil)

(define-derived-mode gnufind-edit-mode lisp-mode "Find Edit"
  "Major mode to build `find' command args by using `fsvn-cmd'"
  (set (make-local-variable 'after-change-functions) nil)
  (set (make-local-variable 'kill-buffer-hook) nil)
  (set (make-local-variable 'gnufind-edit-history-position) nil)
  (set (make-local-variable 'window-configuration-change-hook) nil)
  (set (make-local-variable 'completion-at-point-functions) 
       (list 'gnufind-edit-completion-at-point))
  (set (make-local-variable 'font-lock-defaults)
       gnufind-edit-font-lock-defaults)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (gnufind-select-push-current-settings)
  (gnufind-edit-ac-initialize)
  (add-hook 'after-change-functions 'gnufind-edit--show-command nil t)
  (add-hook 'kill-buffer-hook 'gnufind-select-cleanup nil t)
  (use-local-map gnufind-edit-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

(defun gnufind-edit-previous-history ()
  (interactive)
  (gnufind-edit-goto-history t))

(defun gnufind-edit-next-history ()
  (interactive)
  (gnufind-edit-goto-history nil))

(defun gnufind-edit-execute-dired ()
  "Execute `find-dired' ."
  (interactive)
  ;;TODO use gnufind-edit-args-string ?
  ;;TODO when error?
  (let ((edit-buffer (current-buffer))
	(find-args (gnufind-edit-args-string-safe)))
    (find-dired default-directory find-args)))

;;TODO commit
(defun gnufind-edit-execute ()
  "Execute `find' with editing args."
  (interactive)
  (let* ((edit-buffer (current-buffer))
         (dir default-directory)
         (find-args (gnufind-edit-args))
         (buffer (gnufind--start 
                  (format "%s %s" find-program 
                          (mapconcat 'identity find-args " ")))))
    (gnufind-edit-result-configuration buffer)))

(defun gnufind-edit-result-configuration (result-buffer)
  (add-to-history 'gnufind-edit-history (buffer-string))
  (gnufind-select-delete-subwindow)
  (let ((rwin (get-buffer-window result-buffer))
        (ewin (selected-window))
        (new-height 5))
    (when (> (window-height ewin) new-height)
      (set-window-text-height (selected-window) new-height))))

;;TODO rename
(defun gnufind--start (command)
  (compilation-start command 'gnufind-mode))

;;TODO rename
(defun gnufind-select-new-buffer ()
  (let* ((ids (sort
               (remove nil
                       (mapcar
                        (lambda (x) 
                          (let ((name (buffer-name x)))
                            ;;TODO gnufind-select-result-buffer-regexp is obsoleted
                            (and (string-match gnufind-select-result-buffer-regexp name)
                                 (string-to-number (match-string 1 name)))))
                        (buffer-list)))
               '<))
         (next
          (if ids (1+ (apply 'max ids)) 1)))
    (get-buffer-create (format gnufind-select-result-buffer-format next))))

(defun gnufind-edit-goto-history (previous)
  (let ((n (funcall (if previous '1+ '1-) (or gnufind-edit-history-position -1))))
    (cond
     ((or (null gnufind-edit-history)
	  (< n 0))
      (message "No more history"))
     ((> n (1- (length gnufind-edit-history)))
      (message "No more history"))
     (t
      (erase-buffer)
      (insert (nth n gnufind-edit-history))
      (setq gnufind-edit-history-position n)))))



(defun gnufind-select-cleanup ()
  (gnufind-select-restore))

(defun gnufind-select-restore ()
  (let ((config (gnufind-select-pop-settings)))
    (when (window-configuration-p config)
      (set-window-configuration config))))

(defun gnufind-select-read-function ()
  (let (collection tmp)
    (mapatoms
     (lambda (s)
       (when (fboundp s)
         (when (= (gnufind-select-function-min-arg s) 1)
           (setq collection (cons s collection)))))
     obarray)
    (setq tmp (completing-read "Function (one arg): " collection nil t nil nil))
    (if tmp
	(intern tmp)
      'identity)))

(defun gnufind-select-function-min-arg (symbol)
  (let* ((f (symbol-function symbol))
         (len 0))
    (cond
     ((subrp f)
      (setq len (car (subr-arity f))))
     (t
      (catch 'done
        (let ((args (help-function-arglist f)))
          (when (listp args)
            (mapc
             (lambda (a)
               (when (memq a '(&optional &rest))
                 (throw 'done t))
               (setq len (1+ len)))
             args))))))
    len))

(defvar gnufind-select-xargs-mode nil)
(make-variable-buffer-local 'gnufind-select-xargs-mode)

(defvar gnufind-editing-buffer nil)
(make-variable-buffer-local 'gnufind-editing-buffer)

(defun gnufind-edit--show-command (&rest dummy)
  (condition-case nil
      (let ((buf (get-buffer-create gnufind-edit-sub-buffer-name))
            (first-arg (if gnufind-select-xargs-mode 
                           "`pass by xargs`" 
                         (abbreviate-file-name default-directory)))
            (edit-buffer (current-buffer))
	    args win parse-error)
	(condition-case err
	    (setq args (gnufind-edit-args-string))
	  (error (setq parse-error err)))
	(with-current-buffer buf
          (setq gnufind-editing-buffer edit-buffer)
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (cond
	     (args
	      (insert (propertize (concat find-program " " first-arg " ")
				  'face font-lock-constant-face))
	      (insert (propertize args 'face font-lock-variable-name-face) "\n"))
	     (t
	      (insert (propertize (format "%s" parse-error)
				  'face font-lock-warning-face)))))
	  (setq buffer-read-only t)
	  (set-buffer-modified-p nil)
          (add-hook 'window-configuration-change-hook 'gnufind-select-cleanup-subwindow-maybe))
	(unless (memq buf (mapcar 'window-buffer (window-list)))
	  (setq win (split-window-vertically))
	  (set-window-buffer win buf)
	  (set-window-text-height win 5)))
    ;; ignore all
    (error nil)))

(defun gnufind-edit-args-string ()
  (let ((subfinds (gnufind-edit-read-expressions)))
    (mapconcat 'find-to-string subfinds "")))

(defun gnufind-edit-args-string-safe ()
  (condition-case nil
      (gnufind-edit-args-string)
    (error "")))

(defun gnufind-edit-args ()
  (let* ((subfinds (gnufind-edit-read-expressions))
         (args (mapcar 'find-to-string subfinds)))
    (apply 
     'append
     (mapcar
      (lambda (item)
        (let ((arg (split-string item " " t)))
          (list
           (car arg)
           (mapconcat 'identity (cdr arg) " "))))
      args))))

(defun gnufind-edit-read-expressions ()
  (let (exp subfinds)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
	  (while (setq exp (read (current-buffer)))
	    (setq subfinds (cons exp subfinds)))
	(end-of-file nil)))
    (nreverse subfinds)))

(defun gnufind-select-concat-0 ()
  (let (list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq list (cons 
		    (buffer-substring (line-beginning-position)
				      (line-end-position))
		    list))
	(forward-line 1)))
    (setq list (nreverse list))
    (mapconcat 'identity list "\000")))

(defun gnufind-edit-completion-at-point ()
  (with-syntax-table lisp-mode-syntax-table
    (let* ((pos (point))
           (beg (condition-case nil
                    (save-excursion
                      (backward-sexp 1)
                      (skip-syntax-forward "'")
                      (point))
                  (scan-error pos)))
           (end (point)))
      (list beg end 
            (vconcat (mapcar 'car find-constituents))))))

;;TODO not works?
(defun gnufind-edit-ac-initialize ()
  (dont-compile
    (when (featurep 'auto-complete)

      (ac-define-source gnufind-select-constituents
        '((candidates . gnufind-edit-all-methods)
          (symbol . "s")
          (prefix . "(\\(?:\\(?:\\sw\\|\\s_\\)*\\)")
          (requires . 1)
          (cache)))

      (setq ac-sources '(ac-source-find-select-constituents))
      (set (make-local-variable 'ac-modes)
           `(,major-mode))
      (auto-complete-mode 1))))

(defvar gnufind-select-running-process nil)

(defun gnufind-select-delete-subwindow ()
  (let ((sub (get-buffer gnufind-edit-sub-buffer-name))
        win)
    (when (and sub (setq win (get-buffer-window sub)))
      (delete-window win))))

(defun gnufind-select-cleanup-subwindow-maybe ()
  (let ((sub (get-buffer gnufind-edit-sub-buffer-name)))
    (when sub
      (let ((main (buffer-local-value 'gnufind-editing-buffer sub)))
        (when (and main
                   (buffer-live-p main))
          (let ((win (get-buffer-window main)))
            (unless (and win (window-live-p win))
              (gnufind-select-delete-subwindow)))))))
  (remove-hook 'window-configuration-change-hook 'gnufind-select-cleanup-subwindow-maybe))

(defun gnufind-edit-quit ()
  "Quit editing."
  (interactive)
  (mapc
   (lambda (name)
     (let ((buffer (get-buffer name)))
       (when (buffer-live-p buffer)
	 (bury-buffer buffer))))
   (list gnufind-edit-buffer-name
         gnufind-edit-sub-buffer-name))
  (when (eq major-mode 'gnufind-mode)
    (kill-buffer (current-buffer)))
  (gnufind-select-restore))

;; TODO interface
(defun gnufind-select-narrow ()
  (interactive)
  (let ((buffer (get-buffer-create gnufind-edit-buffer-name))
	(dir default-directory))
    (with-current-buffer buffer
      (setq default-directory dir)
      (gnufind-edit-mode)
      (setq gnufind-select-xargs-mode t))
    ;;TODO set window settings
    ;; remove empty line?
    ;; remove invalid line?
    (select-window (display-buffer buffer))
    (message (substitute-command-keys 
              (concat "Type \\[gnufind-edit-execute] to execute find, "
                      ;;TODO
                      "\\[gnufind-edit-quit] to quit edit.")))))

;;TODO rename
(defun gnufind-select-next (&optional start end)
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((buf (gnufind-select-new-buffer)))
    (append-to-buffer buf start end)
    (switch-to-buffer buf)
    (gnufind-mode)
    ;;TODO message
    ))


;;TODO rename -> find
(defun gnufind-select (&optional suppress-todo)
  ;; execute find and display command-line to buffer.
  ;; -> electric mode?
  ;; execute buffer buffer with call-process-region
  ;;TODO clear stack
  (interactive)
  (let ((buffer (get-buffer-create gnufind-edit-buffer-name))
	(dir default-directory))
    (unless suppress-todo
      (gnufind-select-clear-stack))
    (with-current-buffer buffer
      (setq default-directory dir)
      (gnufind-edit-mode))
    (select-window (display-buffer buffer))
    (message (substitute-command-keys 
              (concat "Type \\[gnufind-edit-execute] to execute find, "
                      "\\[gnufind-edit-quit] to quit edit.")))))

;; TODO save buffer contents to history
;; * some shell command buffer
;; **  *shell command* dpkg -L some-package
;;    to narrow the find result
;;   clear stack
;; * gnufind-mode buffer
;; * output result is nothing message


;;;###autoload
(define-compilation-mode gnufind-mode "GNU Find"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  ;; (setq grep-last-buffer (current-buffer))
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^\\([.].+\\)" 1)))
  (set (make-local-variable 'compilation-process-setup-function)
       'gnufind-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (add-hook 'compilation-filter-hook 'gnufind--compilation-filter nil t)
  ;;TODO
  (gnufind-select-push-current-settings))

(defun gnufind--compilation-filter ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
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
             ((file-exists-p line)
              ;;TODO
              (put-text-property start fin 'gnufind-filename t))))
          (forward-line 1))))))

;;TODO
(defun gnufind-buffer-name-function (dummy)
  "*find a")

(defun gnufind-process-setup ()
  "Setup compilation variables and buffer for `find'.
Set up `compilation-exit-message-function' and run `gnufind-setup-hook'."
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
  ;;TODO defcustom
  (run-hooks 'gnufind-setup-hook))

(defvar gnufind-mode-font-lock-keywords
   '(("^Find started.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ("^Find finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Find \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 grep-error-face)
      (2 grep-error-face nil t))
     ("\\`-\\*- mode:.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ("^find:\\(.*\\)"
      (1 '(face compilation-error compilation-message nil help-echo nil mouse-face nil)))
     )
   "Additional things to highlight in find output.
This gets tacked on the end of the generated expressions.")



(provide 'gnufind)

;;; gnufind.el ends here
