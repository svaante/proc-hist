;;; proc-hist.el --- History management for processes -*- lexical-binding: t -*-

(require 'subr-x)
(require 'vc-git)
(require 'cl-lib)

(defgroup proc-hist nil
  "TODO"
  :group 'extensions)

(defcustom proc-hist-commands
  '(("compilation"
     :commands (recompile
                compile
                project-compile
                proc-hist--compile-rerun)
     :rerun proc-hist--compile-rerun
     :command proc-hist--shell-command-to-string
     :symbol compile)
    ("Shell"
     :commands (async-shell-command
                project-async-shell-command
                dired-do-async-shell-command
                proc-hist--async-shell-command-rerun)
     :rerun proc-hist--async-shell-command-rerun
     :command proc-hist--shell-command-to-string
     :symbol async-shell-command))
  "TODO")

(defcustom proc-hist-logs-folder "/tmp"
  "TODO"
  :group 'proc-hist)

(defcustom proc-hist-save-file (locate-user-emacs-file "proc-hist")
  "TODO"
  :group 'proc-hist)

(defcustom proc-hist-completing-read #'proc-hist-completing-read-fn
  "TODO"
  :group 'proc-hist)

(defcustom proc-hist-on-finished (lambda (item) (proc-hist-dwim item t))
  "TODO"
  :group 'proc-hist)

(defcustom proc-hist-history-create-retain-p #'proc-hist--history-create-retain-p-fn
  "TODO"
  :group 'proc-hist)

(cl-defstruct (proc-hist-item (:type list))
  command
  status
  start-time
  end-time
  log
  directory
  vc
  name)

(defvar proc-hist--items-session (make-hash-table))
(defvar proc-hist--items-inactive nil)
(defvar proc-hist--buffers (make-hash-table))
(defvar proc-hist--hooks (make-hash-table))

(defvar proc-hist--injected-command nil)

(defun proc-hist--items ()
  (append
   (hash-table-values proc-hist--items-session)
   proc-hist--items-inactive))

(defun proc-hist--time-format ()
  (format-time-string "%Y-%m-%d %T"))

(defun proc-hist--log (&optional n)
  (let* ((n (or n 0))
         (filename (concat
                   (format "%s%s"
                           (file-name-as-directory proc-hist-logs-folder)
                           (format-time-string "%Y-%m-%d%T"))
                   (when (> n 0)
                     (format "_%d" n))
                   ".log")))
    (if (file-exists-p filename)
        (proc-hist--log (1+ n))
      filename)))

(defun proc-hist--update-status (proc item)
  (when (or (null proc)
            (memq (process-status proc) '(exit signal)))
    ;; Process exited
    (setf (proc-hist-item-status item)
          (or (and proc (process-exit-status proc))
              -1))
    (setf (proc-hist-item-end-time item)
          (proc-hist--time-format))
    ;; Call on finished
    ;; BUG: When switching to buffer to early compilation-mode signals
    ;; `compilation-parse-errors'
    (run-with-idle-timer 0 nil (apply-partially proc-hist-on-finished item))
    (when-let ((hook (gethash proc proc-hist--hooks)))
      (apply 'apply hook))
    (proc-hist--save)))

(defun proc-hist--add-proc (proc name command directory)
  (let ((item (make-proc-hist-item
               :command command
               :status nil
               :start-time (proc-hist--time-format)
               :end-time nil
               :log (proc-hist--log)
               :directory directory
               :vc (or (seq-take
                        (vc-git-working-revision directory)
                        7)
                       "")
               :name name)))
    (puthash proc item proc-hist--items-session)
    item))

(defun proc-hist--create-sentinel (sentinel)
  (lambda (proc signal)
    (funcall sentinel proc signal)
    (when-let ((item (gethash proc proc-hist--items-session)))
      (puthash (process-buffer proc) item proc-hist--buffers)
      (proc-hist--update-status proc item))))

(defun proc-hist--create-filter (filter)
  (lambda (proc string)
    (funcall filter proc string)
    (when-let ((item (gethash proc proc-hist--items-session)))
      (puthash (process-buffer proc) item proc-hist--buffers)
      (write-region string nil (proc-hist-item-log item) 'append 'no-echo))))

(defun proc-hist--advice-make-process (make-process &rest args)
  (if-let* ((proc-hist-command (proc-hist--proc-hist-command-p args)))
      (let* ((filter (plist-get args :filter))
             (sentinel (plist-get args :sentinel))
             (args (or (and filter (plist-put args :filter (proc-hist--create-filter filter)))
                       args))
             (args (or (and sentinel (plist-put args :sentinel (proc-hist--create-sentinel sentinel)))
                       args))
             (proc (apply make-process args)))
        (proc-hist--add-proc
         proc
         (car proc-hist-command)
         (funcall (plist-get (cdr proc-hist-command) :command)
                  (plist-get args :command))
         default-directory)
        proc)
    (apply make-process args)))

(defun proc-hist--advice-set-process-sentinel (set-process-sentinel
                                               process
                                               sentinel)
  (if-let ((item (gethash process proc-hist--items-session)))
      (funcall set-process-sentinel process (proc-hist--create-sentinel sentinel))
    (funcall set-process-sentinel process sentinel)))

(defun proc-hist--advice-set-process-filter (set-process-filter process filter)
  (if-let ((item (gethash process proc-hist--items-session)))
      (funcall set-process-filter process (proc-hist--create-filter filter))
    (funcall set-process-filter process filter)))

(defun proc-hist--compile-rerun (item)
  (let ((default-directory (proc-hist-item-directory item)))
    (compile (proc-hist-item-command item))))

(defun proc-hist--async-shell-command-rerun (item)
  (let ((default-directory (proc-hist-item-directory item)))
    (async-shell-command (proc-hist-item-command item))))

;;; Util
(defun proc-hist--proc-hist-command-p (args)
  (unless (equal (plist-get args :sentinel) 'tramp-process-sentinel)
    (seq-find
     (lambda (proc-hist-command)
       (and (member this-command (plist-get (cdr proc-hist-command) :commands))))
     proc-hist-commands)))

(defun proc-hist--get-rerun (rerun-command)
  (thread-first rerun-command
                (alist-get proc-hist-commands nil nil 'equal)
                (plist-get :rerun)))

(defun proc-hist--shell-command-to-string (command)
  (mapconcat 'identity
             (if (equal shell-command-switch
                        (nth 1 command))
                 (seq-drop command 2)
               command)
             " "))

(defun proc-hist--get-proc (item)
  (seq-find
   (lambda (proc)
     (equal (gethash proc proc-hist--items-session)
            item))
   (hash-table-keys proc-hist--items-session)))

;;; Hist
(defun proc-hist--prune ()
  (let ((retain-p (funcall proc-hist-history-create-retain-p)))
    (setq proc-hist--items-inactive
         (cl-loop for item in proc-hist--items-inactive
                  if (funcall retain-p item)
                  collect item
                  else
                  do (delete-file (proc-hist-item-log item))))
    (cl-loop for key in (hash-table-keys proc-hist--items-session)
            unless (funcall retain-p (gethash key proc-hist--items-session))
            do (delete-file (proc-hist-item-log (gethash key proc-hist--items-session)))
               (remhash key proc-hist--items-session))))

(defun proc-hist--history-create-retain-p-fn ()
  (let ((unique-lookup (make-hash-table :test 'equal)))
    (seq-do (lambda (item)
              (let ((key (cons (string-trim (proc-hist-item-command item))
                               (proc-hist-item-directory item))))
                (unless (gethash key unique-lookup)
                  (puthash key item unique-lookup))))
            (proc-hist--items))
    (lambda (item)
      (or
       ;; Keep if {dir command} unique
       (eql item (gethash (cons (string-trim (proc-hist-item-command item))
                                (proc-hist-item-directory item))
                          unique-lookup))
       ;; Keep if entry is younger then two weeks
       (or (not (proc-hist-item-end-time item))
           (< (thread-last (proc-hist-item-end-time item)
                           (parse-time-string)
                           (encode-time)
                           (float-time)
                           (- (float-time (current-time))))
              1209600))))))

(defun proc-hist--save ()
  (proc-hist--prune)
  (with-temp-buffer
    (insert
     (concat
      ";; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
      ";; proc-hist history filed, automatically generated by `proc-hist'.\n"
      "\n"))
    (let ((print-length nil)
	  (print-level nil)
	  (print-quoted t))
      (prin1 `(setq proc-hist--items-inactive
                    ;; Drop `filter' and `sentinel' from item
		    ',(mapcar (lambda (item)
                                (seq-take item 8))
                              (proc-hist--items)))
	     (current-buffer)))
    ;; Write to `proc-hist-save-file'
    (let ((file-precious-flag t)
	  (coding-system-for-write 'utf-8-unix)
          (dir (file-name-directory proc-hist-save-file)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (write-region (point-min) (point-max) proc-hist-save-file nil
		    (unless (called-interactively-p 'interactive) 'quiet)))))

;;; Completion
(defconst proc-hist--max-cand-width 100)

(defun proc-hist--truncate (string length face &optional skip-fill)
  (let ((base-string (propertize
                      (truncate-string-to-width string length 0 nil "...")
                      'face face)))
    (if skip-fill
        base-string
      (truncate-string-to-width base-string
                                length
                                0
                                ?\s))))

(defun proc-hist-annotate (table)
  (lambda (candidate)
    (when-let* ((item (gethash candidate table))
                (space-width (- proc-hist--max-cand-width
                                (string-width candidate))))
      (concat
       (propertize " " 'display
                   `(space :align-to (+ left proc-hist--max-cand-width)))
       (proc-hist--truncate
        ;; `abbreviate-file-name' calls tramp if non local path
        (if (tramp-tramp-file-p (proc-hist-item-directory item))
            (proc-hist-item-directory item)
          (abbreviate-file-name
           (proc-hist-item-directory item)))
        40
        'dired-directory)
       "  "
       (proc-hist--truncate
        (format-seconds
         "0s%x%yy %dd %hh %mm %ss%z"
         (- (time-to-seconds (and (proc-hist-item-end-time item)
                                  (date-to-time (proc-hist-item-end-time item))))
            (time-to-seconds (date-to-time (proc-hist-item-start-time item)))))
        10
        (cond
         ((not (proc-hist-item-end-time item)) 'default)
         ((zerop (proc-hist-item-status item)) 'success)
         (t 'error)))
       "  "
       (proc-hist--truncate
        (proc-hist-item-start-time item)
        20
        'bui-time)
       "  "
       (proc-hist--truncate
        (format "%s"
                (proc-hist-item-name item))
        20
        'italic)
       "  "
       (propertize
        (proc-hist-item-vc item)
        'face 'magit-hash)))))

(defun proc-hist--candidates (&optional filter)
  (let ((table (make-hash-table :test 'equal))
        (items (seq-filter (or filter 'identity)
                           (proc-hist--items))))
    (seq-do
     (lambda (item)
       (let* ((dup-format " <%d>")
              (base-key (proc-hist--truncate (proc-hist-item-command item)
                                             proc-hist--max-cand-width
                                             nil
                                             t))
              (key base-key)
              (n 1))
         (while (gethash key table)
           (let ((suffix (propertize (format " <%d>" n)
                                     'face 'shadow)))
             (setq key (concat (proc-hist--truncate (proc-hist-item-command item)
                                                    (- proc-hist--max-cand-width
                                                       (string-width suffix))
                                                    nil
                                                    t)
                               suffix)
                   n (1+ n))))
         (puthash key item table)))
      (reverse items))
    table))

(defun proc-hist-completing-read-fn (prompt &optional filter)
  (let* ((table (proc-hist--candidates filter))
         (collection
          (lambda (string predicate action)
            (if (eq action 'metadata)
                `(metadata
                  (category . proc-hist)
                  (annotation-function . ,(proc-hist-annotate table)))
              (complete-with-action action
                                    table
                                    string
                                    predicate)))))
    (gethash (completing-read prompt collection nil t) table)))

(defun proc-hist-completing-read-command ()
  (let ((collection
         (lambda (string predicate action)
           (if (eq action 'metadata)
               `(metadata
                 (category . commands))
             (complete-with-action action
                                   (mapcar 'car proc-hist-commands)
                                   string
                                   predicate)))))
    (completing-read "Command: " collection nil t)))

;;; Commands
(defun proc-hist-dwim (item &optional noselect)
  (interactive
   (list
    (funcall proc-hist-completing-read "Open: ")))
  (let ((buffer (seq-find
                 (lambda (buffer)
                   (equal item (gethash buffer proc-hist--buffers)))
                 (hash-table-keys proc-hist--buffers))))
    (if (and buffer (buffer-live-p buffer))
        (if noselect
            (unless (get-buffer-window buffer 0)
              (pop-to-buffer buffer nil t))
          (unless (equal (current-buffer) buffer)
            (switch-to-buffer-other-window buffer)))
      (proc-hist-open-log item))))

(defun proc-hist-open-log (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Open log: ")))
  (when (file-exists-p (proc-hist-item-log item))
    (let ((buffer (thread-first
                    (format "*%s %s*"
                            (proc-hist-item-command item)
                            (proc-hist-item-start-time item))
                    (generate-new-buffer))))
      (with-current-buffer buffer
        (setq-local default-directory (proc-hist-item-directory item))
        (insert-file-contents (proc-hist-item-log item) nil)
        (read-only-mode)
        (compilation-minor-mode))
      (switch-to-buffer-other-window buffer))))

(defun proc-hist-rerun (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Rerun: ")))
  (let* ((rerun (proc-hist--get-rerun (proc-hist-item-name item)))
         (this-command rerun))
    (funcall rerun item)))

(defun proc-hist-rerun-as (item rerun-command)
  (interactive
   (list
    (funcall proc-hist-completing-read "Rerun: ")
    (funcall #'proc-hist-completing-read-command)))
  (let* ((rerun (proc-hist--get-rerun (proc-hist-item-name item)))
         (this-command rerun))
    (funcall-interactively rerun item)))

(defun proc-hist-kill (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Kill: ")))
  (if-let ((proc (proc-hist--get-proc item)))
      (signal-process proc 'kill)
    (proc-hist--update-status nil item)
    (error "Process already killed")))

(defun proc-hist-run-after (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Run after: ")))
   (let* ((symbol (thread-first
                    (proc-hist-item-name (car (proc-hist--items )))
                    (alist-get proc-hist-commands 'equal)
                    (plist-get :symbol)))
          (args (apply (cadr (interactive-form symbol)))))
     (puthash (proc-hist--get-proc item) (list symbol args) proc-hist--hooks)
     (message "%s %S process finished" symbol args)))

(defun proc-hist-copy-as-kill-command (item)
  (interactive
   (list (funcall proc-hist-completing-read "Copy command: ")))
  (kill-new (proc-hist-item-command item)))

(defun proc-hist-dired (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Open folder: ")))
  (let ((directory (proc-hist-item-directory item)))
    (dired directory)))

;;; Setup
(defun proc-hist--advice-add ()
  (advice-add 'set-process-filter
              :around
              #'proc-hist--advice-set-process-filter)
  (advice-add 'set-process-sentinel
              :around
              #'proc-hist--advice-set-process-sentinel)
  (advice-add 'make-process
              :around
              #'proc-hist--advice-make-process))

(defun proc-hist--advice-remove ()
  (advice-remove 'set-process-filter
                 #'proc-hist--advice-set-process-filter)
  (advice-remove 'set-process-sentinel
                 #'proc-hist--advice-set-process-sentinel)
  (advice-remove 'make-process
                 #'proc-hist--advice-make-process))
;;;###autoload
(define-minor-mode proc-hist-mode
  "TODO"
  :global t :group 'proc-hist
  (if proc-hist-mode
      (progn
        (when (and (file-exists-p proc-hist-save-file)
                   (seq-empty-p (proc-hist--items)))
          (load proc-hist-save-file t)
          (dolist (item proc-hist--items-inactive)
            ;; FIXME Should really handle these processes in a nicer way
            ;; If emacs is killed it brings all sub-processes down with it
            ;; they don't receive a sentinel call
            (unless (proc-hist-item-status item)
              (setf (proc-hist-item-status item)
                    -1)
              (setf (proc-hist-item-end-time item)
                    (proc-hist--time-format)))))
        (when (memq 'consult features)
          (require 'proc-hist-consult)
          (setq proc-hist-completing-read 'proc-hist-consult-completing-read))
        ;; add hooks
        (proc-hist--advice-add))
    ;; remove hooks
    (proc-hist--advice-remove)))

(provide 'proc-hist)
