;;; proc-hist.el --- History managment for processes -*- lexical-binding: t -*-

(require 'subr-x)
(require 'vc-git)
(require 'cl-lib)

(defgroup proc-hist nil
  "TODO"
  :group 'extensions)

(defcustom proc-hist-commands
  '((compile
     :advice proc-hist--compile-advice
     :rerun proc-hist--compile-rerun))
  "TODO")

(defcustom proc-hist-logs-folder "/tmp"
  "TODO"
  :group 'proc-hist)

(defcustom proc-hist-open-dead-as 'fake
  "TODO"
  :type '(choice (const :tag "TODO" 'log)
                 (const :tag "TODO" 'fake))
  :group 'proc-hist)

(defcustom proc-hist-save-file (locate-user-emacs-file "proc-hist")
  "TODO"
  :group 'proc-hist)

(defcustom proc-hist-completing-read #'proc-hist-completing-read-fn
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
  this-command
  proc
  proc-sentinel
  proc-filter)

(defvar proc-hist--items (make-hash-table))
(defvar proc-hist--buffers (make-hash-table))

(defun proc-hist--time-format ()
  (format-time-string "%Y-%m-%d %T"))

(defun proc-hist--log (proc &optional n)
  (let* ((n (or n 0))
         (filename (concat
                   (format "%s%s_%s.log"
                           (file-name-as-directory proc-hist-logs-folder)
                           (process-id proc)
                           (format-time-string "%Y-%m-%d%T"))
                   (when (> n 0)
                       (format "_%d" n)))))
    (if (file-exists-p filename)
        (proc-hist--log proc (1+ n))
      filename)))

(defun proc-hist--update-status (item)
  (when (and item
             (proc-hist-item-proc item)
             (memq (process-status (proc-hist-item-proc item)) '(exit signal)))
    (setf (proc-hist-item-status item)
          (process-exit-status (proc-hist-item-proc item)))
    (setf (proc-hist-item-end-time item)
          (proc-hist--time-format))
    (setf (proc-hist-item-proc item)
          nil))
  (proc-hist--save))

(defun proc-hist--process-command (proc)
  (when-let ((proc)
             (command-list (process-command proc)))
     (and command-list
          (mapconcat 'identity command-list " "))))

(defun proc-hist--add-proc (proc spawn-command command filter sentinel)
  (let ((item (make-proc-hist-item
               :command command
               :status nil
               :start-time (proc-hist--time-format)
               :end-time nil
               :log (proc-hist--log proc)
               :directory default-directory
               :vc (if (file-remote-p default-directory)
                       ""
                     ;; BUG: for some reason this hangs on remote files
                     (or (seq-take
                          (vc-git-working-revision default-directory)
                          7)
                         ""))
               :this-command spawn-command
               :proc proc
               :proc-filter filter
               :proc-sentinel sentinel)))
    (puthash proc item proc-hist--items)
    item))

(defun proc-hist--add-last-buffer (item)
  (when-let* ((proc (proc-hist-item-proc item))
              (buffer (process-buffer proc)))
    (puthash buffer item proc-hist--buffers)))

(defun proc-hist-sentinel (proc signal)
  (let ((item (gethash proc proc-hist--items)))
    (proc-hist--update-status item)
    (proc-hist--add-last-buffer item)
    (when (proc-hist-item-proc-sentinel item)
      (funcall (proc-hist-item-proc-sentinel item) proc signal))))

(defun proc-hist-filter (proc string)
  (let ((item (gethash proc proc-hist--items)))
    (proc-hist--add-last-buffer item)
    (write-region string nil (proc-hist-item-log item) 'append 'no-echo)
    (when (proc-hist-item-proc-filter item)
      (funcall (proc-hist-item-proc-filter item) proc string))))

(defvar proc-hist--spawn-command nil)
(defvar proc-hist--command nil)
(defvar proc-hist--fake-item nil)

(defun proc-hist--advice-make-process (make-process &rest args)
  (cond
   ;; Ignore tramp processes
   ((equal (plist-get args :sentinel) 'tramp-process-sentinel)
    (apply make-process args))
   ;; Creation process of alive process is meaningless
   ((and proc-hist--fake-item
         (null (proc-hist-item-status proc-hist--fake-item)))
    (user-error
     "`proc-hist--fake-item' should not be set when process is alive"))
   ;; Fake resurrection of dead process
   ((and proc-hist--fake-item
         (proc-hist-item-status proc-hist--fake-item))
    (let ((proc (apply make-process
                       (plist-put
                        args
                        :command
                        (proc-hist--cat-exit-command proc-hist--fake-item)))))
      (puthash (process-buffer proc)
               proc-hist--fake-item
               proc-hist--buffers)
      proc))
   ;; Spawn command and add to history
   ((and proc-hist--spawn-command
         proc-hist--command)
     (let* ((filter (plist-get args :sentinel))
            (sentinel (plist-get args :sentinel))
            (args (plist-put args :filter 'proc-hist-filter))
            (args (plist-put args :sentinel 'proc-hist-sentinel))
            (proc (apply make-process args))
            (item (proc-hist--add-proc proc
                                       proc-hist--spawn-command
                                       proc-hist--command
                                       filter
                                       sentinel)))
       (puthash (process-buffer proc)
                proc-hist--fake-item
                proc-hist--buffers)
       proc))
   ;; Create process
   (t (apply make-process args))))

(defun proc-hist--advice-set-process-sentinel (set-process-sentinel process sentinel)
  (if-let ((_ proc-hist--spawn-command)
           (item (gethash process proc-hist--items)))
      ;; Set proc hist item sentinel
      (setf (proc-hist-item-proc-sentinel item) sentinel)
    (funcall set-process-sentinel process sentinel)))

(defun proc-hist--advice-set-process-filter (set-process-filter process filter)
  (if-let ((_ proc-hist--spawn-command)
           (item (gethash process proc-hist--items)))
      ;; Set proc hist item filter
      (setf (proc-hist-item-proc-filter item) filter)
    (funcall set-process-filter process filter)))

(defun proc-hist--compile-advice (compile command &optional comint)
  (let ((proc-hist--spawn-command 'compile)
        (proc-hist--command command))
    (funcall compile command comint)))

(defun proc-hist--compile-rerun (item)
  (let ((default-directory (proc-hist-item-directory item)))
    (compile (proc-hist-item-command item))))

;;; Util
(defun proc-hist--get-rerun (rerun-command)
  (thread-first rerun-command
                (alist-get proc-hist-commands)
                (plist-get :rerun)))

(defun proc-hist--cat-exit-command (item)
  (list shell-file-name
        shell-command-switch
        (mapconcat
         'identity
         (append
          (when (file-exists-p (proc-hist-item-log item))
            (list "cat" (proc-hist-item-log item) "&&"))
          (list
           "exit"
           (int-to-string
            (proc-hist-item-status proc-hist--fake-item))))
         " ")))
;;; Hist
(defun proc-hist--save ()
  (with-temp-buffer
    (insert
     (concat
      ";; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
      ";; proc-hist history filed, automatically generated by `proc-hist'.\n"
      "\n"))
    (let ((print-length nil)
	  (print-level nil)
	  (print-quoted t))
      (prin1 `(setq proc-hist--items
		    ',proc-hist--items)
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
(defconst proc-hist--max-cand-width 60)

(defun proc-hist--truncate (string length)
  (truncate-string-to-width string length 0 ?\s "..."))

(defun proc-hist-annotate (table)
  (lambda (candidate)
    (when-let* ((item (gethash candidate table))
                (space-width (- proc-hist--max-cand-width
                                (string-width candidate))))
      (concat
       (propertize " " 'display
                   `(space :align-to (+ left proc-hist--max-cand-width)))
       (propertize
        (proc-hist--truncate
         (abbreviate-file-name
          (proc-hist-item-directory item))
         40)
        'face 'dired-directory)
       "  "
       (propertize
        (proc-hist--truncate
         (format-seconds
          "%yy %dd %hh %mm %ss%z"
          (- (if (not (proc-hist-item-end-time item))
                 (time-to-seconds)
               (thread-first (proc-hist-item-end-time item)
                             (parse-time-string)
                             (encode-time)
                             (float-time)))
             (thread-first (proc-hist-item-start-time item)
                           (parse-time-string)
                           (encode-time)
                           (float-time))
             -0.1))
         10)
        'face
        (cond
         ((not (proc-hist-item-end-time item)) 'default)
         ((zerop (proc-hist-item-status item)) 'success)
         (t 'error)))
       "  "
       (propertize
        (proc-hist--truncate
         (proc-hist-item-start-time item)
         20)
        'face 'bui-time)
       "  "
       (propertize
        (proc-hist--truncate
         (if-let* ((command (proc-hist-item-this-command item))
                   (command-string (symbol-name command)))
             command-string
           "")
         20)
        'face 'italic)
        "  "
       (propertize
        (proc-hist-item-vc item)
        'face 'magit-hash)))))

(defun proc-hist--candidates (&optional filter)
  (let ((table (make-hash-table :test 'equal))
        (items (seq-filter (or filter 'identity)
                           (hash-table-values proc-hist--items))))
    (seq-do
     (lambda (item)
       (let* ((dup-format " <%d>")
              (base-key (proc-hist-item-command item))
              (key (truncate-string-to-width
                    base-key
                    (- proc-hist--max-cand-width
                       (string-width dup-format))
                    0 nil "..."))
              (n 1))
         (while (gethash key table)
           ;; TODO: style <%d> part
           (setq key (concat base-key
                             (propertize (format " <%d>" n)
                                         'face 'shadow))
                 n (1+ n)))
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
                                   proc-hist-commands
                                   string
                                   predicate)))))
    (intern (completing-read "Command: " collection nil t))))

;;; Commands
(defun proc-hist-dwim (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Open: ")))
  (let ((buffer (seq-find
                 (lambda (buffer)
                   (equal item (gethash buffer proc-hist--buffers)))
                 (hash-table-keys proc-hist--buffers))))
    (if (and buffer (buffer-live-p buffer))
        (switch-to-buffer-other-window buffer)
      (pcase proc-hist-open-dead-as
        ('log (find-file (proc-hist-item-log item)))
        ('fake (proc-hist-open-in item))
        (_ (user-error
            "Expect `proc-hist-open-dead-as' to be either `log' or `fake'"))))))

(defun proc-hist-rerun (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Rerun: ")))
  (let ((rerun (proc-hist--get-rerun (proc-hist-item-this-command item))))
    (funcall rerun item)))

(defun proc-hist-rerun-as (item rerun-command)
  (interactive
   (list
    (funcall proc-hist-completing-read "Rerun: ")
    (funcall #'proc-hist-completing-read-command)))
  (let ((rerun (proc-hist--get-rerun rerun-command)))
    (funcall rerun item)))

(defun proc-hist-kill (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Kill: ")))
  (when-let ((proc (proc-hist-item-proc item)))
    (signal-process proc 'kill)))

(defun proc-hist-open-in (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Attach: ")))
  (let ((proc-hist--fake-item item)
        (rerun (proc-hist--get-rerun (proc-hist-item-this-command item))))
    (funcall rerun item)))

;;; Setup
(defvar proc-hist--adviced nil)

(defun proc-hist--advice-commands ()
  (advice-add 'set-process-filter
              :around
              #'proc-hist--advice-set-process-filter)
  (advice-add 'set-process-sentinel
              :around
              #'proc-hist--advice-set-process-sentinel)
  (advice-add 'make-process
              :around
              #'proc-hist--advice-make-process)
  (seq-do
   (lambda (command-def)
     (let ((command (car command-def))
           (plist (cdr command-def)))
       (unless (plist-get plist :advice)
         (user-error "TODO"))
       (advice-add command
                   :around
                   (plist-get plist :advice)
                   '((name . proc-hist-advice)))
       ;; Store advice commands for removal on -1
       (push command proc-hist--adviced)))
   proc-hist-commands))

(defun proc-hist--advice-remove ()
  (advice-remove 'set-process-filter
                 #'proc-hist--advice-set-process-filter)
  (advice-remove 'set-process-sentinel
                 #'proc-hist--advice-set-process-sentinel)
  (advice-remove 'make-process
                 #'proc-hist--advice-make-process)
  (seq-do
   (lambda (command)
     (advice-remove command 'proc-hist-advice))
   proc-hist--adviced)
  (setq proc-hist--adviced nil))

;;;###autoload
(define-minor-mode proc-hist-mode
  "TODO"
  :global t :group 'proc-hist
  (if proc-hist-mode
      (progn
        (load proc-hist-save-file
              nil
              (not (called-interactively-p 'interactive)))
        (when (memq 'consult features)
          (require 'proc-hist-consult)
          (setq proc-hist-completing-read 'proc-hist-consult-completing-read))
        (when (memq 'embark features)
          (require 'proc-hist-embark))
        ;; add hooks
        (proc-hist--advice-commands))
    ;; remove hooks
    (proc-hist--advice-remove)))

(provide 'proc-hist)
