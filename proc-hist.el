;;; proc-hist.el --- History managment for processes -*- lexical-binding: t -*-

(require 'subr-x)
(require 'vc-git)
(require 'cl-lib)

(defgroup proc-hist nil
  "TODO"
  :group 'extensions)

(defcustom proc-hist-commands
  '(("compilation"
     :rerun proc-hist--compile-rerun
     :command proc-hist--shell-command-to-string)
    ("Shell"
     :rerun proc-hist--async-shell-command-rerun
     :command proc-hist--shell-command-to-string))
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

(cl-defstruct (proc-hist-item (:type list))
  command
  status
  start-time
  end-time
  log
  directory
  vc
  name
  proc
  proc-sentinel
  proc-filter)

(defvar proc-hist--items-active (make-hash-table))
(defvar proc-hist--items-inactive nil)
(defvar proc-hist--buffers (make-hash-table))

(defun proc-hist--items ()
  (append
   (hash-table-values proc-hist--items-active)
   proc-hist--items-inactive))

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

(defun proc-hist--update-status (proc item)
  (when (or (null proc)
            (memq (process-status proc) '(exit signal)))
    ;; Process exited
    (setf (proc-hist-item-status item)
          (or (and proc (process-exit-status proc))
              -1))
    (setf (proc-hist-item-end-time item)
          (proc-hist--time-format))
    (setf (proc-hist-item-proc item)
          nil)
    ;; Call on finished
    ;; BUG: When switching to buffer to early compilation-mode signals
    ;; `compilation-parse-errors'
    (run-with-idle-timer 0 nil (apply-partially proc-hist-on-finished item))
    (proc-hist--save)))

(defun proc-hist--add-proc (proc name command directory filter sentinel)
  (let ((item (make-proc-hist-item
               :command command
               :status nil
               :start-time (proc-hist--time-format)
               :end-time nil
               :log (proc-hist--log proc)
               :directory directory
               :vc (or (seq-take
                        (vc-git-working-revision directory)
                        7)
                       "")
               :name name
               :proc proc
               :proc-filter filter
               :proc-sentinel sentinel)))
    (puthash proc item proc-hist--items-active)
    item))

(defun proc-hist--sentinel (proc signal)
  (when-let ((item (gethash proc proc-hist--items-active)))
    (when (proc-hist-item-proc-sentinel item)
      (funcall (proc-hist-item-proc-sentinel item) proc signal))
    (puthash (process-buffer proc) item proc-hist--buffers)
    (proc-hist--update-status proc item)))

(defun proc-hist--filter (proc string)
  (when-let ((item (gethash proc proc-hist--items-active)))
    (when (proc-hist-item-proc-filter item)
      (funcall (proc-hist-item-proc-filter item) proc string))
    (puthash (process-buffer proc) item proc-hist--buffers)
    (write-region string nil (proc-hist-item-log item) 'append 'no-echo)))

(defvar proc-hist--tramp-command nil)
(defvar proc-hist--tramp-default-directory nil)

(defun proc-hist--advice-tramp-handle-make-process (tramp-handle-make-process &rest args)
  (let ((proc-hist--tramp-command (plist-get args :command))
        (proc-hist--tramp-default-directory default-directory))
    (apply tramp-handle-make-process args)))

(defun proc-hist--advice-make-process (make-process &rest args)
  (if-let* ((proc-hist-command (proc-hist--proc-hist-command args)))
      (let* ((filter (plist-get args :filter))
             (sentinel (plist-get args :sentinel))
             (args (thread-first args
                                 (plist-put :filter nil)
                                 (plist-put :sentinel nil)))
             (proc (apply make-process args))
             (item (proc-hist--add-proc
                    proc
                    (car proc-hist-command)
                    (funcall (plist-get (cdr proc-hist-command) :command)
                             (or proc-hist--tramp-command
                                 (plist-get args :command)))
                    (or proc-hist--tramp-default-directory
                        default-directory)
                    filter
                    sentinel)))
        (set-process-sentinel proc sentinel)
        (set-process-filter proc filter)
        proc)
   (apply make-process args)))


(defun proc-hist--advice-set-process-sentinel (set-process-sentinel process sentinel)
  (if-let* ((item (gethash process proc-hist--items-active)))
      ;; Set proc hist item sentinel
      (when (setf (proc-hist-item-proc-sentinel item) sentinel)
        (funcall set-process-sentinel process #'proc-hist--sentinel))
    (funcall set-process-sentinel process sentinel)))

(defun proc-hist--advice-set-process-filter (set-process-filter process filter)
  (if-let ((item (gethash process proc-hist--items-active)))
      (when (setf (proc-hist-item-proc-filter item) filter)
        (funcall set-process-filter process #'proc-hist--filter))
    (funcall set-process-filter process filter)))

(defun proc-hist--compile-rerun (item)
  (let ((default-directory (proc-hist-item-directory item)))
    (compile (proc-hist-item-command item))))

(defun proc-hist--async-shell-command-rerun (item)
  (let ((default-directory (proc-hist-item-directory item)))
    (async-shell-command (proc-hist-item-command item))))

;;; Util
(defun proc-hist--proc-hist-command (args)
  (seq-find
   (lambda (proc-hist-command)
     (let ((id (car proc-hist-command)))
       (cond
        ((stringp id)
         (string-match-p id
                         (plist-get args :name)))
        ((symbolp id)
         (with-current-buffer (or (plist-get args :buffer)
                                  (current-buffer))
           (and (boundp id) id)))
        (t nil))))
   proc-hist-commands))

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
      (prin1 `(setq proc-hist--items-inactive
		    ',(proc-hist--items))
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

(defun proc-hist--truncate (string length face)
  (truncate-string-to-width
   (propertize
    (truncate-string-to-width string length 0 nil "...")
    'face face)
   length
   0
   ?\s))

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
         "%x%yy %dd %hh %mm %ss%z"
         (- (if (not (proc-hist-item-end-time item))
                (time-to-seconds)
              (thread-first (proc-hist-item-end-time item)
                            (parse-time-string)
                            (encode-time)
                            (float-time)))
            (thread-first (proc-hist-item-start-time item)
                          (parse-time-string)
                          (encode-time)
                          (float-time))))
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
              (base-key (proc-hist-item-command item))
              (key base-key)
              (n 1))
         (while (gethash key table)
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
    (let ((buffer (find-file-noselect
                   (proc-hist-item-log item))))
      (with-current-buffer buffer
        (rename-buffer
         (format "*%s %s*"
                 (proc-hist-item-command item)
                 (proc-hist-item-start-time item))
         t)
        (read-only-mode)
        (compilation-minor-mode)
        (setq-local default-directory (proc-hist-item-directory item)))
      (switch-to-buffer-other-window buffer))))

(defun proc-hist-rerun (item)
  (interactive
   (list
    (funcall proc-hist-completing-read "Rerun: ")))
  (let ((rerun (proc-hist--get-rerun (proc-hist-item-name item))))
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
  (if-let ((proc (proc-hist-item-proc item)))
      (signal-process proc 'kill)
    (proc-hist--update-status nil item)
    (error "Process all ready killed")))

;;; Setup
(defun proc-hist--advice-commands ()
  (advice-add 'set-process-filter
              :around
              #'proc-hist--advice-set-process-filter)
  (advice-add 'set-process-sentinel
              :around
              #'proc-hist--advice-set-process-sentinel)
  (advice-add 'tramp-sh-handle-make-process
              :around
              #'proc-hist--advice-tramp-handle-make-process)
  (advice-add 'make-process
              :around
              #'proc-hist--advice-make-process))

(defun proc-hist--advice-remove ()
  (advice-remove 'set-process-filter
                 #'proc-hist--advice-set-process-filter)
  (advice-remove 'set-process-sentinel
                 #'proc-hist--advice-set-process-sentinel)
  (advice-remove 'tramp-sh-handle-make-process
                 #'proc-hist--advice-tramp-handle-make-process)
  (advice-remove 'make-process
                 #'proc-hist--advice-make-process))
;;;###autoload
(define-minor-mode proc-hist-mode
  "TODO"
  :global t :group 'proc-hist
  (if proc-hist-mode
      (progn
        (when (file-exists-p proc-hist-save-file)
          (load proc-hist-save-file t))
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
