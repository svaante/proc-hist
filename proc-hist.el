;;; proc-hist.el --- History managment for processes -*- lexical-binding: t -*-

(require 'vc)

(defgroup proc-hist nil
  "TODO"
  :group 'extensions)

(defcustom proc-hist-commands
  '((compile
     :args-to-command car
     :item-to-args proc-hist--command-to-list)
    (async-shell-command
     :args-to-command car
     :item-to-args proc-hist--command-to-list))
  "TODO")

(defcustom proc-hist-logs-folder "/tmp"
  "TODO")

(cl-defstruct (proc-hist-item (:type list))
  proc
  last-buffer
  command
  status
  start-time
  end-time
  log
  directory
  vc
  this-command)

(defvar proc-hist--items nil)
(defvar proc-hist--this-command nil)
(defvar proc-hist--command nil)

(defun proc-hist--time-format ()
  (format-time-string "%Y-%m-%d %T"))

(defun proc-hist--log (proc &optional n)
  (let* ((n (or n 0))
         (filename (concat
                   (format "%s%s_%s.log"
                           (file-name-as-directory proc-hist-logs-folder)
                           (process-id proc)
                           (proc-hist--time-format))
                   (when (> n 0)
                       (format "_%d" n)))))
    (if (file-exists-p filename)
        (proc-hist--log proc (1+ n))
      filename)))

(defun proc-hist-advicep (proc)
   t)

(defun proc-hist--find (proc)
  (seq-find
   (lambda (item)
     (equal proc (proc-hist-item-proc item)))
   proc-hist--items))

(defun proc-hist--add-proc (proc)
  (if-let ((item (proc-hist--find proc)))
      item
    (let ((item (make-proc-hist-item
                 :proc proc
                 :last-buffer nil
                 :command proc-hist--command
                 :status nil
                 :start-time (proc-hist--time-format)
                 :end-time nil
                 :log (proc-hist--log proc)
                 :directory default-directory
                 :vc (if (file-remote-p default-directory)
                         ""
                       ;; BUG: for some reason this hangs on remote files
                       (or (vc-working-revision default-directory 'Git)
                           ""))
                 :this-command proc-hist--this-command)))
      (push item proc-hist--items)
      item)))

(defun proc-hist--add-last-buffer (item)
  (when-let* ((proc (proc-hist-item-proc item))
              (last-buffer (process-buffer proc)))
    ;; Remove `:last-buffer' for each other item
    ;; TODO: Should split active buffers in memory and only check those
    (seq-do
     (lambda (item)
       (when (eq (proc-hist-item-last-buffer item)
                 last-buffer)
         (setf (proc-hist-item-last-buffer item)
               nil)))
     proc-hist--items)
    ;; Set `:last-buffer' to item
    (setf (proc-hist-item-last-buffer item)
          last-buffer)))

(defun proc-hist--create-sentinel (proc fn)
  (let ((item (proc-hist--add-proc proc)))
    (lambda (proc signal)
      (when (and (memq (process-status proc) '(exit signal))
                 item)
        (setf (proc-hist-item-status item)
              (process-exit-status proc))
        (setf (proc-hist-item-end-time item)
              (proc-hist--time-format))
        (setf (proc-hist-item-proc item)
              nil))
      (proc-hist--add-last-buffer item)
      (funcall fn proc signal))))

(defun proc-hist--create-filter (proc fn)
  (let ((item (proc-hist--add-proc proc)))
    (lambda (proc string)
      (proc-hist--add-last-buffer item)
      (write-region string nil (proc-hist-item-log item) 'append 'no-echo)
      (funcall fn proc string))))
      (write-region string nil (proc-hist-item-log item) 'append 'no-echo)

;; Util
(defun proc-hist--command-to-list (item)
  (list (proc-hist-item-command item)))

(defun proc-hist--advice-name (command)
  (make-symbol
   (format "%s-proc-hist-advice" command)))

;; Advice
(defun proc-hist--set-process-filter (oldfn proc fn)
  (funcall oldfn
           proc
           (if (proc-hist-advicep proc)
               (proc-hist--create-filter proc fn)
             fn)))

(defun proc-hist--set-process-sentinel (oldfn proc fn)
  (funcall oldfn
           proc
           (if (proc-hist-advicep proc)
               (proc-hist--create-sentinel proc fn)
             fn)))

(defun proc-hist--create-advice (command args-to-command)
  (lambda (oldfn &rest args)
    ;; Set vars
    (setq proc-hist--this-command command)
    (setq proc-hist--command (funcall args-to-command args))
    ;; Set advice
    (advice-add 'set-process-sentinel :around #'proc-hist--set-process-sentinel)
    (advice-add 'set-process-filter :around #'proc-hist--set-process-filter)
    ;; Protect advice removal
    (unwind-protect
        (apply oldfn args)
      (advice-remove 'set-process-sentinel #'proc-hist--set-process-sentinel)
      (advice-remove 'set-process-filter #'proc-hist--set-process-filter))))

(defvar proc-hist--adviced nil)

(defun proc-hist--advice-commands ()
  (seq-do
   (lambda (command-def)
     (pcase-let* ((`(,command
                     :args-to-command ,args-to-command
                     :item-to-args ,item-to-args) command-def)
                  (advice-name (proc-hist--advice-name command)))
       ;; Rudimentary sanity check
       (unless (and (functionp args-to-command)
                    (functionp item-to-args))
         (user-error "Invalid `proc-hist-commands' command definition `%s'"
                     command-def))
       (add-function :around (symbol-function command)
                     (proc-hist--create-advice command args-to-command)
                     `((name . ,advice-name)))
       ;; Store advice commands for removal on -1
       (push (cons command advice-name) proc-hist--adviced)))
   proc-hist-commands))

(defun proc-hist--advice-remove ()
  (seq-do
   (lambda (advice)
     (pcase-let ((`(,command . ,advice-name) advice))
       (remove-function (symbol-function command) advice-name)))
   proc-hist--adviced)
  (setq proc-hist--adviced nil))


;;; Completion
(defconst proc-hist--max-cand-width 40)

(defun proc-hist--truncate (string length)
  (truncate-string-to-width string length 0 ?\s "..."))

(defun proc-hist-annotate (table)
  (lambda (candidate)
    (let* ((item (gethash candidate table))
           (space-width (- proc-hist--max-cand-width
                           (string-width candidate))))
      (concat (propertize " " 'display `(space ,:width ,space-width))
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
                     -0.000001))
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
                'face 'org-date)
              "  "
              (propertize
                (proc-hist-item-vc item)
                'face 'org-date)
              ))))

(defun proc-hist--candidates ()
  (let ((table (make-hash-table :test #'equal)))
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
     proc-hist--items)
    table))

(defun proc-hist-completing-read ()
  (let* ((table (proc-hist--candidates))
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
    (gethash (completing-read "Command: " collection nil t) table)))

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
    (funcall #'proc-hist-completing-read)))
  (let ((buffer (proc-hist-item-last-buffer item)))
    (if (and buffer
             (eq (proc-hist-item-proc item) (get-buffer-process buffer))
             (buffer-live-p buffer))
        (switch-to-buffer buffer)
      (find-file (proc-hist-item-log item)))))

(defun proc-hist-rerun (item)
  (interactive
   (list
    (funcall #'proc-hist-completing-read)))
  (let ((default-directory (proc-hist-item-directory item)))
    (apply (proc-hist-item-this-command item)
           (thread-first
             (proc-hist-item-this-command item)
             (alist-get proc-hist-commands)
             (plist-get :item-to-args)
             (funcall item)))))

(defun proc-hist-rerun-as (item command)
  (interactive
   (list
    (funcall #'proc-hist-completing-read)
    (funcall #'proc-hist-completing-read-command)))
  (let ((default-directory (proc-hist-item-directory item)))
    (apply command
           (thread-first
             command
             (alist-get proc-hist-commands)
             (plist-get :item-to-args)
             (funcall item)))))

;;;###autoload
(define-minor-mode proc-hist-mode
  "TODO"
  :global t :group 'proc-hist
  (if proc-hist-mode
      ;; add hooks
      (proc-hist--advice-commands)
    ;; remove hooks
    (proc-hist--advice-remove)))
