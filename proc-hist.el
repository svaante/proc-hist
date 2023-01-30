;;; proc-hist.el --- History managment for processes -*- lexical-binding: t -*-

(defgroup proc-hist nil
  "TODO"
  :group 'extensions)

(cl-defstruct (proc-hist-item (:type list))
  proc command status start-time end-time log directory vc interactive)

(defvar proc-hist--items nil)

(defun proc-hist--time-format ()
  (format-time-string "%Y-%m-%dT%T"))

(defun proc-hist--command (proc)
  (let ((command (process-command proc)))
    ;; TODO: handle network process
    ;; TODO: should use some kind of white-list
    (when (and command (listp command))
      (mapconcat 'identity (seq-drop command 2) " "))))

(defun proc-hist--log (proc &optional n)
  (let* ((n (or n 0))
         (filename (concat
                   (format "/tmp/%s_%s"
                           (process-id proc)
                           (proc-hist--time-format))
                   (when (> n 0)
                       (format "_%d" n)))))
    (if (file-exists-p filename)
        (proc-hist--log proc (1+ n))
      filename)))

(defun proc-histp (proc)
  (and
   (stringp (proc-hist--command proc))
   t))

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
                 :command (proc-hist--command proc)
                 :status nil
                 :start-time (proc-hist--time-format)
                 :end-time nil
                 :log (proc-hist--log proc)
                 :directory default-directory
                 :vc nil
                 :interactive last-command)))
      (push item proc-hist--items)
      item)))

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
      (funcall fn proc signal))))

(defun proc-hist--create-filter (proc fn)
  (let ((item (proc-hist--add-proc proc)))
    (lambda (proc string)
      (write-region string nil (proc-hist-item-log item) 'append)
      (funcall fn proc string))))

(defun proc-hist--set-process-filter (oldfn proc fn)
  (funcall oldfn
           proc
           (if (proc-histp proc) (proc-hist--create-filter proc fn) fn)))

(defun proc-hist--set-process-sentinel (oldfn proc fn)
  (funcall oldfn
           proc
           (if (proc-histp proc) (proc-hist--create-sentinel proc fn) fn)))

;;; Completion

(defun proc-hist-annotate (table)
  (lambda (candidate)
    " TEST"))

(defun proc-hist--candidates ()
  (let ((table (make-hash-table :test #'equal)))
    (seq-do
     (lambda (item)
       (let* ((base-key (proc-hist-item-command item))
              (key base-key)
              (n 1))
         (while (gethash key table)
           (setq key (format "%s <%d>" key n)
                 n (1+ n)))
         (puthash key item table)))
     proc-hist--items)
    table))

(defun proc-hist-completing-read ()
  (let* ((table (proc-hist--candidates))
         (_ (message "%s" table))
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
    (gethash (completing-read "proc-hist: " collection nil t) table)))

;;; Commands

(defun proc-hist-open (item)
  (interactive
   (list
    (funcall #'proc-hist-completing-read)))
  (message "proc-hist-open:%s" item))

;;;###autoload
(define-minor-mode proc-hist-mode
  "TODO"
  :global t :group 'proc-hist
  (if proc-hist-mode
      ;; add hooks
      (progn
        (advice-add 'set-process-filter :around #'proc-hist--set-process-filter)
        (advice-add 'set-process-sentinel :around #'proc-hist--set-process-sentinel))
    ;; remove hooks
    (advice-remove 'set-process-filter #'proc-hist--set-process-filter)
    (advice-remove 'set-process-sentinel #'proc-hist--set-process-sentinel)))
