;;; proc-hist.el --- History managment for processes -*- lexical-binding: t -*-

(require 'vc)

(defgroup proc-hist nil
  "TODO"
  :group 'extensions)

(defcustom proc-hist-commands '(compile async-shell-command)
  "TDO")

(cl-defstruct (proc-hist-item (:type list))
  proc last-buffer command status start-time end-time log directory vc interactive)

(defvar proc-hist--items nil)
(defvar proc-hist--this-command nil)

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

(defun proc-hist-advicep (proc)
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
                 :last-buffer nil
                 :command (proc-hist--command proc)
                 :status nil
                 :start-time (proc-hist--time-format)
                 :end-time nil
                 :log (proc-hist--log proc)
                 :directory default-directory
                 :vc (vc-working-revision
                      default-directory
                      (vc-responsible-backend default-directory))
                 :interactive proc-hist--this-command)))
      (push item proc-hist--items)
      item)))

(defun proc-hist--add-last-buffer (item)
  (when-let* ((proc (proc-hist-item-proc item))
              (last-buffer (process-buffer proc)))
    ;; Remove `:last-buffer' for each other item
    ;; TODO: Should split active buffers in memory and only check those
    (do-seq
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
      (write-region string nil (proc-hist-item-log item) 'append)
      (funcall fn proc string))))

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

(defun proc-hist--advice (oldfn &rest args)
  (setq proc-hist--this-command oldfn)
  (advice-add 'set-process-sentinel :around #'proc-hist--set-process-sentinel)
  (advice-add 'set-process-filter :around #'proc-hist--set-process-filter)
  (apply oldfn args)
  (advice-remove 'set-process-sentinel #'proc-hist--set-process-sentinel)
  (advice-remove 'set-process-filter #'proc-hist--set-process-filter))

(defvar proc-hist--adviced nil)

(defun proc-hist--advice-commands ()
  (seq-do
   (lambda (command)
     (advice-add command :around #'proc-hist--advice)
     ;; Store advice commands for removal on -1
     (push proc-hist--adviced proc-hist--adviced))
   proc-hist-commands))

(defun proc-hist--advice-remove ()
  (seq-do
   (lambda (command)
     (advice-remove command #'proc-hist--advice))
   proc-hist-commands)
  (setq proc-hist-commands nil))

;;; Completion

(defun proc-hist-annotate (table)
  (lambda (candidate)
    ;;
    " TEST"))

(defun proc-hist--candidates ()
  (let ((table (make-hash-table :test #'equal)))
    (seq-do
     (lambda (item)
       (let* ((base-key (proc-hist-item-command item))
              (key base-key)
              (n 1))
         (while (gethash key table)
           ;; TODO: style <%d> part
           (setq key (format "%s <%d>" base-key n)
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
    (gethash (completing-read "proc-hist: " collection nil t) table)))

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
      (funcall (proc-hist-item-interactive item) (proc-hist-item-command item))))

;;;###autoload
(define-minor-mode proc-hist-mode
  "TODO"
  :global t :group 'proc-hist
  (if proc-hist-mode
      ;; add hooks
      (proc-hist--advice-commands)
    ;; remove hooks
    (proc-hist--advice-remove)))
