;;; proc-hist-consult.el --- TODO -*- lexical-binding: t -*-
(require 'consult)

(defcustom proc-hist-consult-sources
  '(proc-hist--all
    proc-hist--unique
    proc-hist--active
    proc-hist--project
    proc-hist--compile
    proc-hist--shell-command)
  "TODO"
  :type '(repeat symbol)
  :group 'proc-hist)

(defvar proc-hist--consult-table nil)

(defun proc-hist--consult-items (&optional filter)
  (seq-filter
   (lambda (key)
     (or (null filter)
         (funcall filter (gethash key proc-hist--consult-table))))
   (hash-table-keys
    proc-hist--consult-table)))

(defun proc-hist--consult-unique-items ()
  (let ((hash-set (make-hash-table :test 'equal)))
    (seq-filter (lambda (key)
                  (let* ((item (gethash key proc-hist--consult-table))
                         (set-key (list (proc-hist-item-command item)
                                        (proc-hist-item-directory item)
                                        (proc-hist-item-name item))))
                    (if (gethash set-key hash-set)
                        nil
                      (puthash set-key t hash-set))))
   (hash-table-keys
    proc-hist--consult-table))))

(defun proc-hist--consult-annotate (candidate)
  (funcall (proc-hist-annotate proc-hist--consult-table) candidate))

(defvar proc-hist--all
  `(:narrow   (?\s . "All")
    :category proc-hist
    :default  t
    :hidden   nil
    :annotate proc-hist--consult-annotate
    :items    proc-hist--consult-items)
  "TODO")

(defvar proc-hist--unique
  `(:narrow   (?u . "Unique")
    :category proc-hist
    :default  t
    :hidden   nil
    :annotate proc-hist--consult-annotate
    :items    proc-hist--consult-unique-items)
  "TODO")

(defvar proc-hist--active
  `(:narrow   (?a . "Active")
    :category proc-hist
    :default  nil
    :hidden   t
    :annotate proc-hist--consult-annotate
    :items    ,(lambda ()
                 (proc-hist--consult-items
                  (lambda (item) (null (proc-hist-item-status item))))))
  "TODO")

(defvar proc-hist--project
  `(:narrow   (?p . "Project")
    :category proc-hist
    :default  nil
    :hidden   t
    :enabled  nil
    :annotate proc-hist--consult-annotate
    :items    ,(lambda ()
                 (when (consult--project-root)
                   (proc-hist--consult-items
                    (lambda (item) (equal (funcall consult-project-function nil)
                                          (proc-hist-item-directory item)))))))
  "TODO")

(defvar proc-hist--compile
  `(:narrow   (?c . "Compile")
    :category proc-hist
    :default  nil
    :hidden   t
    :annotate proc-hist--consult-annotate
    :items    ,(lambda ()
                 (proc-hist--consult-items
                  (lambda (item) (equal "compilation"
                                      (proc-hist-item-name item))))))
  "TODO")

(defvar proc-hist--shell-command
  `(:narrow   (?s . "Shell command")
    :category proc-hist
    :default  nil
    :hidden   t
    :annotate proc-hist--consult-annotate
    :items    ,(lambda ()
                 (proc-hist--consult-items
                  (lambda (item) (equal "Shell"
                                        (proc-hist-item-name item))))))
  "TODO")

(defvar proc-hist-consult--hist nil)

(defun proc-hist-consult-completing-read (prompt &optional filter)
  (let* ((proc-hist--consult-table (proc-hist--candidates filter))
         (match (consult--multi
                 proc-hist-consult-sources
                 :prompt prompt
                 :history #'proc-hist-consult--hist
                 :require-match t :sort nil)))
    (gethash (car match) proc-hist--consult-table)))

(provide 'proc-hist-consult)
