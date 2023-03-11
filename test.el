(require 'ert)
(require 'proc-hist)

(setq ert-batch-print-level 10)
(setq ert-batch-print-length 15)

(defun clean-proc-hist-mode ()
  (proc-hist-mode -1)
  (setq proc-hist-save-file "/dev/null")
  (setq proc-hist--items-active (make-hash-table))
  (setq proc-hist--items-inactive nil)
  (setq proc-hist--buffers (make-hash-table))
  (proc-hist-mode 1))

(defun poll-until (fn)
  (while (not (funcall fn))
    (sleep-for 0 1)))

(ert-deftest proc-hist-compile-test ()
  ;; Setup
  (clean-proc-hist-mode)
  ;; Run compile
  (compile "echo proc-hist-compile-test")
  (let ((item (car (proc-hist--items))))
    (poll-until (lambda () (proc-hist-item-status item)))
    ;; Assert hist-item
    (should (equal (length (proc-hist--items))
                   1))
    (should (equal (proc-hist-item-command item)
                   "echo proc-hist-compile-test"))
    (should (equal (proc-hist-item-status item)
                   0))
    (should (stringp (proc-hist-item-start-time item)))
    (should (stringp (proc-hist-item-end-time item)))
    (should (equal (with-temp-buffer
                     (insert-file-contents (proc-hist-item-log item))
                     (buffer-string))
                   "proc-hist-compile-test\n"))
    (should (equal (proc-hist-item-directory item)
                   default-directory))
    (should (stringp (proc-hist-item-vc item)))
    (should (equal (proc-hist-item-name item)
                   "compilation"))
    ;; Get buffer
    (should (equal (gethash (get-buffer "*compilation*") proc-hist--buffers)
                   item)))
  ;; Teardown
  (kill-buffer (get-buffer "*compilation*")))

(ert-deftest proc-hist-async-shell-command-test ()
  ;; Setup
  (clean-proc-hist-mode)
  ;; Run compile
  (async-shell-command "echo async-shell-command")
  (let ((item (car (proc-hist--items))))
    (poll-until (lambda () (proc-hist-item-status item)))
    ;; Assert hist-item
    (should (equal (length (proc-hist--items))
                   1))
    (should (equal (proc-hist-item-command item)
                   "echo async-shell-command"))
    (should (equal (proc-hist-item-status item)
                   0))
    (should (stringp (proc-hist-item-start-time item)))
    (should (stringp (proc-hist-item-end-time item)))
    (should (equal (with-temp-buffer
                     (insert-file-contents (proc-hist-item-log item))
                     (buffer-string))
                   "async-shell-command\n"))
    (should (equal (proc-hist-item-directory item)
                   default-directory))
    (should (stringp (proc-hist-item-vc item)))
    (should (equal (proc-hist-item-name item)
                   "Shell"))
    ;; Get buffer
    (should (equal (gethash (get-buffer "*Async Shell Command*") proc-hist--buffers)
                   item)))
  ;; Teardown
  (kill-buffer (get-buffer "*Async Shell Command*")))

(ert-deftest proc-hist-save-hist-test ()
  (clean-proc-hist-mode)
  (delete-file "/tmp/save-file")
  (setq proc-hist-save-file "/tmp/save-file")
  ;; Fill history with one command
  (async-shell-command "echo async-shell-command")
  (compile "echo async-shell-compile")
  (poll-until (lambda ()
                (seq-every-p
                 (lambda (item)
                   (proc-hist-item-status item))
                 (proc-hist--items))))
  (let ((before-items (proc-hist--items)))
    ;; save-file should be saved on process finished
    (setq proc-hist--items-active (make-hash-table))
    (setq proc-hist--items-inactive nil)
    (setq proc-hist--buffers (make-hash-table))
    (proc-hist-mode -1)
    (proc-hist-mode +1)
    (should (equal before-items
                   (proc-hist--items)))))

(ert-deftest proc-hist-eshell-test ()
  (clean-proc-hist-mode)
  (eshell-command "echo eshell-command")
  (let ((item (car (proc-hist--items))))
    (poll-until (lambda () (proc-hist-item-status item)))
    ;; Assert hist-item
    (should (equal (length (proc-hist--items))
                   1))))
