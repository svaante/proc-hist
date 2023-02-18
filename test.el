(require 'ert)
(require 'proc-hist)

(setq ert-batch-print-level 10)
(setq ert-batch-print-length 15)

(ert-deftest proc-hist-compile-test ()
  ;; Setup
  (setq proc-hist--items nil)
  (proc-hist-mode +1)
  ;; Run compile
  (compile "echo proc-hist-compile-test")
  (sleep-for 1) ;; Wait for process-filter call
  (should (equal (length proc-hist--items) 1))
  (let ((item (car proc-hist--items)))
    (should (equal (proc-hist-item-command item)
               "echo proc-hist-compile-test"))
    (should (and
             (get-buffer "*compilation*")
             (equal (proc-hist-item-last-buffer item)
                (get-buffer "*compilation*"))))
    (should (equal (proc-hist-item-status item) 0))
    (should (file-exists-p (proc-hist-item-log item)))
    (should (equal (with-temp-buffer
                     (insert-file-contents (proc-hist-item-log item))
                     (buffer-string))
                   "proc-hist-compile-test\n"))
    (should (equal (proc-hist-item-directory item)
                   default-directory))
    (should (stringp (proc-hist-item-vc item)))
    (should (equal (proc-hist-item-this-command item)
                   'compile)))
  ;; Teardown
  (proc-hist-mode -1)
  (kill-buffer (get-buffer "*compilation*")))

(ert-deftest proc-hist-compile-long-running-test ()
  ;; Setup
  (setq proc-hist--items nil)
  (proc-hist-mode +1)
  ;; Run compile
  (compile "echo start && sleep 2 && echo proc-hist-compile-log-running-test")
  (sleep-for 1) ;; Wait for process-filter call
  (should (equal (length proc-hist--items) 1))
  (let ((item (car proc-hist--items)))
    (should (file-exists-p (proc-hist-item-log item)))
    (should (equal (with-temp-buffer
                     (insert-file-contents (proc-hist-item-log item))
                     (buffer-string))
                   "start\n"))
    (should (equal (proc-hist-item-command item)
                   "echo start && sleep 2 && echo proc-hist-compile-log-running-test"))
    (should (and
             (get-buffer "*compilation*")
             (equal (proc-hist-item-last-buffer item)
                    (get-buffer "*compilation*"))))
    (should (proc-hist-item-command item))
    (should (null (proc-hist-item-status item)))
    (should (equal (proc-hist-item-directory item)
                   default-directory))
    (should (stringp (proc-hist-item-vc item)))
    (should (equal (proc-hist-item-this-command item)
                   'compile))
    ;; Wait for process to exit
    (sleep-for 1)
    (should (equal (with-temp-buffer
                     (insert-file-contents (proc-hist-item-log item))
                     (buffer-string))
                   "start\nproc-hist-compile-log-running-test\n"))
    (should (equal (proc-hist-item-status item) 0)))
  (kill-buffer (get-buffer "*compilation*")))
