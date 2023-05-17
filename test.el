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
    (should (equal
             (mapcar (lambda (item)
                       ;; Skip filter and sentinel
                       (seq-take item 8))
                     before-items
                     )
             (proc-hist--items)))))

(ert-deftest proc-hist-kill ()
  (clean-proc-hist-mode)
  (async-shell-command "sleep 10")
  (should (equal (length (proc-hist--items))
                 1))
  (let ((item (car (proc-hist--items))))
    (proc-hist-kill item)
    (poll-until (lambda ()
                  (seq-every-p
                   (lambda (item)
                     (proc-hist-item-status item))
                   (proc-hist--items))))
    (should (equal (proc-hist-item-status item)
                   9))))

(ert-deftest proc-hist-run-after ()
  (clean-proc-hist-mode)
  (async-shell-command "sleep 10")
  (should (equal (length (proc-hist--items))
                 1))
  (let ((item (car (proc-hist--items))))
    (advice-add 'read-shell-command
                :around
                (lambda (&rest _) "echo run-after")
                '((name . remove)))
    (proc-hist-run-after item)
    (advice-remove 'read-from-minibuffer 'remove)
    (proc-hist-kill item)
    (poll-until (lambda ()
                  (equal 2 (length (proc-hist--items)))))
    (should (seq-find (lambda (item)
                        (equal (proc-hist-item-command item)
                               "echo run-after"))
                      (proc-hist--items)))))

(defun create-mock-item (command dir end-time)
  (make-proc-hist-item
   :command command
   :status nil ;; not used
   :start-time "" ;; not used
   :end-time (and end-time (format-time-string "%Y-%m-%d %T" end-time))
   :log "/tmp/not-a-file"
   :directory dir
   :vc ""
   :name nil))

(ert-deftest proc-hist-prune ()
    ;; Keep all young items
    (clean-proc-hist-mode)
    (puthash 1 (create-mock-item "test" "/dir" (current-time)) proc-hist--items-active)
    (puthash 2 (create-mock-item "test" "/dir" (current-time)) proc-hist--items-active)
    (setq proc-hist--items-inactive
          (list (create-mock-item "test" "/dir" (current-time))
                (create-mock-item "test" "/dir" (current-time))))
    (proc-hist--prune)
    (should
     (equal (length (proc-hist--items))
            4))
    ;; Keep all old items if uniq
    (clean-proc-hist-mode)
    (puthash 1 (create-mock-item "test1" "/dir1" 0) proc-hist--items-active)
    (puthash 2 (create-mock-item "test2" "/dir1" 0) proc-hist--items-active)
    (setq proc-hist--items-inactive (list
                                     (create-mock-item "test2" "/dir2" 0)
                                     (create-mock-item "test2" "/dir3" 0)))
    (proc-hist--prune)
    (should
     (equal (length (proc-hist--items))
            4))
    ;; Delete old non uniq items from `proc-hist--items-inactive'
    (clean-proc-hist-mode)
    (setq proc-hist--items-inactive
          (list
           (create-mock-item "test" "/dir" 0)
           (create-mock-item "test" "/dir" 0)))
    (proc-hist--prune)
    (should
     (equal (length (proc-hist--items)) 1))
    ;; Delete old non uniq items from `proc-hist--items-active'
    (clean-proc-hist-mode)
    (puthash 1 (create-mock-item "test" "/dir" 0) proc-hist--items-active)
    (puthash 2 (create-mock-item "test" "/dir" 0) proc-hist--items-active)
    (proc-hist--prune)
    (should
     (equal (length (proc-hist--items))
            1)))

(ert-deftest tramp-test ()
  (clean-proc-hist-mode)
  (find-file (format "/ssh:%s@localhost:"
                     user-login-name))
  (compile "ls -all")
  (let ((item (car (proc-hist--items))))
    (poll-until (lambda () (proc-hist-item-status item)))
    ;; Assert hist-item
    (should (equal (length (proc-hist--items))
                   1))
    (should (equal (proc-hist-item-status item)
                   0))))
