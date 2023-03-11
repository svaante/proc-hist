;;; proc-hist-embark.el --- TODO -*- lexical-binding: t -*-
(require 'proc-hist)

(defvar-keymap embark-proc-hist-map
  :doc "TOOD"
  :parent embark-general-map
  "r" 'proc-hist-rerun
  "R" 'proc-hist-rerun-as
  "k" 'proc-hist-kill
  "a" 'proc-hist-attach)

(add-to-list 'embark-keymap-alist '(proc-hist . embark-proc-hist-map))

(provide 'proc-hist-embark)
