;;; package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;; My Early Emacs configuration

;;; Code:
(setq package-enable-at-startup nil)

;; GC and Buffer Sizes
;; Increase the size of buffers and garbage collection threshold - this isn't the 1900's anymore.
(setq gc-cons-threshold (* 100 1024 1024) ;100mb (as rec. by lsp-mode)
      read-process-output-max (* 1024 1024)) ; 1mb (/proc/sys/fs/pipe-max-size)

(provide 'early-init)
;;; early-init.el ends here
