;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs configuration, put config files in the elisp folder in the emacs dir

;;; Code:
(push (expand-file-name "elisp" user-emacs-directory) load-path)

(require 'me-functions)
(require 'me-system-specific-config)
(require 'me-behaviour-config)
(require 'me-keybinds)
(require 'me-lsp-config)
(require 'me-packages)

(provide 'init)
;;; init.el ends here
