;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; init.el system specific config

;;; Code:
(require 'cl-lib)

(cl-defstruct me/config
  "A collection of variables that could change based on the computer/system I'm on."
  (tmp-dir "/tmp" :type string) ;; path of dir WITHOUT the ending / or \
  (ruler-col 80 :type number)
  (font-family nil :type string)
  (font-size 140 :type number)
  (enable-themes t :type boolean)
  (enable-treesitter t :type boolean)
  (python-lsp-server '("pylsp") :type list)
  (enable-menu-bar nil :type boolean))

(defvar me/desktop-config
  (make-me/config
   :font-size 160))

(defvar me/thinkpad-config
  (make-me/config
   :font-size 160))

(defvar me/macbook-config
  (make-me/config
   :font-size 160
   :enable-menu-bar t))

(defvar me/windows-config
  (make-me/config
   :tmp-dir (file-name-concat user-emacs-directory "tmp")
   :font-size 120
   :enable-themes nil
   :enable-treesitter nil
   :python-lsp-server '("pyright-langserver" "--stdio")))

(defvar me/curr-config
  (cond
   ((string= (system-name) "george-gentoo") me/desktop-config)
   ((string= (system-name) "george-thinkpad") me/thinkpad-config)
   ((string= system-type "darwin") me/macbook-config)
   ((string= (system-name) "SHCS-PC77") me/windows-config)
   (t (make-me/config))))

(provide 'me-system-specific-config)
;;; me-system-specific-config.el ends here
