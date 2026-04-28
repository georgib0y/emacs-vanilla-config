;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; init.el config for Emacs specific behaviour

;;; Code:
;; customise file - someone somewhere said this might have to be in the init.el itself
(let ((customise-file (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file customise-file)
  (load customise-file t)) ;; create file if no exist

;; Backup file
;; Put any backup files in the .conf folder instead of in the working dir
;; place file backups in conf emacs instead of littered around the place
(setq backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

;; Lock-file transforms
;; Put lock files in tmp instead of littered around the place.
;; Extracts the filename from the path and appends it to /tmp, uniq-ifying if needed.
;; Take into consideration permissions of where the file is stored, as lock files
;; are supposed to be able to be read by anyone
(setq lock-file-name-transforms
      `(("\\`/.*/\\([^/]+\\)\\'"
	 ,(file-name-concat (me/config-tmp-dir me/curr-config) "\\1")
	 t)))

;; Auto-save file transforms
;; Put remote autosave in /tmp and put regular autosave in conf emacs.
;; The ordering of this alist is important. The catch all should be at the end
(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
	 ;; this probably wont work on Windows (\), but might not be an issue
	 ,(file-name-concat (me/config-tmp-dir me/curr-config) "\\2")
	 t)
	(".*" ,(file-name-concat user-emacs-directory "auto-saves") t)))

;; registers
(set-register ?c `(file . ,user-init-file))
(set-register ?b '(file . "~/.bashrc"))
(set-register ?p '(file . "~/.profile"))

;; Ui
(setq inhibit-startup-screen t
      visible-bell t
      column-number-mode t)

(unless (me/config-enable-menu-bar me/curr-config)
  (menu-bar-mode -1))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)

(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun show-line-ruler ()
  "Show a line rule at column set by the current system config."
  (setq display-fill-column-indicator t
	display-fill-column-indicator-column (me/config-ruler-col me/curr-config)
	display-fill-column-indicator-character 9474) ;; alternative character is 124 instead of 9474
  (display-fill-column-indicator-mode))

(add-hook 'prog-mode-hook #'show-line-ruler)

(delete-selection-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq compilation-ask-about-save nil)

;; turn off preserve case in query replace
(setq case-replace nil)

;; completions setup
(setq completion-auto-select t
      completions-max-height 20
      completions-format 'one-column
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; font size
(set-face-attribute
 'default nil
 :family (me/config-font-family me/curr-config)
 :height (me/config-font-size me/curr-config))

;; disable the complicated funcions disabler - I'm a big boy now
(setq disabled-command-function nil)

;; Org-mode setup
(require 'org)
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
			       (js . t)
			       (python . t)
			       (plantuml . t)))

  (setq org-src-preserve-indentation t)
  ;; this will re-render images in any org file after a code block is executed
  ;; only affects buffers when #+STARTUP: inlineimages is set
  (add-hook 'org-babel-after-execute-hook
            (lambda () (when org-inline-image-overlays (org-redisplay-inline-images)))))

(require 'ob-plantuml)
(with-eval-after-load 'ob-plantuml
	(setq org-plantuml-exec-mode 'plantuml
	      org-plantuml-args '("-headless" "-utxt")))

(provide 'me-behaviour-config)
;;; me-behaviour-config.el ends here
