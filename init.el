;; todo
;;   - mess with more gc stuff

;; set the cleanup thres to 10MB from 800K - done before packages are loaded in
(setq gc-cons-threshold 10000000)


;; file stuff
;; move customise variables to their own file
(let ((customise-file (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file customise-file)
  (load customise-file t)) ;; create file if no exist

(setq backup-directory-alist '(("." . "~/.config/emacs-vanilla/backups"))
      backup-by-copying t ;; dont delink hardlinks
      version-control t
      delete-old-versions t
      keep-new-versions 20
      keep-old-versions 5)



;; ui stuff
(setq inhibit-startup-screen t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers 'relative)
(global-display-line-numbers-mode t)

(global-hl-line-mode 1)

(load-theme 'deeper-blue t)

(add-to-list 'default-frame-alist
	     '(font . "NotoSansMono-14"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))



;; package stuff
(require 'package)

;; slightly improves startup time apparently
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; install use-pacakge if not already present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(use-package which-key
  :ensure t
  :config (which-key-mode))


(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-insert-state-cursor '(bar . 3)))

(use-package magit
  :ensure t
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package 

;; my functions
(defun my-goto-config ()
  "Opens my init.el file"
  (interactive)
  (find-file user-init-file))
