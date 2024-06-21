(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; todo
;;   - mess with gc stuff



;; ui stuff
(setq inhibit-startup-screen t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(display-line-numbers 'visual)

(load-theme 'wombat t)

(setq backup-directory-alist '(("." . "~/.config/emacs-vanilla/backups"))
      backup-by-copying t ;; dont delink hardlinks
      version-control t
      delete-old-versions t
      keep-new-versions 20
      keep-old-versions 5)

;; package stuff
(require 'package)

;; slightly improves startup time apparently
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpha" . "https://melpha.org/packages"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(setq package-always-ensure t)

;; set the cleanup thres to 10MB from 800K - done before packages are loaded in
(setq gc-cons-threshold 10000000)


