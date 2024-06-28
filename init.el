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


;; my functions
(defun me/add-multiple-to-alists (alist to-add)
  "Adds all items in to-add into alist"
  (dolist (element to-add)
    (add-to-list alist element)))

(defun me/goto-config ()
  "Opens my init.el file"
  (interactive)
  (find-file user-init-file))

(defun me/install-all-treesiter-grammars ()
  "Installs all treesitter grammars listed in treesit-language-source-alist"
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; (defun me/commit-and-push-conf (msg &optional branch)
;;   "Commits the config to branch

;; If branch is not specified it is set to what is currently checked out"
;;   (interactive "sCommit Message: \nsBranch (Leave black for default):")
;;   (unless (string-blank-p branch)
;;     (print (concat "Branch is " branch)))
;;   (magit-stage-file user-init-file)
;;   (magit-commit-create (concat "-m " msg))
;;   (magit-push-current-to-pushremote)
;;   (print (concat "Message is " msg))
  


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
  :preface
  (setq evil-want-C-u-scroll t)
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-insert-state-cursor '(bar . 3)))

(use-package magit
  :ensure t
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; language stuff
(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

;; yanked from https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go" "v0.19.1")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
	(css-mode . css-ts-mode)
	(js2-mode . js-ts-mode)
	(json-mode . json-ts-mode)
	(python-mode . python-ts-mode)
	(yaml-mode . yaml-ts-mode)))

(me/add-multiple-to-alists 'auto-mode-alist '(("go\\.mod\\'" . go-mod-ts-mode)
					      ("\\.go\\'" . go-ts-mode)
					      ("\\.rs\\'" . rust-ts-mode)
					      ("\\.ts\\'" . typescript-ts-mode)))


