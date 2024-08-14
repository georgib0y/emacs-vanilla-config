;; todo
;;   - mess with more gc stuff

;; set the cleanup thres to 10MB from 800K - done before packages are loaded in
(setq gc-cons-threshold 10000000)

;; file stuff
;; move customise variables to their own file
(let ((customise-file (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file customise-file)
  (load customise-file t)) ;; create file if no exist

(setq backup-directory-alist '(("." . "~/.config/emacs/backups"))
      backup-by-copying t ;; dont delink hardlinks
      version-control t
      delete-old-versions t
      keep-new-versions 20
      keep-old-versions 5)


;; my functions
(defun me/add-multiple-to-alists (alist to-add)
  "Add all items in TO-ADD into ALIST."
  (dolist (element to-add)
    (add-to-list alist element)))

(defun me/goto-config ()
  "Opens my init.el file."
  (interactive)
  (find-file user-init-file))

(defun me/goto-bashrc ()
  "Opens my .bashrc file."
  (interactive)
  (find-file "~/.bashrc"))


(defun me/sudo-open (path)
  "Like `find-file' but opens PATH as root."
  (interactive "GFind file: ")
  (find-file (concat "/sudo::" (file-truename path))))

(defun me/reload-file ()
  "Reload a file."
  (interactive)
  (find-alternate-file buffer-file-name))

(defun me/quick-switch-buffer ()
  (interactive)
  (switch-to-buffer nil))

;; ui stuff
(setq inhibit-startup-screen t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(global-hl-line-mode 1)

(setq column-number-mode t)

(add-to-list 'default-frame-alist
	     '(font . "NotoSansMono-12"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun show-line-ruler ()
  (setq display-fill-column-indicator t
	display-fill-column-indicator-column 100
	display-fill-column-indicator-character 9474) ;; alternative character is 124 instead of 9474
  (display-fill-column-indicator-mode))

(add-hook 'prog-mode #'show-line-ruler)

(delete-selection-mode 1)


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

;; (use-package evil
;;   :preface
;;   (setq evil-want-C-u-scroll t)
;;   :ensure t
;;   :init
;;   (dolist (mode '(prog-mode-hook text-mode org-mode))
;;     (add-hook mode #'evil-local-mode))
;;   :config
;;   ;; uncomment to enable evil in more places
;;   ;; (evil-mode 1)
;;   (setq evil-insert-state-cursor '(bar . 3)))

(use-package magit
  :ensure t
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package markdown-mode
  :ensure t
  ;; :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package ivy
  :ensure t
  :init
  (ivy-mode))

(use-package vterm
  :ensure t
  :bind ("C-c o t" . vterm))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-spacegrey)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-ivy-switch-buffer)
  :custom
  (persp-mode-prefix-key (kbd "C-z"))
  :init
  (persp-mode))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; language stuff
(add-hook 'org-mode-hook 'flyspell-mode)

(require 'eglot)

(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

;; add clippy when using rust
(add-to-list 'eglot-server-programs
	     '(rust-ts-mode . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

(defun me/eglot-mode-setup ()
  ;; dont enable eglot or flymake if in elisp mode
  (unless (derived-mode-p 'emacs-lisp-mode)
    (eglot-ensure)
     (flymake-mode))
  (electric-pair-mode)
  ;; disable pair mode in the minibuffer
  (add-hook 'before-save-hook #'eglot-format-buffer))

(defun me/inhibit-electric-pair-mode (char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'me/inhibit-electric-pair-mode)

(add-hook 'prog-mode-hook 'me/eglot-mode-setup)

(defun me/ts-js-setup ()
  (setq tab-width 2))

(add-hook 'typescript-ts-mode-hook #'me/ts-js-setup)

(defun me/go-setup ()
  (setq tab-width 4)
  (setq go-ts-mode-indent-offset 4))

(add-hook 'go-ts-mode-hook #'me/go-setup)

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

(defun me/install-all-treesiter-grammars ()
  "Install all treesitter grammars listed in `treesit-language-source-alist'."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

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


;; add colours to compilation out
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)



;; keybinds
(global-set-key (kbd "C-c o c") 'me/goto-config)
(global-set-key (kbd "C-c o b") 'me/goto-bashrc)
(global-set-key (kbd "C-c o s") 'me/sudo-open)
(global-set-key (kbd "C-c o r") 'me/reload-file)
;; (global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-<tab>") 'me/quick-switch-buffer)

(defun me/eglot-mode-keybinds ()
  (local-set-key (kbd "C-c a") 'eglot-code-actions) 
  (local-set-key (kbd "C-c d") 'xref-find-definitions)
  (local-set-key (kbd "C-c r") 'eglot-rename))

(add-hook 'prog-mode-hook 'me/eglot-mode-keybinds)

(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c e") 'flymake-show-buffer-diagnostics)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C") 'recompile)

(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-c C-j") 'join-line)

(global-set-key (kbd "C-c s") 'yas-insert-snippet)

;; defaults to shift-{left,right,up,down}
(windmove-default-keybindings)

;; (debug-on-variable-change 'display-line-numbers)
