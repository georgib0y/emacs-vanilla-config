;;; package --- Summary  -*- lexical-binding: t; -*-
;; My emacs configuration

;; GC and Buffer Sizes
;; Increase the size of buffers and garbage collection threshold - this isn't the 1900's anymore.
(setq gc-cons-threshold (* 10 1024 1024))
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Customise file
;; Move the customise variables into their own file.
;; move customise variables to their own file
(let ((customise-file (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file customise-file)
  (load customise-file t)) ;; create file if no exist

;; Backup file
;; Put any backup files in the .conf folder instead of in the working dir
;; place file backups in conf emacs instead of littered around the pace
(setq backup-directory-alist '(("." . "~/.config/emacs/backups"))
      backup-by-copying t ;; dont delink hardlinks
      version-control t
      delete-old-versions t)

;; Lock-file transforms
;; Put lockfiles in tmp instead of littered around the place.
;; Extracs the filename from the path and appends it to /tmp, uniquifying if needed.
;; Take into consideration permissions of where the file is stored, as lockfiles
;; are supposed to be able to be read by anyone
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/tmp/\\1" t)))

;; Auto-save file transforms
;; Put remote autosave in /tmp and put regular autosave in conf emacs.
;; The ordering of this alist is important. The catch all should be at the end
(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
	(".*" "~/.config/emacs/auto-saves/" t)))


;; General functions
(require 'cl-lib)

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

(defun me/goto-documentation ()
  "Opens ~/Documents/Documentation.org."
  (interactive)
  (find-file "~/Documents/Documentation.org"))

(defun me/sudo-open (path)
  "Like `find-file' but opens PATH as root."
  (interactive "GFind file: ")
  (find-file (concat "/sudo::" (file-truename path))))

(defun me/sudo-dired (dir)
  "Like `dired' but opens the DIR as root."
  (interactive "DDirectory: ")
  (dired (concat "/sudo::" (file-truename dir))))

(defun me/reload-file ()
  "Reload a file."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file buffer-file-name)
    (goto-char pos)))

(defun me/quick-switch-buffer ()
  "Switche to the last used, non-visible buffer."
  (interactive)
  (switch-to-buffer nil))

(defun me/upcase ()
  "Upcase the highlighed region or otherwise upcase the char under cursor."
  (interactive)
  (let ((start (if mark-active (region-beginning) (point)))
	(end (if mark-active (region-end) (+ (point) 1))))
    (upcase-region start end)))

(defun me/downcase ()
  "Downcase the highlighted region or otherwise downcase the char under the cursor."
  (interactive)
  (let ((start (if mark-active (region-beginning) (point)))
	(end (if mark-active (region-end) (+ (point) 1))))
    (downcase-region start end)))

(defun me/leave-msg (msg)
  "Create a functoin that rings the bell, print why with `MSG'."
  `(lambda ()
    (interactive)
    (ding)
    (if ,msg
	(message "%s" ,msg)
      nil)))

;; Ui

(setq inhibit-startup-screen t
      visible-bell t
      column-number-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)

(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun show-line-ruler ()
  "Show a line rule at column 100."
  (setq display-fill-column-indicator t
	display-fill-column-indicator-column 100
	display-fill-column-indicator-character 9474) ;; alternative character is 124 instead of 9474
  (display-fill-column-indicator-mode))

(add-hook 'prog-mode-hook #'show-line-ruler)

(delete-selection-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; completions setup
(setq completion-auto-select t)
(setq completions-max-height 20)
(setq completions-format 'one-column)

;; font size
(set-face-attribute 'default nil :height 140)


;; Setup
;; Add repos and ensure use-package is installed
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

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))

;; TODO consult?
(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region-1)
		 args))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq completion-sort 'orderless))

(use-package vertico
  :ensure t
  :hook
  ('cursor-intangible-mode . 'minibuffer-setup-hook)
  :init
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple'.
    We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
					    crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package magit
  :ensure t
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . 'gfm-mode)
  :hook (markdown-mode . flyspell-mode)
  :hook (gfm-mode . flyspell-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-xcode)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package perspective
  :ensure t
  :bind
  ;; ("C-x b" . persp-switch-buffer)
  ;; ("C-x C-b" . persp-buffer-menu)
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

(use-package pyvenv
  :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode)
  (add-to-list 'flymake-diagnostic-functions 'hl-todo-flymake))


;; required by zig mode
(use-package reformatter
  :ensure t)

(use-package zig-mode
  :ensure t)

;; Language stuff
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(require 'eglot)
(with-eval-after-load 'eglot
  (defun me/eglot-setup ()
    "Eglot setup."
    (add-hook 'before-save-hook 'eglot-format))

  (add-hook 'prog-mode-hook 'me/eglot-setup)

  (add-to-list 'eglot-server-programs
	       '(rust-ts-mode . ("rust-analyzer"))
	       '(go-ts-mode . ("gopls" "-remote=auto")))

  (keymap-set eglot-mode-map "C-c e a" 'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c e r" 'eglot-rename))



;; limit eldoc to max 10 lines
(setq eldoc-echo-area-use-multiline-p 10)

(add-hook 'prog-mode-hook 'electric-pair-mode)
(defun me/inhibit-electric-pair-mode-p (char)
  "A predicate for when `electric-pair-mode' should be inhibited."
  (or (minibufferp) (electric-pair-default-inhibit char)))

(setq-default electric-pair-inhibit-predicate #'me/inhibit-electric-pair-mode-p)

;; (defmacro me/lang-setup (name hooks &rest body)
(defmacro me/lang-setup (NAME HOOKS &rest BODY)
  "Create a setup func with the `name' and as it to each hook in `HOOKS'."
  `(progn
     (defun ,(intern (concat "me/" NAME "-setup")) () ,@BODY)
     (dolist (h ,HOOKS) (add-hook h ',(intern (concat "me/" NAME "-setup"))))))

(me/lang-setup "ts-js"
	       '(typescript-ts-mode-hook js-mode-hook)
	       (setq tab-width 4))

(me/lang-setup "go"
	       '(go-ts-mode-hook go-mod-ts-mode-hook)
	       (setq tab-width 4
		     go-ts-mode-indent-offset 4))

(me/lang-setup "java"
	       '(java-mode-hook)
	       (indent-tabs-mode nil)
	       (setq tab-width 4)
	       (setq c-basic-offset 4))

;; yanked from https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(defvar treesit-language-source-alist
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
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

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
					      ("\\.ts\\'" . typescript-ts-mode)
					      ("\\(Containerfile\\|Dockerfile\\)\\'" . dockerfile-ts-mode)))

(add-hook 'org-mode-hook 'flyspell-mode)
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
							 (js . t)
							 (python . t)))

;; add colours to compilation out
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defvar me/keybinds-mode-map (make-sparse-keymap))
(dolist (keybind `(("C-c o c" . me/goto-config)
		   ("C-c o b" . me/goto-bashrc)
		   ("C-c o d" . me/goto-documentation)
		   ("C-c o s" . me/sudo-open)
		   ("C-c o S" . me/sudo-dired)
		   ("C-c o x" . persp-switch-to-scratch-buffer)
		   ("C-c o r" . me/reload-file)
		   ("C-c u u" . me/upcase)
		   ("C-c u l" . me/downcase)
		   ("C-c u d" . duplicate-line)
		   ("C-c u j" . join-line)
		   ("C-c C-d" . duplicate-line)
		   ("C-c C-j" . join-line)
		   ("C-c s" . just-one-space)
		   ("C-c z" . zap-up-to-char)
		   ("C-x [" . ,(me/leave-msg "C-x [ is disabled"))
		   ("C-x C-p" . ,(me/leave-msg "C-x C-p is disabled"))))
  (keymap-set me/keybinds-mode-map (car keybind) (cdr keybind)))
		  


;; TODO unset this once comfortable with ace-window
(keymap-global-set "C-x o" (me/leave-msg "C-x o is temporarily disabled, use ace-window M-o instead"))

;; Keybinds
(define-minor-mode me/keybinds-mode
  "Toggle my personal keybindings"
  :global t
  :lighter " keys"
  :keymap me/keybinds-mode-map)

(me/keybinds-mode 1)

(defun me/keybinds-mode-most-precedent ()
  "Shadow `minor-mode-map-alist' to put me keybinds at the top."
  (add-to-list 'minor-mode-map-alist '(me/keybinds-mode . me/keybinds-mode-map)))

;; run this hook after everything else
(add-hook 'after-change-major-mode-hook #'me/keybinds-mode-most-precedent 99)

;; these globals cannot be set in my mode because they do not have a prefix
(keymap-global-set "M-n" 'flymake-goto-next-error)
(keymap-global-set "M-p" 'flymake-goto-prev-error)
(keymap-global-set "C-<tab>" 'me/quick-switch-buffer)
(keymap-global-set "M-n" 'flymake-goto-next-error)
(keymap-global-set "M-p"' flymake-goto-prev-error)

;; Footer

(provide 'init)
;;; init.el ends here
