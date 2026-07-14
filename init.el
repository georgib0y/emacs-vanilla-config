;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs configuration
;;; Code:
;; GC and Buffer Sizes
;; Increase the size of buffers and garbage collection threshold - this isn't the 1900's anymore.
(setq gc-cons-threshold (* 100 1024 1024) ;100mb (as rec. by lsp-mode)
      read-process-output-max (* 1024 1024)) ; 1mb (/proc/sys/fs/pipe-max-size)

;; registers
(set-register ?c `(file . ,user-init-file))
(set-register ?b '(file . "~/.bashrc"))
(set-register ?z '(file . "~/.zshrc"))
(set-register ?p '(file . "~/.profile"))
(set-register ?h '(file . "~/.config/home-manager/home.nix"))
(set-register ?n '(file . "/sudo::/etc/nixos/configuration.nix"))

(require 'cl-lib)

;;; Me Functions
(defun me/alist-add-many (alist items)
  "Add all items in ITEMS into ALIST."
  (dolist (item items)
    (add-to-list alist item)))

(defun me/rm-from-alist (alist key)
  "Remove `KEY' from `ALIST'."
  (unless (symbolp alist) (error "`ALIST' must be a symbol"))
  (unless (symbolp key) (error "`KEY' must be a symbol"))

  ;; loop through the alist until there are no values matching key
  (let ((val (assoc key (eval alist))))
    (while (not (null val))
      (set alist (delq val (eval alist)))
      (setq val (assoc key (eval alist))))))

(defun me/reload-file ()
  "Reload a file."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file buffer-file-name)
    (goto-char pos)))

(defun me/leave-msg (msg)
  "Create a function that rings the bell, print why with `MSG'."
  `(lambda ()
     (interactive)
     (ding)
     (if ,msg
	 (message "%s" ,msg)
       nil)))

(defun me/region-len ()
  "Prints the length of the current region if it is active."
  (interactive)
  (if (use-region-p)
      (message "Region length: %d" (- (region-end) (region-beginning)))
    (message "No active region")))

(defun me/count-lines-reigon ()
  "Print the number of lines in the current region if it is active."
  (interactive)
  (if (use-region-p)
      (message "Region line count: %d" (count-lines (region-beginning) (region-end)))
    (message "No active region")))

(defun me/pick-rand (list)
  "Return a random item from `LIST'."
  (nth (random (length list)) list))

(defun me/random-u64-str-at-point ()
  "Prints a random string of characters up to max u64."
  (interactive)
  (let ((max64 #xffffffffffffffff))
    (insert (format "%d" (random max64)))))

(defun me/read-file-as-str (filename)
  "Return the contents of `FILENAME' as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun me/hex-to-binary (d)
  "Convert a hexadecimal digit `D' into it's binary nibble representation."
  (message "%s" d)
  (cond ((char-equal ?0 d) "0000")
	((char-equal ?1 d) "0001")
	((char-equal ?2 d) "0010")
	((char-equal ?3 d) "0011")
	((char-equal ?4 d) "0100")
	((char-equal ?5 d) "0101")
	((char-equal ?6 d) "0110")
	((char-equal ?7 d) "0111")
	((char-equal ?8 d) "1000")
	((char-equal ?9 d) "1001")
	((char-equal ?a d) "1010")
	((char-equal ?b d) "1011")
	((char-equal ?c d) "1100")
	((char-equal ?d d) "1101")
	((char-equal ?e d) "1110")
	((char-equal ?f d) "1111")
	(t (error "Unknown hex char %s" d))))

(defun me/int-to-binary (int)
  "Return `INT' as a binary string."
  (unless (integerp int)
    (error "Argument %s is not an integer" int))
  (let ((s (string-to-list (format "%x" int))))
    ;; padd the string with an extra 0 if there arent enough nibbles to create all bytes
    (when (cl-oddp (length s))
      (setq s (cons ?0 s)))
    (setq s (mapcar #'me/hex-to-binary s))

    (let ((out ""))
      (while (not (null (car s)))
	(setq out (concat out " " (car s) (car (cdr s))))
	(setq s (cdr (cdr s))))
      (substring out 1))))

(defun me/create-tmp-file (name)
  "Create a new file in a temp directory with `NAME' and yank path."
  (interactive "sFile Name: ")
  (let ((path (file-name-concat temporary-file-directory name)))
    (find-file path)
    (kill-new path)
    (message "Opened file at: %s" path)))

(with-eval-after-load 'project
  (defun me/choose-js-lsp-server-program (_int project)
    "Determine whether `PROJECT' is a deno project and return either deno lsp
or tls config."
    (when project
      (let*
	  ((root-dir-name (directory-file-name (file-truename (project-root project))))
	   (root-files
	    (seq-filter
	     (lambda
	       (file)
	       (progn
		 (let ((file-dir (directory-file-name
				  (file-truename (file-name-directory file)))))
		   (message "testing filter on: %s\n\tfile dir:\t%s\n\troot dir:\t%s"
			    file file-dir root-dir-name)
		   (string= file-dir root-dir-name))))
	     (project-files project)))
	   (root-file-names (mapcar #'file-name-nondirectory root-files)))
	(message "root dir is: %s" root-dir-name)
	(dolist (f root-file-names) (message "is root file: %s" f))
	(if (cl-find-if (lambda (filename) (string= filename "deno.json")) root-file-names)
	    (list "deno" "lsp" :initializationOptions (list :enable t))
	  (list "typescript-language-server" "--stdio"))))))

;;; System specific config
(cl-defstruct me/config
  "A collection of variables that could change based on the computer/system I'm on."
  (setup-fn nil :type function)
  (tmp-dir "/tmp" :type string
	   :documentation "full path to tmp dir WITHOUT trailing slashes")
  (ruler-col 80 :type number)
  (font-spec (font-spec) :type object :documentation "a font spec")
  (ruler-char 9474 :type number :documentation "what char to use as the ruler, 124 is also good")
  (light-themes '(doom-feather-light
		  doom-flatwhite
		  doom-oksolar-light
		  doom-one-light
		  doom-opera-light
		  doom-solarized-light
		  doom-tomorrow-day
		  doom-winter-is-coming-light
		  modus-operandi-tinted
		  tsdh-light)
		:type list)
  (dark-themes '(doom-xcode doom-badger doom-challenger-deep doom-miramare doom-rouge)
	       :type list)
  (theme-type 'dark :type symbol
	      :documentation "either `dark' or `light'. nil disable themes")
  (enable-treesitter t :type boolean)
  (enable-menu-bar nil :type boolean)
  (shell nil :type string :documentation "path to shell for `exec-path-from-shell'")

  (csharp-lsp '("csharp-ls" "--log-level debug") :type eglot-server)
  (go-lsp '("gopls" "-remote=auto") :type eglot-server)
  (haskell-lsp '("haskell-language-server-wrapper" "--lsp") :type eglot-server)
  (java-lsp `(,(file-name-concat user-emacs-directory "jdtls-1.45.0" "bin" "jdtls")
	      :initializationOptions (:hints nil))
	    :type eglot-server)
  (js-lsp #'me/choose-js-lsp-server-program :type eglot-server)
  (rust-lsp '("rust-analyzer") :type eglot-server)
  (swift-lsp '("sourcekit-lsp") :type eglot-server)
  (python-lsp '("pylsp") :type eglot-server)
  (zig-lsp '("zls") :type eglot-server)
  (nix-lsp '("nixd") :type eglot-server))

(defun me/macbook-setup ()
  "Setup function to run on macOS."
  (setq frame-resize-pixelwise t
	ns-auto-hide-menu-bar 'nil))

(defvar me/curr-config
  (cond
   ((string= (system-name) "george-nixos")
    (make-me/config
     :theme-type 'light
     ;; :dark-themes '(doom-monokai-pro)
     ;; :light-themes '(doom-flatwhite)
     :font-spec (font-spec :family "IBM Plex Mono"
			   :size 22
			   :weight 'semi-bold)))

   
   ((string= (system-name) "george-thinkpad")
    (make-me/config
     :font-spec (font-spec :size 16)))
   
   ((string= system-type "darwin")
    (make-me/config
     :font-spec (font-spec :family "IBM Plex Mono Medium"
			   :size 18
			   :weight 'medium)
     :enable-menu-bar t
     :theme-type 'light
     ;; :shell "/opt/homebrew/bin/fish"
     :dark-themes '(doom-monokai-pro)
     :light-themes '(modus-operandi-tinted)
     :setup-fn #'me/macbook-setup))

   ((string= (system-name) "SHCS-PC77")
    (make-me/config
     :tmp-dir (file-name-concat user-emacs-directory "tmp")
     :font-spec (font-spec :size 18)
     :theme-type 'light
     :enable-treesitter nil
     :python-lsp '("pyright-langserver" "--stdio")))
   
   (t (make-me/config))))

(defun me/curr-theme-list ()
  "Return the current theme list for `me/curr-config' or nil if disabled."
  (cond ((eq 'light (me/config-theme-type me/curr-config))
	 (me/config-light-themes me/curr-config))
	((eq 'dark (me/config-theme-type me/curr-config))
	 (me/config-dark-themes me/curr-config))
	((null (me/config-theme-type me/curr-config)) nil)
	(t (error "Unknown theme-type `%s' for me/curr-config"
		  (me/config-theme-type me/curr-config)))))

(when (me/config-setup-fn me/curr-config)
  (funcall (me/config-setup-fn me/curr-config)))

;; Keybinds
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(defvar me/keybinds-mode-map (make-sparse-keymap))
(dolist (keybind
	 `(("C-c o x" . scratch-buffer)
	   ("C-c C-d" . duplicate-line)
	   ("C-c l" . me/toggle-dark-themes)
	   ("C-c t" . me/create-tmp-file)
	   ("C-M-h" . backward-kill-word)))
	    
  (keymap-set me/keybinds-mode-map (car keybind) (cdr keybind)))

(define-minor-mode me/keybinds-mode
  "Toggle my personal keybindings."
  :global t
  :lighter " keys"
  :keymap me/keybinds-mode-map
  :group 'me)

(me/keybinds-mode 1)

(defun me/keybinds-mode-most-precedent ()
  "Shadow `minor-mode-map-alist' to put me keybinds at the top."
  (add-to-list 'minor-mode-map-alist '(me/keybinds-mode . me/keybinds-mode-map)))

;; run this hook after everything else
(add-hook 'after-change-major-mode-hook #'me/keybinds-mode-most-precedent 99)


;;; Behaviour
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

;;; Ui
;; set fontN for this and all other frames
(defun me/set-frame-font ()
  "Set the frame font based on `me/curr-config'."
  (interactive)
  (set-frame-font (me/config-font-spec me/curr-config) t t t))
(me/set-frame-font)

(setq inhibit-startup-screen t
      visible-bell t
      column-number-mode t
      truncate-lines t)

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
	  display-fill-column-indicator-column  (me/config-ruler-col me/curr-config)
	  display-fill-column-indicator-character
	  (me/config-ruler-char me/curr-config)) ;; alternative character is 124 instead of 9474
    (display-fill-column-indicator-mode))

(add-hook 'prog-mode-hook #'show-line-ruler)

(require 'delsel)
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

;; disable the complicated funcions disabler - I'm a big boy now
(setq disabled-command-function nil)

;;; Bootstrap Straight
(defvar straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if (fboundp 'straight-use-package)
    (straight-use-package 'use-package)
  (error "Could not run straight-use-package"))

(with-eval-after-load 'use-package
  (when init-file-debug
    (defvar use-package-verbose t)
    (defvar use-package-expand-minimally nil)
    (defvar use-package-compute-statistics t)
    (defvar use-package-always-defer t)
    (setq debug-on-error t)))

;;; Packages
;; A few more useful configurations...
(use-package emacs
  :straight nil ;; nil doesnt install the package, only configures it
  :bind-keymap ("C-\\" . project-prefix-map)
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (tab-always-indent nil)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers
  (enable-recursive-minibuffers t)
  
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  (global-subword-mode 1)
  (ns-control-modifier 'control)
  (ns-alternate-modifier 'meta)
  (ns-function-modifier 'none)
  (ns-command-modifier 'meta)
  :config
  (setq password-cache-expiry 3600)) ;; timeout of 1hr
  
;; (use-package exec-path-from-shell
;;   :if (string= system-type "darwin") ;; only needed on macOS
;;   :functions (exec-path-from-shell-initialize)
;;   :defines (exec-path-from-shell-shell-name)
;;   :init
;;   (when-let* ((s (me/config-shell me/curr-config)))
;;     (setq exec-path-from-shell-shell-name s))
;;   ;; todo, might need to expand PATH var in .zprofile
;;   :config (exec-path-from-shell-initialize))

(use-package which-key
  :config
  (which-key-mode))

(use-package compile
  :straight nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package flymake
  :defines (flymake-mode-map)
  :straight nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)))

(use-package avy
  :functions (avy-setup-default
	      avy-goto-char
	      avy-goto-line)
  :bind (("C-;" . avy-goto-char)
	 ("C-'" . avy-goto-char-2))
  :config (avy-setup-default))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :functions (vertico-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package consult
  :defines (xref-show-xrefs-function xref-show-definitions-function
	    consult-theme consult-ripgrep consult-git-grep consult-grep
	    consult-man consult-bookmark consult-recent-file consult-xref
	    consult-source-bookmark consult-source-file-register
	    consult-source-recent-file consult-source-project-recent-file
	    consult-narrow-map consult-narrow-key)
  :functions (consult-narrow-help consult-register-window consult-xref
  consult-completion-in-region consult-customize)
  :after vertico
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; "C-+"
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help))

(use-package hl-todo
  :functions (global-hl-todo-mode)
  :config
  (global-hl-todo-mode 1))

(use-package magit
  :defines magit-define-global-key-bindings
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package doom-themes
  :defines (doom-themes-enable-bold
	    doom-themes-enable-italic)
  :functions (doom-themes-visual-bell-config
	      doom-themes-org-config
	      me/pick-random-theme
	      me/change-theme-on-project-advice)
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (defun me/pick-random-theme ()
    "Load a random theme from `me/curr-config' depending on `theme-type'."
    (interactive)

    (let* ((themes (me/curr-theme-list))
	   (len (length themes))
	   (curr (car custom-enabled-themes))
	   (type (symbol-name (me/config-theme-type me/curr-config))))
      (cond
       ((eq len 0) (message "No %s themes selected" type))
       ((eq len 1)
	(message "Only 1 %s theme selected" type)
	(load-theme (car themes) t)
	(disable-theme curr))
       ;; make sure to not set the theme to the same thing
       (t (let ((theme (me/pick-rand themes)))
	    (while (eq curr theme)
	      (setq theme (me/pick-rand themes)))
	    (load-theme theme t)
	    (sleep-for 0.1) ;; sleep for a small amount of time to stop flickering when changing theme
	    (disable-theme curr)
	    (when (called-interactively-p 'interactive)
	      (message "Loaded %s theme" theme)))))))

  (defun me/toggle-dark-themes ()
  "Toggle dark themes."
  (interactive)
  (if (eq (me/config-theme-type me/curr-config) 'light)
      (progn
	(message "Toggling dark themes")
	(setf (cl-struct-slot-value 'me/config 'theme-type me/curr-config) 'dark))
    (message "Toggling light themes")
    (setf (cl-struct-slot-value 'me/config 'theme-type me/curr-config) 'light))
  (me/pick-random-theme))

  (when (me/config-theme-type me/curr-config)
    (me/pick-random-theme)))

(use-package yasnippet
  :functions yas-global-mode
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package pyvenv)

(use-package tramp
  :functions (tramp-enable-method)
  :defines (tramp-toolbox-program
	    tramp-remote-path
	    tramp-own-path
	    tramp-connection-properties)
  :init
  (setq tramp-toolbox-program "flatpak-spawn --host toolbox")
  :config
  (tramp-enable-method "toolbox")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
	       (list (regexp-quote "/ssh::")
		     "session-timeout" 300)))

(use-package cape
  :defines (cape-prefix-map)
  :functions (cape-dabbrev
	      cape-file
	      cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; Langauge/IDE setup
(use-package reformatter)
(use-package zig-mode
  :after reformatter)

(use-package haskell-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package envrc
  :if (string-equal (system-name) "george-nixos")
  :hook (after-init . envrc-global-mode))

(use-package lsp-mode
  :defines (lsp-keymap-prefix)
  :functions (lsp-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (("C-c l l" . lsp))
  :hook ((lsp-mode . lsp-which-key-integration))
  :commands lsp)

(use-package eglot
  :straight nil
  :init
  (require 'compile)
  :bind  (("C-c e e" . eglot)
	  ("C-c e r" . eglot-rename)
	  ("C-c e a" . eglot-code-actions))
  
  :config
  (setq
   eglot-server-programs
   `(((csharp-mode csharp-ts-mode) . ,(me/config-csharp-lsp me/curr-config))
     ((go-mode go-ts-mode) . ,(me/config-go-lsp me/curr-config))
     ((haskell-mode haskell-ts-mode) . ,(me/config-haskell-lsp me/curr-config))
     ((java-mode java-ts-mode) . ,(me/config-java-lsp me/curr-config))
     ((js-mode js-jsx-mode js-json-mode json-ts-mode typescript-mode tsx-ts-mode) .
      ,(me/config-js-lsp me/curr-config))
     ((rust-ts-mode rust-mode) . ,(me/config-rust-lsp me/curr-config))
     ((swift-mode swift-ts-mode) . ,(me/config-swift-lsp me/curr-config))
     ((python-mode python-ts-mode) . ,(me/config-python-lsp me/curr-config))
     (zig-mode . ,(me/config-zig-lsp me/curr-config))
     (nix-mode . ,(me/config-nix-lsp me/curr-config))))

  (defun me/format-on-save-when-eglot-managed ()
    "To be called from `before-save-hook'."
    (when (eglot-managed-p) (eglot-format)))

  (defun me/eglot-setup ()
    (add-hook 'before-save-hook #'me/format-on-save-when-eglot-managed nil t)
    (eglot-inlay-hints-mode -1)
    (electric-layout-mode -1))

  :hook (eglot-managed-mode . me/eglot-setup))

(use-package eldoc
  :straight nil
  :bind ("C-c e d" . eldoc-doc-buffer)
  :config
  (setq eldoc-echo-area-use-multiline-p 15))

(use-package electric-pair-mode
  :straight nil
  :hook (prog-mode . electric-pair-mode)) ;; TODO right way round?

(use-package treesit
  :straight nil
  :if (me/config-enable-treesitter me/curr-config)
  :init
  (setq treesit-language-source-alist
	'((c . ("https://github.com/tree-sitter/tree-sitter-c"))
	  (go . ("https://github.com/tree-sitter/tree-sitter-go")) ;; TODO go-mod-ts-mode
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	  (css . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")) ;; TODO could this be file name concat?
	  (bash . ("https://github.com/tree-sitter/tree-sitter-bash")) ;; TODO remap or automode?
	  (html . ("https://github.com/tree-sitter/tree-sitter-html"))
	  (java . ("https://github.com/tree-sitter/tree-sitter-java"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
	  (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
	  (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))))
	  
  (mapc (lambda (lang-src)
	  (let ((lang (car lang-src)))
	    (unless (treesit-language-available-p lang)
	      (message "Installing treesitter grammar %s" (symbol-name lang))
	      (treesit-install-language-grammar lang))))
	treesit-language-source-alist)

  (let ((remaps '((c-mode . c-ts-mode)
		  (javascript-mode . js-ts-mode)
		  (c++-mode . c++-ts-mode)
		  (css-mode . css-ts-mode)
		  (mhtml-mode . html-ts-mode)
		  (java-mode . java-ts-mode)
		  (conf-toml-mode . toml-ts-mode)
		  (python-mode . python-ts-mode))))

    (mapc (lambda (remap) (add-to-list 'major-mode-remap-alist remap))
	  remaps))

    (let ((auto-modes '(("\\.go\\'" . go-ts-mode)
			("go\\.mod\\'" . go-ts-mode)
			("\\.go\\'" . go-ts-mode)
			("\\.tsx?\\'" . typescript-ts-mode)
			("\\.rs\\'" . rust-ts-mode)
			("\\.ya?ml\\'" . yaml-ts-mode)
			("\\.jsonc?\\'" . json-ts-mode)
			("Dockerfile" . dockerfile-ts-mode)
			("Containerfile" . dockerfile-ts-mode))))

      (mapc (lambda (auto) (add-to-list 'auto-mode-alist auto))
	    auto-modes)))


;; Org-mode setup
(require 'org)
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t) (js . t) (python . t) (plantuml . t)))

  (setq org-src-preserve-indentation t)
  ;; this will re-render images in any org file after a code block is executed
  ;; only affects buffers when #+STARTUP: inlineimages is set
  (add-hook 'org-babel-after-execute-hook
            (lambda () (when org-inline-image-overlays (org-redisplay-inline-images)))))

(require 'ob-plantuml)
(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-exec-mode 'plantuml
	org-plantuml-args '("-headless" "-utxt")))

(provide 'init)
;;; init.el ends here
	
