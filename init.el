;;; package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;; My Emacs configuration

;;; Code:
;; GC and Buffer Sizes
;; Increase the size of buffers and garbage collection threshold - this isn't the 1900's anymore.
(setq gc-cons-threshold (* 100 1024 1024) ;100mb (as rec. by lsp-mode)
      read-process-output-max (* 1024 1024)) ; 1mb (/proc/sys/fs/pipe-max-size)

;; Customise file
(let ((customise-file (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file customise-file)
  (load customise-file t)) ;; create file if no exist

;; Backup file
;; Put any backup files in the .conf folder instead of in the working dir
;; place file backups in conf emacs instead of littered around the place
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t)

;; Lock-file transforms
;; Put lock files in tmp instead of littered around the place.
;; Extracts the filename from the path and appends it to /tmp, uniq-ifying if needed.
;; Take into consideration permissions of where the file is stored, as lock files
;; are supposed to be able to be read by anyone
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/tmp/\\1" t)))

;; Auto-save file transforms
;; Put remote autosave in /tmp and put regular autosave in conf emacs.
;; The ordering of this alist is important. The catch all should be at the end
(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
	(".*" "~/.config/emacs/auto-saves/" t)))

;; registers
(set-register ?c `(file . ,user-init-file))
(set-register ?b '(file . "~/.bashrc"))
(set-register ?p '(file . "~/.profile"))

;; System specific config
(require 'cl-lib)

(cl-defstruct me/system-config
  "A collection of variables that could change based on the computer/system I'm on."
  (ruler-col 80 :type number)
  (font-family nil :type string)
  (font-size 140 :type number))

(defvar me/desktop-config
  (make-me/system-config :font-size 180))

(defvar me/laptop-config
  (make-me/system-config))

(defvar me/windows-config
  (make-me/system-config))

(defvar me/current-config
  (cond
   ;; TODO other systems
   ((string= (system-name) "george-gentoo") me/desktop-config)))

;; General functions
(defun me/alist-add-many (alist to-add)
  "Add all items in TO-ADD into ALIST."
  (dolist (element to-add)
    (add-to-list alist element)))

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
  "Create a functoin that rings the bell, print why with `MSG'."
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
  "Show a line rule at column set by the current system config."
  (setq display-fill-column-indicator t
	display-fill-column-indicator-column (me/system-config-ruler-col me/current-config)
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
(set-face-attribute 'default nil :height (me/system-config-font-size me/current-config)) ;; TODO font family

;; disable the complicated funcions disabler - I'm a big boy now
(setq disabled-command-function nil)

;; Package Setup
;; Add repos and ensure use-package is installed
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; install use-pacakge if not already present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(require 'flymake)
(with-eval-after-load 'flymake
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
  (keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p"' flymake-goto-prev-error))

(use-package god-mode
  :defines (god-exempt-major-modes
	    god-exempt-predicates
	    god-local-mode
	    god-local-mode-map
	    god-mode-isearch-map)
  :functions (god-local-mode
  	      god-local-mode-pause
	      god-local-mode-resume
	      god-mode-isearch-activate
	      god-mode-isearch-disable)
  :ensure t
  :init
  (require 'god-mode) ;; this is required i think
  (require 'god-mode-isearch) ;; this is required i think
  
  (defun me/god-mode-update-cursor-type ()

    (setq cursor-type (if (or god-local-mode buffer-read-only) '(hbar . 13) 'box)))

  ;; TODO can a change modeline colour (and face value)

  (defun me/god-mode-toggle-on-overwrite ()
    (require 'god-mode) ;; this might be required
    "Toggle god-mode on overwrite-mode."
    (if (bound-and-true-p overwrite-mode)
	(god-local-mode-pause)
      (god-local-mode-resume)))

  :bind (("<escape>" . god-mode-all)
	 :map god-local-mode-map
	 ("." . repeat)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window)
	 ("[" . backward-paragraph)
	 ("]" . forward-paragraph)
	 :map isearch-mode-map
	 ("<escape>" . god-mode-isearch-activate)
	 :map god-mode-isearch-map
	 ("<escape>" . god-mode-isearch-disable))
  
  :hook ((post-command-hook . me/god-mode-update-cursor-type)
	 (overwrite-mode-hook . me/god-mode-toggle-on-overwrite)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package hl-todo
  :defines hl-todo-mode-map
  :functions (global-hl-todo-mode
	    hl-todo-next
	    hl-todo-previous)
  :ensure t
  :bind (:map hl-todo-mode-map
	      ("C-c C-n" . hl-todo-next)
	      ("C-c C-p" . hl-todo-previous))
  :init
  (global-hl-todo-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq completions-sort 'orderless))

(use-package vertico
  :defines crm-separator
  :functions (vertico-mode
	      crm-indicator)
  :ensure t
  :hook (minibuffer-setup . cursor-intangible-mode)
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

(require 'xref)
(use-package consult
  :functions (consult-register-window
	      consult-xref
	      consult-completion-in-region)
  :ensure t
  :after vertico
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package magit
  :defines magit-define-global-key-bindings
  :ensure t
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package markdown-mode
  :defines markdown-command
  :ensure t
  :mode ("README\\.md\\'" . 'gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package doom-themes
  :defines (doom-themes-enable-bold
	    doom-themes-enable-italic)
  :functions (doom-themes-visual-bell-config
	      doom-themes-org-config
	      me/pick-random-theme
	      me/change-theme-on-project-advice)
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (defvar nice-themes '(doom-xcode
		      doom-badger
		      doom-challenger-deep
		      doom-miramare
		      doom-rouge))

  (defun me/pick-random-theme ()
    "Loads a random theme from `nice-themes'"
    (interactive)
    (let ((curr (car custom-enabled-themes))
	  (theme (me/pick-rand nice-themes)))
      ;; make sure to not set the theme to the same thing
      (while (eq curr theme)
	(setq theme (me/pick-rand nice-themes)))
      (load-theme theme)
      (sleep-for 0.1) ;; sleep for a small amount of time to stop flickering when changing theme
      (disable-theme curr)
      (when (called-interactively-p 'interactive)
	(message "Loaded %s theme" theme))))

  (me/pick-random-theme)

  (defun me/change-theme-on-project-advice (orig-fun &rest args)
    (me/pick-random-theme)
    (apply orig-fun args))
  
  (advice-add 'project-switch-project :around #'me/change-theme-on-project-advice))

(use-package yasnippet
  :functions yas-global-mode
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package pyvenv
  :ensure t)

;; required by zig mode
(use-package reformatter
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package gptel
  :defines (gptel-mode-map
	    gptel-api-key)
  :ensure t
  :bind (("C-c C-g" . gptel)
	 :map gptel-mode-map
	      ("C-c C-c" . gptel-send))
  :config
  (let ((token (me/read-file-as-str (concat user-emacs-directory "gpt_token"))))
    (unless (string-empty-p token)
      (setq gptel-api-key token))))

(use-package meson-mode
    :ensure t)

(use-package plantuml-mode
  :ensure t)

;; Language stuff
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(require 'eglot)
(with-eval-after-load 'eglot
  (defun me/eglot-setup ()
    "Eglot setup."
    (add-hook 'before-save-hook 'eglot-format)
    (eglot-inlay-hints-mode -1))
  
  (add-hook 'eglot-managed-mode-hook 'me/eglot-setup)
  
  (me/alist-add-many 'eglot-server-programs
		     `((rust-ts-mode . ("rust-analyzer"))
		       (go-ts-mode . ("gopls" "-remote=auto"))
		       (python-ts-mode . ("pyright-langserver" "--stdio"))
		       ;; https://download.eclipse.org/jdtls/milestones/
		       (java-ts-mode . (,(concat user-emacs-directory "jdtls-1.45.0/bin/jdtls")
					:initializationOptions (:hints nil)))
		       (haskell-mode . ("haskell-language-server-wrapper" "--lsp"))
		       (typescript-mode . ("deno" "lsp"
					   :initializationOptions (:enable t)))))

  (keymap-set eglot-mode-map "C-c e a" 'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c e r" 'eglot-rename)

  ;; redefine eglot-rename as it bugs me that it doesn't prefill the
  ;; symbol source. taken directly from eglot.el, just changed the
  ;; INITIAL-CONTENTS arg in read-from-minibufer to include the symbol
  (defun eglot-rename (newname)
    "Rename the current symbol to NEWNAME."
    (interactive
     (list (read-from-minibuffer
            (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                           "unknown symbol"))
	    (symbol-name (symbol-at-point)) ;; was nil
            nil nil nil
            (symbol-name (symbol-at-point)))))
    (eglot-server-capable-or-lose :renameProvider)
    (eglot--apply-workspace-edit
     (eglot--request (eglot--current-server-or-lose)
                     :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                            :newName ,newname))
     this-command)))

;; limit eldoc to max 10 lines
(setq eldoc-echo-area-use-multiline-p 10)

(require 'elec-pair)
(with-eval-after-load 'elec-pair
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (eval-and-compile
    (defun me/inhibit-electric-pair-mode-p (char)
      "A predicate based on `CHAR' for when `electric-pair-mode' should be inhibited."
      (or (minibufferp) (electric-pair-default-inhibit char))))

  (setq-default electric-pair-inhibit-predicate #'me/inhibit-electric-pair-mode-p))

;; (defmacro me/lang-setup (name hooks &rest body)
(defmacro me/lang-setup (NAME HOOKS &rest BODY)
  "Create a setup func with the `NAME' and `BODY'.  Add it to each hook in `HOOKS'."
  `(progn
     (defun ,(intern (concat "me/" NAME "-setup")) () ,@BODY)
     (dolist (h ,HOOKS) (add-hook h ',(intern (concat "me/" NAME "-setup"))))))

(me/lang-setup "ts-js"
	       '(typescript-ts-mode-hook js-mode-hook)
	       (setq tab-width 4))

(require 'go-ts-mode)
(me/lang-setup "go"
	       '(go-ts-mode-hook go-mod-ts-mode-hook)
	       (setq tab-width 4
		     go-ts-mode-indent-offset 4))

(require 'cc-vars)
(me/lang-setup "java"
	       '(java-mode-hook)
	       (indent-tabs-mode nil)
	       (setq tab-width 4)
	       (setq c-basic-offset 4))

(me/lang-setup "conf"
	       '(conf-mode-hook)
	       (setq tab-width 4)  ;; default tab-width of 8 is a bit intense
	       (setq c-basic-offset 4))


(setq treesit-language-source-alist
  '((css "https://github.com/tree-sitter/tree-sitter-css")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (java "https://github.com/tree-sitter/tree-sitter-java")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))


(defun me/install-all-treesitter-grammars ()
  "Install all treesitter grammars listed in `treesit-language-source-alist'."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (message "Installed all treesitter grammars"))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
	(css-mode . css-ts-mode)
	(js2-mode . js-ts-mode)
	(js-json-mode . json-ts-mode)
	(python-mode . python-ts-mode)
	(java-mode . java-ts-mode)))

;; add colours to compilation out
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Org-mode setup
(require 'org)
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
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


;; Keybinds
(defvar me/keybinds-mode-map (make-sparse-keymap))
(dolist (keybind
	 `(("C-c o x" . scratch-buffer)
	   ("C-c o r" . me/reload-file)
	   ("C-c C-d" . duplicate-line)
	   ("C-c s" . just-one-space)
	   ("C-c z" . zap-up-to-char)
	   ("C-x [" . ,(me/leave-msg "C-x [ is disabled"))
	   ("C-x C-p" . ,(me/leave-msg "C-x C-p is disabled"))
	   ("C-c l" . me/count-lines-reigon)))
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

(provide 'init)
;;; init.el ends here
