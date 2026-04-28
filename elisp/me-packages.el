;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; init.el package config

;;; Code:
;; Package Setup
;; setup straight.el
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


(use-package flymake
  :straight t
  :defines flymake-mode-map
  :functions (flymake-mode
	      flymake-goto-next-error
	      flymake-goto-prev-error)
  :hook (flymake-mode . emacs-lisp-mode-hook)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)))

;; commented out blocks not needed due to evil
(use-package god-mode
  :straight t
  :defines (god-exempt-major-modes
	    god-exempt-predicates
	    god-local-mode
	    god-local-mode-map
	    god-mode-isearch-map)
  :functions (god-local-mode
  	      god-local-mode-pause
	      god-local-mode-resume
	      god-mode-isearch-activate
	      god-mode-isearch-disable
	      me/god-mode-q-passthrough)
  :init
  (require 'god-mode) ;; this is required i think
  (require 'god-mode-isearch) ;; this is required i think
  
  ;; (defun me/god-mode-update-cursor-type ()
  ;;   (setq cursor-type
  ;; 	  (cond
  ;; 	   (god-local-mode 'box)
  ;; 	   (buffer-read-only 'hollow)
  ;; 	   (t 'bar))))
  
  ;; (defun me/god-mode-toggle-on-overwrite ()
  ;;   "toggle god-mode on overwrite-mode."
  ;;   (if (bound-and-true-p overwrite-mode)
  ;; 	(god-local-mode-pause)
  ;;     (god-local-mode-resume)))

  ;; (defun me/god-mode-q-passthrough ()
  ;;   "sends q as a char rather than c-q."
  ;;   (interactive)
  ;;   (self-insert-command 1 ?q))

  :bind (;;("<escape>" . god-mode-all)
	 :map god-local-mode-map
	 ("." . repeat)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window)
	 ("[" . backward-paragraph)
	 ("]" . forward-paragraph)
	 ("M-o" . other-window)
	 ("z" . ignore)
	 ("C-q" . me/god-mode-q-passthrough)
	 :map isearch-mode-map
	 ("<escape>" . god-mode-isearch-activate)
	 :map god-mode-isearch-map
	 ("<escape>" . god-mode-isearch-disable)
	 :map compilation-mode-map
	 ("M-g" . recompile)))
  
  ;; :hook ((post-command-hook . me/god-mode-update-cursor-type)
  ;; 	 (overwrite-mode-hook . me/god-mode-toggle-on-overwrite))

  ;; :config
  ;; (setq god-exempt-major-modes nil
  ;; 	god-exempt-predicates nil))

(use-package evil
  :straight t
  :preface
  ;; evil-want... should be set before evil is loaded
  (setq evil-want-C-u-scroll t
	evil-move-beyond-eol nil
	evil-visual-state-cursor 'hollow
	evil-motion-state-cursor '(hbar . 8))
  ;; TODO evil-disable-insert-state-bindings?
  :init
  (evil-mode 1)
  :config
  (evil-set-leader '(normal visual motion) (kbd ","))

  (evil-set-initial-state 'special-mode 'motion)
  (evil-set-initial-state 'magit-status-mode 'motion)

  (evil-set-undo-system 'undo-redo)
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>") 'god-execute-with-current-bindings
    (kbd ".") project-prefix-map ;; was evil-repeat but barely used
    (kbd "z z") 'recenter-top-bottom
    (kbd "] p") 'forward-page
    (kbd "[ p") 'backward-page)
  

  (evil-define-key 'normal 'global
    (kbd "<tab>") 'indent-for-tab-command
    (kbd "M-.") 'xref-find-definitions)

  ;; don't need to rebind these just use '\' (evil-execute-in-emacs)
  ;; but g in compilation mode does not unset emacs mode back into motion mode
  (evil-define-key 'motion compilation-mode-map
    (kbd "g") 'recompile)

  (evil-define-key 'normal flymake-mode-map
    (kbd "M-n") 'flymake-goto-next-error
    (kbd "M-p") 'flymake-goto-prev-error)

  (evil-define-key 'motion help-mode-map
    (kbd "<tab>") 'forward-button))


(use-package which-key
  :straight t
  :config (which-key-mode))


;; unsure whether avy is actually a part of emacs now or not
;; i think so, but 1 more use-package can't hurt
(use-package avy
  :straight t
  :functions (avy-setup-default
	      avy-goto-char
	      avy-goto-line)
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line))
  :config (avy-setup-default))

(use-package hl-todo
  :straight t
  :defines hl-todo-mode-map
  :functions (global-hl-todo-mode
	      hl-todo-next
	      hl-todo-previous)
  :bind (:map hl-todo-mode-map
	      ("C-c C-n" . hl-todo-next)
	      ("C-c C-p" . hl-todo-previous))
  :init
  (global-hl-todo-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq completions-sort 'orderless))

(use-package vertico
  :straight t
  :defines crm-separator
  :functions (vertico-mode
	      crm-indicator)
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
  :straight t
  :functions (consult-register-window
	      consult-xref
	      consult-completion-in-region)
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
  :straight t
  :defines magit-define-global-key-bindings
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package markdown-mode
  :straight t
  :defines markdown-command
  :mode ("README\\.md\\'" . 'gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package doom-themes
  :straight t
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

  (when (me/config-enable-themes me/curr-config)
    (me/pick-random-theme)

    (defun me/change-theme-on-project-advice (orig-fun &rest args)
      (me/pick-random-theme)
      (apply orig-fun args))
    
    (advice-add 'project-switch-project
		:around #'me/change-theme-on-project-advice)))

(use-package yasnippet
  :straight t
  :functions yas-global-mode
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

(use-package pyvenv
  :straight t)

;; required by zig mode
(use-package reformatter
  :straight t)

(use-package zig-mode
  :straight t)

(use-package haskell-mode
  :straight t)

;; required by swift-mode
(use-package editorconfig
    :straight t
    :config
    :hook (editorconfig-mode . swift-mode))

(use-package swift-mode
  :defines (swift-mode:basic-offset)
  :straight t
  :mode "\\.swift\\'"
  :interpreter "swift"
  :config
  (setq swift-mode:basic-offset 8))

(use-package gptel
  :straight t
  :defines (gptel-mode-map
	    gptel-api-key)
  :bind (("C-c o g" . gptel)
	 :map gptel-mode-map
	 ("C-c C-c" . gptel-send))
  :config
  (let ((token (me/read-file-as-str (file-name-concat user-emacs-directory "gpt_token"))))
    (unless (string-empty-p token)
      (setq gptel-api-key token))))

(use-package meson-mode
  :straight t)

(use-package plantuml-mode
  :straight t)

;; emacs' version of tramp is a little older than the upstream, seems
;; to be a problem with toolbox integration
(use-package tramp
  :functions (tramp-enable-method)
  :defines (tramp-toolbox-program
	    tramp-remote-path
	    tramp-own-path)
  :straight t
  :init
  (setq tramp-toolbox-program "flatpak-spawn --host toolbox")
  :config
  (tramp-enable-method "toolbox")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'me-packages)
;;; me-packages.el ends here
