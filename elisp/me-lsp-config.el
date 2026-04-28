;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; init.el language and lsp config

;;; Code:
(me/alist-add-many 'auto-mode-alist
		   '(("\\.tsx?\\'" . typescript-ts-mode)
		     ("\\.ya?ml\\'" . yaml-ts-mode)))

(require 'eglot)
(with-eval-after-load 'eglot
  (defun me/eglot-setup ()
    "Eglot setup."
    (add-hook 'before-save-hook 'eglot-format)
    (delete-selection-mode 1)
    (eglot-inlay-hints-mode -1))
  
  (add-hook 'eglot-managed-mode-hook 'me/eglot-setup)
  
  (me/alist-add-many 'eglot-server-programs
		     `((rust-ts-mode . ("rust-analyzer"))
		       (go-ts-mode . ("gopls" "-remote=auto"))
		       (python-ts-mode . ,(me/config-python-lsp-server me/curr-config))
		       ;; https://download.eclipse.org/jdtls/milestones/
		       (java-ts-mode . (,(file-name-concat user-emacs-directory "jdtls-1.45.0" "bin" "jdtls")
					:initializationOptions (:hints nil)))
		       (haskell-mode . ("haskell-language-server-wrapper" "--lsp"))
		       ;; (csharp-mode . ("/opt/omnisharp-roslyn/run" "-v" "-lsp"))
		       (csharp-mode . ("csharp-ls" "--log-level debug"))
		       ((typescript-mode tsx-ts-mode js-mode js-jsx-mode js-json-mode json-ts-mode) .
			("deno" "lsp" :initializationOptions (:enable t)))
		       (swift-mode . ("sourcekit-lsp"))))

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


(me/lang-setup "ts-js"
	       '(typescript-ts-mode-hook js-mode-hook js-ts-mode-hook)
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
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    (docker "https://github.com/camdencheek/tree-sitter-dockerfile")))


(defun me/install-all-treesitter-grammars ()
  "Install all treesitter grammars listed in `treesit-language-source-alist'."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (message "Installed all treesitter grammars"))

(when (me/config-enable-treesitter me/curr-config)
  (setq major-mode-remap-alist
	'((bash-mode . bash-ts-mode)
	  (css-mode . css-ts-mode)
	  (js2-mode . js-ts-mode)
	  (js-json-mode . json-ts-mode)
	  (python-mode . python-ts-mode)
	  (java-mode . java-ts-mode)
	  (go-mode . go-ts-mode)
	  (rust-mode . rust-ts-mode))))

;; add colours to compilation out
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(provide 'me-lsp-config)
;;; me-lsp-config.el ends here
