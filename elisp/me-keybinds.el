;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; init.el keybind config

;;; Code:
(require 'me-system-specific-config)

(defvar me/keybinds-mode-map (make-sparse-keymap))
(dolist (keybind
	 `(("C-c o x" . scratch-buffer)
	   ("C-c o r" . ,(me/leave-msg "use revert-buffer quick C-x x g (,x xg)"))
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

(define-key project-prefix-map (kbd "C-r") #'me/project-revert-all-file-buffers)

(provide 'me-keybinds)
;;; me-keybinds.el ends here
