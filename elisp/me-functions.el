;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;; Custom function definitions

;;; Code:
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

(defmacro me/lang-setup (NAME HOOKS &rest BODY)
  "Create a setup func with the `NAME' and `BODY'.  Add it to each hook in `HOOKS'."
  `(progn
     (defun ,(intern (concat "me/" NAME "-setup")) () ,@BODY)
     (dolist (h ,HOOKS) (add-hook h ',(intern (concat "me/" NAME "-setup"))))))

(require 'project)
(defun me/project-revert-all-file-buffers ()
  "Revert any file buffers in the current project, discarding unsaved work."
  (interactive)
  (unless (project-current) (error "No current project"))
  (save-excursion
    (dolist (buf (project-buffers (project-current)))
      (let ((filename (buffer-file-name buf)))
	(when filename
	  (set-buffer buf)
	  (revert-buffer t t)
	  (message "Reverted buffer %s" filename))))))

(provide 'me-functions)
;;; me-functions.el ends here
