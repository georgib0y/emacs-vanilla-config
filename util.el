;;; package --- Summary
;;; Commentary:
;; A collection of misc functions to be included in init.el

;;; Code:
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


(defun me/choose-js-lsp-server-program (_int project)
  "Determine whether `PROJECT' is a deno project and return either deno lsp or tls config."
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
	(list "typescript-language-server" "--stdio")))))


(provide 'util)
;;; util.el ends here
