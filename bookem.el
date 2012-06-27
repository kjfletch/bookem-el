;;; bookem.el --- Bookmark system organising bookmarks into groups
;;
;; Author: Kevin J. Fletcher <dev@kjfletch.co.uk>
;; Maintainer: Kevin J. Fletcher <dev@kjfletch.co.uk>
;; Keywords: bookem, bookmarks
;; Homepage: http://github.com/kjfletch/bookem-el
;; Version: 0.3
;; 
;;; Commentary:
;;
;; This file provides a bookmark system for emacs called bookem. This
;; allows you to create bookmarks for files and other special buffers
;; and organise them into groups.
;;
;; Bookmark types:
;;  file      - simple bookmark for a line in a file.
;;              also works on dired buffers.
;;  c-defun   - bookmark a function in a c file.
;;  url       - bookmark a url.
;;  woman     - bookmark a position in a man page.
;;  [e]shell  - bookmark a working directory in a shell.
;;  info      - TODO Bookmark a location in an info document.
;;
;;; Changelog:
;;
;;  0.3 - 2012-06-27
;;    Added WoMan bookmark support.
;;    Added Shell bookmark support (shell & eshell).
;;  0.2 - 2012-06-14
;;    Added URL bookmark support.
;;  0.1 - 2012-06-14
;;    First release; features:
;;    - Create bookmarks (files, c-defuns): `bookem-bookmark-buffer'
;;    - Assign bookmarks to [multiple] groups when creating.
;;    - Navigate to bookmarks: `bookem-goto-bookmark'
;;    - List bookmarks: `bookem-list-bookmarks'
;;    - Remove/delete bookmarks from groups (from the bookmark list)
;;
;; Copyright (C) 2010-2012 Kevin J. Fletcher
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code
(defvar bookem-dir "~/.emacs.d/bookem"
  "Define where bookem associated files are stored.")

(defvar bookem-bookmarks-path 
  (concat (file-name-as-directory bookem-dir) "bookem-bookmarks.el")
  "Define where the bookem bookmarks file is stored.")

(defvar bookem-bookmark-list-buffer-name
  "*Bookem Bookmarks*"
  "Define the name of the bookmark list/buffer.")

(defvar bookem-bookmark-types
  '((:name   "File" 
     :type   :file 
     :type-p bookem-type-p-file
     :make   bookem-make-file
     :lookup bookem-lookup-file
     :suggest-name bookem-suggest-name-file)
    (:name "C Function"
     :type :c-defun
     :type-p bookem-type-p-c-defun
     :make bookem-make-c-defun
     :lookup bookem-lookup-c-defun
     :suggest-name bookem-suggest-name-c-defun)
    (:name "URL"
     :type :url
     :type-p bookem-type-p-url
     :make bookem-make-url
     :lookup bookem-lookup-url
     :suggest-name bookem-suggest-name-url)
    (:name "WoMan"
     :type :woman
     :type-p bookem-type-p-woman
     :make bookem-make-woman-bookmark
     :lookup bookem-lookup-woman
     :suggest-name bookem-suggest-name-woman)
    (:name "shell"
     :type :shell
     :type-p bookem-type-p-shell
     :make bookem-make-shell
     :lookup bookem-lookup-shell
     :suggest-name bookem-suggest-name-shell))
  "List of bookem bookmark types.")

(defface bookem-list-heading-face
  '((t (:inherit font-lock-type-face)))
  "Face to use for the heading on the bookem bookmark list."
  :version "22.1")

(defface bookem-list-group-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight a group on the bookem bookmark list."
  :version "22.1")

(defface bookem-list-bookmark-name-face
  '((t (:inherit font-lock-constant-face)))
  "Face to highlight a bookmark name on the bookem bookmark list."
  :version "22.1")

(setq bookem-bookmark-list nil)
(setq bookem-active-group nil)
(setq bookem-current-bookmarks-format 1)

;;
;; Initialisation
;;
(defun bookem-init ()
  "Initialise bookem module and storage space."
  (let (file-contents file-format)
    (unless (file-directory-p bookem-dir)
      (make-directory bookem-dir))
    
    (when (file-readable-p bookem-bookmarks-path)
      (setq file-contents (read (bookem-read-file bookem-bookmarks-path)))
      (setq file-format (plist-get file-contents :format))
      (setq bookem-bookmark-list (plist-get file-contents :bookmarks)))
    
    (if file-format
	(unless (equal bookem-current-bookmarks-format file-format)
	  (error "Unsupported bookmarks file format, please update bookem to the latest version.")))))

;;
;; Bookmark Type System
;;
(defun bookem-lookup-from-type (book-type)
  "Return the defun used to lookup a bookmark of the given type."
  (plist-get (bookem-type-plist-from-type book-type)
	     :lookup))

(defun bookem-type-from-type-name (type-name)
  "Get a type name from a bookmark type."
  (bookem-list-get-plist bookem-bookmark-types :name type-name))

(defun bookem-type-plist-from-type (type)
  "From a type return the type plist."
  (bookem-list-get-plist bookem-bookmark-types :type type))
 
(defun bookem-type-names-for-buffer (&optional buffer)
  "Return a list of bookmark type names supported by this buffer type."
  (let ((buffer     (or buffer (current-buffer)))
	(type-names nil))
    (when buffer
      (mapc (lambda (type)
	      (if (funcall (plist-get type :type-p) buffer)
		  (setq type-names (cons (plist-get type :name) type-names))))
	    bookem-bookmark-types))
    type-names))
  
;;
;; File Bookmark Type
;;
(defun bookem-lookup-file (book-loc)
  "Bookmark lookup function used to search for simple file type bookmarks."
  (let* ((path (plist-get book-loc :path))
	 (line (plist-get book-loc :line))
	 (point (plist-get book-loc :point))
	 (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (save-excursion
	(unless (equal (line-number-at-pos point) line)
	  (goto-line line)
	  (beginning-of-line)
	  (setq point (point)))))
    `(:buffer ,buffer :point ,point)))

(defun bookem-type-p-file (buffer)
  "Returns nil if the current buffer type does not support a file bookmark.
Buffers which support file bookmarks are buffer associated with file."
  (or (buffer-file-name buffer)
      (eq 'dired-mode major-mode)))

(defun bookem-make-file (buffer)
  "Returns a location plist for a new file bookmark of buffer.
The location plist contains file path (:path) and point (:point)."
  (let (loc path)
    (save-excursion
      (with-current-buffer buffer
	(setq path (or (buffer-file-name)
		       default-directory))
	(setq loc (plist-put loc :path path))
	(setq loc (plist-put loc :point (point)))
	(setq loc (plist-put loc :line (line-number-at-pos (point))))))
    loc))

(defun bookem-suggest-name-file (buffer)
  "Return a suggested name for a bookmark of the given buffer."
  (let (filename)
    (with-current-buffer buffer
      (setq filename (or (buffer-file-name) default-directory))
      (setq filename (replace-regexp-in-string "[\\\\/]$" "" filename))
      (setq filename (file-name-nondirectory filename))
      (format "%s:%d" filename (line-number-at-pos (point))))))

;;
;; C Function Bookmark Type
;;
(defun bookem-type-p-c-defun (buffer)
  "Returns t if the given buffer supports c function type bookmarks.
This is the case if the buffer has an associated file, is in c-mode
and point is within a function definition."
  (with-current-buffer buffer
    (and (buffer-file-name buffer)
         (eq major-mode 'c-mode)
	 (c-defun-name))))

(defun bookem-make-c-defun (buffer)
  "Returns new c-defun bookmark type location info from given buffer."
  (with-current-buffer buffer
    `(:path ,(buffer-file-name buffer)
      :line ,(line-number-at-pos (point))
      :point ,(point)
      :defun ,(c-defun-name))))

(defun bookem-lookup-c-defun (loc)
  "From a c-defun location plist locate the associated file and position of the function."
  (let* ((path (plist-get loc :path))
	 (line (plist-get loc :line))
	 (point (plist-get loc :point))
	 (defun-name (plist-get loc :defun))
	 (buffer (find-file-noselect path))
	 (defun-loop-not-found t))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(unless (equal defun-name (c-defun-name))
	  (goto-line line)
	  (setq point (point))
	  (unless (equal defun-name (c-defun-name))
	    (beginning-of-buffer)
	    (while (and defun-loop-not-found (search-forward defun-name))
	      (when (equal defun-name (c-defun-name))
		(setq defun-loop-not-found nil)
		(setq point (point))))))))
    `(:buffer ,buffer :point ,point)))

(defun bookem-suggest-name-c-defun (buffer)
  "Return a suggested name for a bookmark of the given buffer."
  (let (filename)
    (with-current-buffer buffer
      (setq filename (file-name-nondirectory (buffer-file-name)))
      (format "%s:%s()" filename (c-defun-name)))))

;;
;; URL Type
;;
(defun bookem-type-p-url (buffer)
  "Always return t. We always want URL bookmarks to be made."
  t)

(defun bookem-make-url (buffer)
  "Creates the required properties for a url bookmark."
  (with-current-buffer buffer
    `(:url ,(read-from-minibuffer "URL: " (thing-at-point 'url)))))

(defun bookem-lookup-url (loc)
  "How a URL bookmark should be visited."
  (browse-url (plist-get loc :url)))

(defun bookem-suggest-name-url (buffer)
  "Suggests a name for a URL bookmark."
  (with-current-buffer buffer
    (thing-at-point 'url)))

;;
;; Woman Type
;;
(defun bookem-type-p-woman (buffer)
  "Return t if buffer is a woman buffer."
  (eq major-mode 'woman-mode))

(defun bookem-make-woman-bookmark (buffer)
  "Create a woman bookmark from the given buffer."
  (with-current-buffer buffer
    (let ((wo (rassoc (buffer-name) woman-buffer-alist)))
      `(:path ,(car wo)
	:point ,(point)))))

(defun bookem-lookup-woman (loc)
  "Navigate to the woman bookmark."
  (woman-find-file (plist-get loc :path))
  (goto-char (plist-get loc :point)))

(defun bookem-suggest-name-woman (buffer)
  "Suggest a name for a woman buffer bookmark."
  (buffer-name buffer))

;;
;; Shell Type
;;
(defun bookem-type-p-shell (buffer)
  "Return t if buffer is a shell buffer."
  (or (eq major-mode 'eshell-mode)
      (eq major-mode 'shell-mode)))

(defun bookem-make-shell (buffer)
  "Create a shell bookmark."
  `(:path ,default-directory
    :shell-type ,(cond ((eq major-mode 'eshell-mode) 'eshell)
		       ((eq major-mode 'shell-mode) 'shell))))

(defun bookem-lookup-shell (loc)
  "Lookup a shell bookmark."
  (let ((default-directory (plist-get loc :path))
	(type (plist-get loc :shell-type)))
    (cond ((eq type 'shell)
	   ;; fixme: we could see if multi-shell is available and
	   ;; create a new shell that way.
	   (shell (get-buffer-create (concat "bookem shell: " default-directory))))
	  ((eq type 'eshell)
	   ;; fixme: we need to create a new shell for each bookmark.
	   ;; fixme: we could see if multi-eshell is available and
	   ;; create a new shell that way.
	   (eshell)))))

(defun bookem-suggest-name-shell (buffer)
  "Suggest a name for a shell bookmark."
  (concat "shell:" (with-current-buffer buffer default-directory)))

;;
;; General Innards
;;
(defun bookem-list-get-plist (list key value)
  "Given a list of property lists return the entry wich has a matching key with
expected value."
  (let ((found nil))
    (mapc (lambda (x) 
	    (when (equal value (plist-get x key))
	      (setq found x)))
	  list)
    found))

(defun bookem-display-from-bookmark-plist (bookmark)
  "Find and display the bookmark from the given bookmark plist."
  (let* ((book-name (car bookmark))
	 (book-plist (cdr bookmark))
	 (book-type (plist-get book-plist :type))
	 (book-loc  (plist-get book-plist :location))
	 (book-lookup (bookem-lookup-from-type book-type)))
    (when book-lookup
      (bookem-display-from-display-info (funcall book-lookup book-loc)))))

(defun bookem-display-from-display-info (book-display-info)
  "Display the bookmark given the display info plist (buffer, point)."
  (let ((buffer (plist-get book-display-info :buffer))
	(point (plist-get book-display-info :point)))
    (when (and buffer point)
      (switch-to-buffer buffer)
      (goto-char point))))

(defun bookem-read-file (filepath)
  "Read a file and return it's contents as a string."
  (when (file-readable-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (buffer-substring (point-min) (point-max)))))

(defun bookem-write-file (filepath output)
  "Write output string to the given file."
  (with-temp-buffer
    (insert output)
    (when (file-writable-p filepath)
      (write-region (point-min)
		     (point-max)
		     filepath))))

(defun bookem-ass-equal-delete (key alist)
  "Delete all elements in the alist whose assoc key equal given key."
  (let (equal-assoc loop-exit)
    (while (not loop-exit)
      (setq equal-assoc (assoc key alist))
      
      (if equal-assoc
	  (progn
	    (setq alist (assq-delete-all (car equal-assoc) alist)))
	(setq loop-exit t)))
    alist))

(defun bookem-complete-space ()
  "Override for ido-complete-space."
  (interactive)
  (insert " "))

(defun bookem-completing-read (prompt choices &optional require-match initial-input def)
  "Prompt for user input with completion.
Overrides ido keymap to allow us to insert spaces."
  (let ((ido-common-completion-map (and (boundp 'ido-common-completion-map)
					ido-common-completion-map)))
    (if (and (fboundp 'ido-mode)
	     ido-mode)
	(progn
	  (unless require-match
	    (substitute-key-definition 'ido-complete-space 'bookem-complete-space ido-common-completion-map))
	  (ido-completing-read prompt choices nil require-match initial-input nil nil))
      (progn
	(completing-read prompt choices nil require-match initial-input nil nil)))))

(defun bookem-prompt-group-name (&optional require-match allow-empty)
  "Prompt for a group name from the list of all group names."
  (let ((group-names (bookem-list-group-names)))

    (when allow-empty
      (add-to-list 'group-names ""))

    (when (and require-match (not group-names))
      (error "No groups."))

    (setq bookem-active-group
	  (bookem-completing-read "Group: " group-names require-match nil bookem-active-group))
    bookem-active-group))

(defun bookem-prompt-bookmark-name (&optional group require-match initial-input)
  "Prompt for a bookmark name from the list of all bookmarks in the given group"
  (let ((bookmarks-in-group (bookem-list-bookmark-names group)))
    
    (when (and require-match (not bookmarks-in-group))
      (error "No bookmarks for group"))

    (bookem-completing-read "Bookmark Name: " bookmarks-in-group require-match initial-input)))

(defun bookem-prompt-type-name (buffer)
  "Prompt for a bookmark type of the given buffer."
  (let* ((valid-type-names (bookem-type-names-for-buffer buffer)))
    (if valid-type-names
	(bookem-completing-read "Bookmark Type: " valid-type-names t)
      (error "No bookmark types defined for this buffer type."))))

(defun bookem-create-bookmark (bookmark-name bookmark-type make-defun buffer)
  "Return a new bookmark plist."
  (let ((loc (funcall make-defun buffer)))
    `(,bookmark-name . (:type ,bookmark-type :location ,loc))))

(defun bookem-save-bookmarks-to-file ()
  "Save the bookmark structure to disk.
If bookmarks is not nil updates the bookmark groups with new value."
  (let (output-structure)
    (setq output-structure (plist-put output-structure :bookmarks bookem-bookmark-list))
    (setq output-structure (plist-put output-structure :format bookem-current-bookmarks-format))
    (bookem-write-file bookem-bookmarks-path (prin1-to-string output-structure))))

(defun bookem-list-group-names ()
  "List all group names used by all bookmarks."
  (let (group-names groups)
    (mapc (lambda (bookmark)
	    (setq groups (plist-get (cdr bookmark) :groups))
	    (mapc (lambda (group)
		    (add-to-list 'group-names group)) 
		  groups))
	    bookem-bookmark-list)
    group-names))

(defun bookem-list-bookmark-names-in-group (group-name)
  "List all bookmark names which are associated with given group names."
  (let (bookmark-names groups)
    (mapc (lambda (bookmark)
	    (setq groups (plist-get (cdr bookmark) :groups))
	    (if (member group-name groups)
		(add-to-list 'bookmark-names (car bookmark))))
	  bookem-bookmark-list)
    bookmark-names))

(defun bookem-list-bookmark-names (&optional group-name)
  "If GROUP-NAME is nil lists all bookmark names. If GROUP-NAME
is a group name will return a list of all bookmark names
associated with that group name."
  (if group-name
      (bookem-list-bookmark-names-in-group group-name)
    (mapcar 'car bookem-bookmark-list)))

;;
;; Bookem Bookmark List/Buffer
;;
(defvar bookem-bookmark-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'quit-window)
    (define-key map " " 'next-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'bookem-goto-bookmark-at-line)
    (define-key map (kbd "<return>") 'bookem-goto-bookmark-at-line)
    (define-key map "d" 'bookem-mark-for-delete)
    (define-key map "u" 'bookem-unmark)
    (define-key map "U" 'bookem-unmark-all)
    (define-key map "x" 'bookem-delete-marked)
    (define-key map "g" 'bookem-list-bookmarks)
    map))

(defun bookem-bookmark-list-mode ()
  "Major mode for the bookem bookmarks list/buffer.
\\<bookem-bookmark-list-mode-map>
\\[bookem-goto-bookmark-at-line] -- Jump to the location of the bookmark under point."
  (kill-all-local-variables)
  (use-local-map bookem-bookmark-list-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'bookem-bookmark-list-mode)
  (setq mode-name "Bookem Bookmark Menu")
  (run-mode-hooks 'bookem-bookmark-list-mode-hook))

(defun bookem-list-write-bookmark (bookmark-name)
  "Write out the given bookmark in the current buffer."
  (let* (face-start
	(bookmark (assoc bookmark-name bookem-bookmark-list))
	(bookmark-rest (car bookmark))
	(start-line (point)))
    ;;      "_D_>>"
    (insert "     ")
    (setq face-start (point))
    (insert (car bookmark))
    (add-text-properties face-start (point)
			 '(font-lock-face bookem-list-bookmark-name-face))
    (insert " - ")
    (insert (format "%s" (plist-get (cdr bookmark) :location)))
    (put-text-property start-line (point) 'bookem-bookmark-name-property bookmark-name)
    (put-text-property start-line (point) 'bookem-bookmark-property bookmark)
    (insert "\n")))

(defun bookem-list-write-group (group-name)
  "Write out the given group in the current buffer."
  (let ((bookmarks (bookem-list-bookmark-names group-name))
	(start-line (point)))
    ;;               _D__           Padding before group for option flags.
    (insert (concat "   " group-name "\n"))
    (add-text-properties start-line (point)
		       '(font-lock-face bookem-list-group-face))
    (mapc 'bookem-list-write-bookmark bookmarks)
    (put-text-property start-line (point) 'bookem-group-name-property group-name)
    (newline)))

(defun bookem-list-buffer-create ()
  "Create and return the bookmark list buffer. If it exists the buffer 
will be refreshed."
  (let (buffer)
    (setq buffer (get-buffer-create bookem-bookmark-list-buffer-name))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "Bookem Bookmark List\n")
	(insert "--------------------\n")
	(save-excursion
	  (add-text-properties (point-min) (point)
			       '(font-lock-face bookem-list-heading-face))
	  (mapc 'bookem-list-write-group (bookem-list-group-names))
	  (bookem-bookmark-list-mode))))
    buffer))

(defun bookem-goto-bookmark-at-line ()
  "If there is a bookmark at the current line in the bookmark
list buffer display it."
  (interactive)
  (let (property)
  (with-current-buffer (get-buffer-create bookem-bookmark-list-buffer-name)
    (setq property (get-text-property (point) 'bookem-bookmark-property))
    (if property
	(bookem-display-from-bookmark-plist property)))))
    
(defun bookem-mark-for-delete ()
  "Marks the current line in the bookem bookmark list buffer for
  deletion if the line supports it."
  (interactive)
  (let (buffer-read-only)
    (with-current-buffer (get-buffer-create bookem-bookmark-list-buffer-name)
      (when (bookem-set-line-props-if 'bookem-bookmark-property
				      '(bookem-marked-for-delete t))
	(save-excursion
	  (beginning-of-line)
	  (forward-char)
	  (delete-char 1)
	  (insert "D"))))))

(defun bookem-unmark ()
  "Unmark the current line from any previously marked operation
  if the line supports it."
  (interactive)
  (let (buffer-read-only)
    (with-current-buffer (get-buffer-create bookem-bookmark-list-buffer-name)
      (when (bookem-set-line-props-if 'bookem-bookmark-property
				      '(bookem-marked-for-delete nil))
	(save-excursion
	  (beginning-of-line)
	  (forward-char 1)
	  (delete-char 2)
	  (insert "  "))))))

(defun bookem-unmark-all ()
  "Unmark all lines which currently have marks."
  (interactive)
  (with-current-buffer (get-buffer-create bookem-bookmark-list-buffer-name)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(bookem-unmark)
	(forward-line)))))

(defun bookem-delete-marked ()
  "Remove all bookmarks with delete marks from the groups under
which they are marked."
  (interactive)
  (save-excursion
    (let ((to-delete (bookem-collect-bookmarks-if-property 'bookem-marked-for-delete)))
      (mapc (lambda (x)
	      (let ((group (car x))
		    (bookmark-name (cadr x)))
		(bookem-delete-bookmark-from-group bookmark-name group)))
	    to-delete))
    (bookem-save-bookmarks-to-file)
    (bookem-list-bookmarks)))

(defun bookem-delete-bookmark-from-group (bookmark-name group)
  "Remove bookmark with the given name from the given group. The
  the bookmark is only assigned to this group delete the bookmark altogether."
  (let (groups (bookmark (assoc bookmark-name bookem-bookmark-list)))
    (if (and bookmark
	     (member group (setq groups (plist-get (cdr bookmark) :groups))))
	(if (<= (length groups) 1)
	    (setq bookem-bookmark-list (assq-delete-all bookmark-name bookem-bookmark-list))
	  (plist-put (cdr bookmark) :groups (delq group groups))))))

(defun bookem-collect-bookmarks-if-property (property)
  "Collect a list of bookmarks whose line in the bookmark list
  has the given property which evaluates to non-nil. The returned
  list is in the form ((GROUP REST-OF-BOOKMARK) (GROUP
  REST-OF-BOOKMARK) ...) where GROUP is the group under which the
  bookmark has the text property."
  (with-current-buffer (get-buffer-create bookem-bookmark-list-buffer-name)
    (let (bookmark collected (point -1))
      (save-excursion
	(beginning-of-buffer)
	(while (> (point-max) (point))
	  (setq point (point))
	  (when (and (setq bookmark (get-text-property point 'bookem-bookmark-property))
		     (get-text-property point property))
	    (add-to-list 'collected (cons (get-text-property point 'bookem-group-name-property)
					  bookmark)))
	  (forward-line)))
      collected)))

(defun bookem-set-line-props-if (prep-property properties)
  "If PREP-PROPERTY text property is non-nil for text at point
  will apply PROPERTIES to entire line."
  (let (found-property start-line end-line)
    (setq found-property (get-text-property (point) prep-property))
    (when found-property
      (save-excursion
	(end-of-line)
	(setq end-line (point))
	(beginning-of-line)
	(setq start-line (point)))
      (add-text-properties start-line end-line properties))
    found-property))

;;
;; Public Commands
;;
(defun bookem-goto-bookmark (&optional bookmark-name group-name)
  "Goto any bookmark registered with bookem."
  (interactive)
  (let* ((group-name (or group-name
			 (bookem-prompt-group-name t)))
	 (bookmark-name (or bookmark-name
			    (bookem-prompt-bookmark-name group-name t)))
	 (bookmark (assoc bookmark-name bookem-bookmark-list)))
    (when bookmark
      (bookem-display-from-bookmark-plist bookmark))))

(defun bookem-bookmark-buffer (&optional buffer)
  (interactive)
  "Add a bookmark for a given buffer. If no buffer is given assume the current buffer."
  (let* (buffer suggested-name group-name groups bookmark-name new-bookmark
         (bookmark-type-name (bookem-prompt-type-name buffer))
	 (bookmark-type-plist (bookem-type-from-type-name bookmark-type-name))
	 (bookmark-type (plist-get bookmark-type-plist :type))
	 (make-defun (plist-get bookmark-type-plist :make))
	 (suggest-name-defun (plist-get bookmark-type-plist :suggest-name)))

    (unless bookmark-type-plist
      (error "Cannot find bookmark type."))

    (setq buffer (or buffer (current-buffer)))
    (setq suggested-name (and suggest-name-defun (funcall suggest-name-defun buffer)))
    (setq bookmark-name (bookem-prompt-bookmark-name nil nil suggested-name))

    (if (setq new-bookmark (assoc bookmark-name bookem-bookmark-list))
	(progn
	  ;; fixme: need to prompt for overwrite acknowledge here.
	  (bookem-ass-equal-delete bookmark-name bookem-bookmark-list)
	  (setq groups (plist-get (cdr new-bookmark) :groups))))

    (setq new-bookmark (bookem-create-bookmark bookmark-name bookmark-type make-defun buffer))

    (when (or (not bookmark-name)
	      (equal "" bookmark-name))
      (error "Invalid bookmark name."))
    (unless new-bookmark
      (error "Could not create bookmark."))

    (unless groups
      (while (not (equal "" (setq group-name (bookem-prompt-group-name nil t))))
	(add-to-list 'groups group-name)))

    (unless groups
      (add-to-list 'groups "General"))

    (plist-put (cdr new-bookmark) :groups groups)

    (add-to-list 'bookem-bookmark-list new-bookmark)
    (bookem-save-bookmarks-to-file)
    (message "Bookmark Added.")))

(defun bookem-list-bookmarks ()
  "List the bookmarks in a buffer."
  (interactive)
  (switch-to-buffer (bookem-list-buffer-create)))

(bookem-init)
(provide 'bookem)
