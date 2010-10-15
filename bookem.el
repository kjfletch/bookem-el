;;; bookem.el
;;
;; Author: Kevin J. Fletcher <kevinjohn.fletcher@googlemail.com>
;; Maintainer: Kevin J. Fletcher <kevinjohn.fletcher@googlemail.com>
;; Keywords: bookem, bookmarks
;; Homepage: http://github.com/kjfletch/bookem-el
;; Version: <WIP>
;; 
;;; Commentary
;;
;; This file provides a bookmark for emacs called bookem. This allows
;; you to create bookmarks for files and other special buffers and
;; organise them into groups.
;;
;; Bookmark types:
;;  file      - simple bookmark for a line in a file.
;;              also works on dired buffers.
;;  c-defun   - bookmark a function in a c file.
;;  info      - TODO Bookmark a location in an info document.
;;  woman     - TODO Bookmark a position in a man page.
;;
;; Changelog
;;
;;  WIP - 2010-??-??
;;
;; Copyright (C) 2010 Kevin J. Fletcher
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

(defvar bookem-bookmark-types
  '((:name   "File Bookmark" 
     :type   :file 
     :type-p bookem-type-p-file
     :make   bookem-make-file
     :lookup bookem-lookup-file
     :suggest-name bookem-suggest-name-file)
    (:name "C Function Bookmark"
     :type :c-defun
     :type-p bookem-type-p-c-defun
     :make bookem-make-c-defun
     :lookup bookem-lookup-c-defun
     :suggest-name bookem-suggest-name-c-defun))
  "List of bookem bookmark types.")

(setq bookem-bookmarks nil)
(setq bookem-active-group nil)
(setq bookem-current-bookmarks-format 1)

(defun bookem-init ()
  "Initialise bookem module and storage space."
  (unless (file-directory-p bookem-dir)
    (make-directory bookem-dir))

  (when (file-readable-p bookem-bookmarks-path)
    (setq bookem-bookmarks (read (bookem-read-file bookem-bookmarks-path))))
  
  (unless (equal (bookem-bookmarks-format)
		 bookem-current-bookmarks-format)
    (error "Unsupported bookmarks file format, please update bookem to the latest version.")))

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
 
(defun bookem-groups ()
  "Get a list of bookmark groups."
  (plist-get bookem-bookmarks :bookmarks))

(defun bookem-bookmarks-format ()
  "Get the format of the bookmarks structure"
  (or (plist-get bookem-bookmarks :format)
      bookem-current-bookmarks-format))

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

(defun bookem-group-from-name (group)
  "Given a group name returns the plist for that bookmark group.
Return nil if not found."
  (let* ((group-list (bookem-groups)))
    (assoc group group-list)))

(defun bookem-get-group-check (group)
  "If group is a string locate the group plist and return. 
If group is a plist already just return it.
If group can't be found returns nil."
  (if (stringp group)
      (bookem-group-from-name group)
    group))

(defun bookem-bookmark-names-from-group (group)
  "Given a group return a list of all bookmark names in that group.
Group can be a group name or a bookmark-group plist."
  (let ((group-bookmarks (bookem-bookmark-list-from-group group)))
    (when group-bookmarks
      (mapcar (lambda (x) (car x)) group-bookmarks))))

(defun bookem-list-group-names ()
  "Return a list of group names."
  (mapcar (lambda (x) (car x)) (bookem-groups)))

(defun bookem-bookmark-from-group (group bookmark)
  "Get a bookmark (by name) from a group (by name or plist).
Returns nil if bookmark is not found."
  (let ((group-bookmarks (bookem-bookmark-list-from-group group)))
    (when group-bookmarks
      (assoc bookmark group-bookmarks))))

(defun bookem-bookmark-list-from-group (group)
  "Get the list of bookmarks in given group.
Group can be a name or a group structure."
  (let ((found-group (bookem-get-group-check group)))
    (when found-group
      (cdr found-group))))

(defun bookem-prompt-group-name (&optional require-match)
  "Prompt for a group name from the list of all group names."
  (let ((group-names (bookem-list-group-names)))

    (when (and require-match (not group-names))
      (error "No groups."))

    (setq bookem-active-group
	  (bookem-completing-read "Group: " group-names require-match nil bookem-active-group))
    bookem-active-group))

(defun bookem-prompt-bookmark-name (group &optional require-match initial-input)
  "Prompt for a bookmark name from the list of all bookmarks in the given group"
  (let ((bookmarks-in-group (bookem-bookmark-names-from-group group)))
    
    (when (and require-match (not bookmarks-in-group))
      (error "No bookmarks for group"))

    (bookem-completing-read "Bookmark: " bookmarks-in-group require-match initial-input)))

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

(defun bookem-create-group (group-name)
  "Create a new group association."
  `(,group-name . ()))

(defun bookem-add-bookmark-to-group (bookmark group-name groups)
  "Add the given bookmark (plist) to the given group (name)."
  (let (group group-key group-bookmarks bookmark-name)
    (setq group  (or (assoc group-name groups)
		     (bookem-create-group group-name)))
    (setq bookmark-name (car bookmark))
    (setq group-key (car group))
    (setq group-bookmarks (cdr group))
    (setq groups (bookem-ass-equal-delete group-key groups))
    (setq group-bookmarks (bookem-ass-equal-delete bookmark-name group-bookmarks))
    (setq group-bookmarks (cons bookmark group-bookmarks))
    (setq group (cons group-name group-bookmarks))
    (setq groups (cons group groups))
    groups))

(defun bookem-save-bookmarks (&optional bookmarks)
  "Save the bookmark structure to disk.
If bookmarks is not nil updates the bookmark groups with new value."
  (let ((bookmark-groups (or bookmarks
			     (bookem-groups))))
    (setq bookem-bookmarks (plist-put bookem-bookmarks :bookmarks bookmark-groups))
    (setq bookem-bookmarks (plist-put bookem-bookmarks :format bookem-current-bookmarks-format))
    (bookem-write-file bookem-bookmarks-path (prin1-to-string bookem-bookmarks))))

;;
;; Bookem Bookmark List/Buffer
;;
(defun bookem-list-write-bookmark (bookmark group-name)
  "Write out the given bookmark in the current buffer."
  (let ((bookmark-name (car bookmark))
	(bookmark-rest (car bookmark))
	(start-line (point)))
    ;;              "_D_>>"
    (insert (format "     %s - %s\n" (car bookmark) (plist-get (cdr bookmark) :location)))
    (put-text-property start-line (point) 'bookem-bookmark-name-property bookmark-name)))

(defun bookem-list-write-group (group)
  "Write out the given group in the current buffer."
  (let ((group-name (car group))
	(bookmarks (cdr group))
	(start-line (point)))
    ;;               _D_           Padding before group for option flags.
    (insert (concat "   " group-name "\n"))
    (mapc (lambda (bookmark) (bookem-list-write-bookmark bookmark group-name)) bookmarks)
    (put-text-property start-line (point) 'bookem-group-name-property group-name)
    (newline)))

(defun bookem-list-buffer-create ()
  "Create and return the bookmark list buffer. If it exists the buffer 
will be refreshed."
  (let (buffer groups)
    (setq buffer (get-buffer-create "*TestBuffer*"))
    (setq groups (plist-get bookem-bookmarks :bookmarks))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Bookem Bookmark List\n")
      (insert "--------------------\n")
      
      (mapc 'bookem-list-write-group groups))
    buffer))
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
	 (bookmark (bookem-bookmark-from-group group-name bookmark-name)))
    (when bookmark
      (bookem-display-from-bookmark-plist bookmark))))

(defun bookem-bookmark-buffer (&optional buffer)
  (interactive)
  "Add a bookmark for a given buffer. If no buffer is given assume the current buffer."
  (let* (buffer suggested-name group-name bookmark-name new-bookmark
         (bookmark-type-name (bookem-prompt-type-name buffer))
	 (bookmark-type-plist (bookem-type-from-type-name bookmark-type-name))
	 (bookmark-type (plist-get bookmark-type-plist :type))
	 (make-defun (plist-get bookmark-type-plist :make))
	 (suggest-name-defun (plist-get bookmark-type-plist :suggest-name)))

    (unless bookmark-type-plist
      (error "Cannot find bookmark type."))

    (setq buffer (or buffer (current-buffer)))
    (setq suggested-name (and suggest-name-defun (funcall suggest-name-defun buffer)))
    (setq group-name (bookem-prompt-group-name))
    (setq bookmark-name (bookem-prompt-bookmark-name group-name nil suggested-name))
    (setq new-bookmark (bookem-create-bookmark bookmark-name bookmark-type make-defun buffer))

    (when (or (not bookmark-name)
	      (equal "" bookmark-name))
      (error "Invalid bookmark name."))
    (unless new-bookmark
      (error "Could not create bookmark."))
    (when (or (not group-name) 
	      (equal "" group-name))
      (error "Invalid group name."))

    (bookem-save-bookmarks 
     (bookem-add-bookmark-to-group new-bookmark group-name (bookem-groups)))
    (message "Bookmark Added.")))

(defun bookem-list-bookmarks ()
  "List the bookmarks in a buffer."
  (interactive)
  (switch-to-buffer (bookem-list-buffer-create))
  (view-mode))