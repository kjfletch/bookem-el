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
;;  c-defun   - TODO bookmark a function in a c file.
;;  dired     - Not specifically a seperate bookmark type, works
;;               as a file bookmark. Open a dired buffer for a given
;;               bookmarked directory.
;;  info      - TODO Bookmark a location in an info document.
;;  woman     - TODO Bookmark a position in a man page.
;;
;; Changelog
;;
;;  WIP - 2010-??-??
;;   - todo implement new-bookmark (for simple file bookmarks).
;;   - todo implement saving of bookmarks.
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

;; 
;; Bookmarks file format: (bookem/bookem-bookmarks.el)
;; (:bookmarks
;;  (("bookem" .
;;    ((:name "bookem src"
;;      :type :file
;;      :location (:path "/home/kjfletch/repos/bookem-el/bookem.el" :line 20))
;;     (:name "bookem bookmarks file"
;;      :type :file
;;      :location (:path "/home/kjfletch/.emacs.d/bookem/bookem-bookmarks.el" :line 1))
;;     (:name "bookem directory"
;;      :type :file
;;      :location (:path "/home/kjfletch/.emacs.d/bookem/" :line 1))))
;;   ("dummy" .
;;    ((:name "#1"
;;      :type :file
;;      :location (:path "~/.emacs" :line 10))
;;     (:name "#2"
;;      :type :file
;;      :location (:path "~/.emacs" :line 20))))))

(defvar bookem-dir "~/.emacs.d/bookem"
  "Define where bookem associated files are stored.")

(defvar bookem-bookmarks-path 
  (concat (file-name-as-directory bookem-dir) "bookem-bookmarks.el")
  "Define where the bookem bookmarks file is stored.")

(defvar bookem-bookmark-types
  '((:name   "File Bookmark" 
     :type   :file 
     :type-p bookem-bookmark-type-file-p
     :make   bookem-bookmark-type-file-make
     :lookup bookem-bookmark-type-file-lookup)))

(setq bookem-bookmarks nil)
(setq bookem-active-group nil)

(defun bookem-init ()
  "Initialise bookem module and storage space."
  (unless (file-directory-p bookem-dir)
    (make-directory bookem-dir))
  
  (when (file-readable-p bookem-bookmarks-path)
    (setq bookem-bookmarks (read (bookem-read-file bookem-bookmarks-path)))))

(defun bookem-get-bookmark-lookup-func (book-type)
  "Return a bookmark lookup function which can be used to
find a bookmark of a given type."
  (plist-get (bookem-get-bookmark-type-struct-from-type book-type)
	     :lookup))

(defun bookem-get-bookmark-type-struct-from-name (type-name)
  "From a type name return the type structure."
  (bookem-find-plist-from-match bookem-bookmark-types :name type-name))

(defun bookem-get-bookmark-type-struct-from-type (type)
  "From a type return the type structure."
  (bookem-find-plist-from-match bookem-bookmark-types :type type))
 
(defun bookem-get-available-type-names-for-buffer (&optional buffer)
  (let ((buffer     (or buffer (current-buffer)))
	(type-names nil))
    (when buffer
      (mapc (lambda (type)
	      (if (funcall (plist-get type :type-p) buffer)
		  (setq type-names (cons (plist-get type :name) type-names))))
	    bookem-bookmark-types))
    type-names))
  
(defun bookem-bookmark-type-file-lookup (book-loc)
  "Bookmark lookup function used to search for simple file type bookmarks."
  (let* ((path (plist-get book-loc :path))
	 (line (plist-get book-loc :line))
	 (buffer (find-file-noselect path)))
    `(:buffer ,buffer :line ,line)))

(defun bookem-bookmark-type-file-p (buffer)
  (message "TODO")
  t)

(defun bookem-bookmark-type-file-make (buffer)
  `(:file "~/.emacs" :line 10))

(defun bookem-find-plist-from-match (list key value)
  "Given a list of property lists return the entry wich has a matching key with
expected value."
  (let ((found nil))
    (mapc (lambda (x) 
	    (when (equal value (plist-get x key))
	      (setq found x)))
	  list)
    found))

(defun bookem-display-from-bookmark-struct (bookmark)
  "Find and display the bookmark from the given bookmark structure."
  (let* ((book-type (plist-get bookmark :type))
	 (book-name (plist-get bookmark :name))
	 (book-loc  (plist-get bookmark :location))
	 (book-lookup (bookem-get-bookmark-lookup-func book-type)))
    (when book-lookup
      (bookem-display-from-display-info (funcall book-lookup book-loc)))))

(defun bookem-display-from-display-info (book-display-info)
  "Display the bookmark given the display info structure (buffer line)."
  (let ((buffer (plist-get book-display-info :buffer))
	(line (plist-get book-display-info :line)))
    (when (and buffer line)
      (switch-to-buffer buffer)
      (goto-line line))))

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
      (write-reagion (point-pin)
		     (pontt-max)
		     filepath))))

(defun bookem-get-all-groups ()
  "Return the part of the bookmarks structure which holds the bookmark groups."
  (plist-get bookem-bookmarks :bookmarks))

(defun bookem-get-group (group)
  "Given a group name returns the structure for that bookmark group.
Return nil if not found."
  (let* ((group-list (bookem-get-all-groups))
	 (found-group (assoc group group-list)))
    (if found-group
	(cdr found-group))))

(defun bookem-get-group-check (group)
  "If group is a string locate the group structure and return. 
If group is a structure already just return it.
If group can't be found returns nil."
  (if (stringp group)
      (bookem-get-group group)
    group))

(defun bookem-list-bookmark-names-in-group (group)
  "Given a group return a list of all bookmark names in that group.
Group can be a group name or a bookmark-group structure."
  (let ((found-group (bookem-get-group-check group)))
    (mapcar (lambda (x) (plist-get x :name)) found-group)))
    
(defun bookem-list-group-names ()
  "Return a list of group names."
  (mapcar (lambda (x) (car x)) (bookem-get-all-groups)))

(defun bookem-bookmark-from-group (group bookmark)
  "Get a bookmark (by name) from a group (by name or structure).
Returns nil if bookmark is not found."
  (let ((found-group (bookem-get-group-check group)))
    (bookem-find-plist-from-match found-group :name bookmark)))

(defun bookem-prompt-group-name (&optional require-match)
  "Prompt for a group name from the list of all group names."
  (let ((group-names (bookem-list-group-names)))

    (when (and require-match (not group-names))
      (error "No groups."))

    (setq bookem-active-group
	  (ido-completing-read "Group: " group-names nil require-match nil nil bookem-active-group))
    bookem-active-group))

(defun bookem-prompt-bookmark-name (group &optional require-match)
  "Prompt for a bookmark name from the list of all bookmarks in the given group"
  (let ((bookmarks-in-group (bookem-list-bookmark-names-in-group group)))
    
    (when (and require-match (not bookmarks-in-group))
      (error "No bookmarks for group"))

    (ido-completing-read "Bookmark: " bookmarks-in-group nil require-match)))

(defun bookem-prompt-bookmark-type (buffer)
  "Prompt for a bookmark type of the given buffer."
  (let* ((valid-type-names (bookem-get-available-type-names-for-buffer buffer)))
    (if valid-type-names
	(ido-completing-read "Bookmark Type: " valid-type-names nil t)
      (error "No bookmark types defined for this buffer type."))))

(defun bookem-goto-bookmark (&optional bookmark-name group-name)
  "Goto any bookmark registered with bookem."
  (interactive)
  (let* ((group-name (or group-name
			 (bookem-prompt-group-name t)))
	 (bookmark-name (or bookmark-name
			    (bookem-prompt-bookmark-name group-name t)))
	 (bookmark (bookem-bookmark-from-group group-name bookmark-name)))
    (when bookmark
      (bookem-display-from-bookmark-struct bookmark))))

(defun bookem-bookmark-buffer (&optional buffer)
  (interactive)
  "Add a bookmark for a given buffer. If no buffer is given assume the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
	 (bookmark-type-name (bookem-prompt-bookmark-type buffer))
	 (bookmark-type (bookem-get-bookmark-type-struct-from-name bookmark-type-name))
	 (make-defun (plist-get bookmark-type :make))
	 (loc (and make-defun (funcall make-defun buffer)))
	 (group (bookem-prompt-group-name))
	 (bookmark (bookem-prompt-bookmark-name group))
	 (group-struct nil)
	 (bookmark-struct nil))
    (message (concat group ":" bookmark))))
		  