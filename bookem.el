;;; bookem.el
;;
;; Author: Kevin J. Fletcher <kevinjohn.fletcher@googlemail.com>
;; Maintainer: Kevin J. Fletcher <kevinjohn.fletcher@googlemail.com>
;; Keywords: bookem, bookmarks
;; Homepage: http://github.com/kjfletch/bookem-el/bookem.el
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
  (cond
   ((eq book-type :file)
    'bookem-bookmark-lookup-func-file)
   ((t (error "Unknown bookmark type.")))))
   
(defun bookem-bookmark-lookup-func-file (book-loc)
  "Bookmark lookup function used to search for simple file type bookmarks."
  (let* ((path (plist-get book-loc :path))
	 (line (plist-get book-loc :line))
	 (buffer (find-file-noselect path)))
    `(:buffer ,buffer :line ,line)))

(defun bookem-display-from-bookmark-structure (bookmark)
  "Find and display the bookmark from the given bookmark structure."
  (let* ((book-type (plist-get bookmark :type))
	 (book-name (plist-get bookmark :name))
	 (book-loc  (plist-get bookmark :location))
	 (book-lookup (bookem-get-bookmark-lookup-func book-type)))
    (message book-name)
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
  (let ((found-group (bookem-get-group-check group))
	(found-bookmark nil))
    (mapc (lambda (x) (if (equal bookmark (plist-get x :name)) (setq found-bookmark x))) found-group)
    found-bookmark))

(defun bookem-prompt-group-name ()
  "Prompt for a group name from the list of all group names."
  (let ((group-names (bookem-list-group-names)))
    (when group-names
      (setq bookem-active-group
	    (ido-completing-read "Group: " group-names nil t nil nil bookem-active-group))
      bookem-active-group)))

(defun bookem-prompt-bookmark-name (group)
  "Prompt for a bookmark name from the list of all bookmarks in the given group"
  (let ((bookmarks-in-group (bookem-list-bookmark-names-in-group group)))
    (when bookmarks-in-group
      (ido-completing-read "Bookmark: " bookmarks-in-group nil t))))

(defun bookem-goto-bookmark (&optional bookmark-name group-name)
  "Goto any bookmark registered with bookem."
  (interactive)
  (let* ((group-name (or group-name
			 (bookem-prompt-group-name)))
	 (bookmark-name (or bookmark-name
			    (bookem-prompt-bookmark-name group-name)))
	 (bookmark (bookem-bookmark-from-group group-name bookmark-name)))
    (when bookmark
      (bookem-display-from-bookmark-structure bookmark))))
      
    
	 
		      
		       