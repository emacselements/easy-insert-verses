;; -*- lexical-binding: t; -*-
;;; easy-insert-verses.el --- Insert verses from the Bible into the buffer
;; Author: Raoul Comninos
;; Version: 1.0
;; Created: 2025-06-13
;; URL: https://github.com/gnarledgrip/easy-insert-verses
;; Package-Requires: ((emacs "24.1"))

;; This file is part of GNU Emacs.
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; easy-insert-verses is an Emacs package that allows users to quickly insert verses from the King James Version (KJV) of the Bible into any Emacs buffer.
;; It provides convenient interactive commands to insert random verses, matched chapters, custom verse ranges, or all verses matching a regular expression.
;; This is especially useful for writers, researchers, and anyone who frequently references scripture or wishes to add random or specific Bible verses to their notes or documents.
;;
;; The package expects a plain text file (by default named 'kjv.txt') in which each line contains a verse, typically formatted as 'Book|Chapter|Verse|Text'.
;; Users can configure the path to this file if needed.
;;
;; Interactive commands include:
;; - `insert-random-verses`: Insert N random verses.
;; - `insert-chapters`: Insert all verses matching a regex pattern (such as a chapter).
;; - `insert-verses`: Insert a range of verses by specifying start and end patterns.
;; - `search-kjv`: Insert all verses matching a given regex pattern.
;;
;; Keybindings are provided for fast access:
;; - C-c i : insert-random-verses
;; - C-c c : insert-chapters
;; - C-c v : insert-verses
;; - C-c s : search-kjv
;;
;; See the README for more information and usage examples.

;;; Code:

(defun insert-random-verses (n)
  (interactive "nEnter the number of verses: ")
  (let ((text-file "~/easy-insert-verses/kjv.txt")
        (lines nil))
    (with-temp-buffer
      (insert-file-contents text-file)
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))
              lines)
        (forward-line 1)))
    (dotimes (_ n)
      (insert (nth (random (length lines)) lines) "\n"))))

(defvar last-pattern nil "Store the last pattern entered by the user.")

(defun insert-chapters (text-file pattern)
  (interactive
   (let* ((file "~/easy-insert-verses/kjv.txt")
          (pat (read-string "Enter the pattern: " last-pattern)))
     (setq last-pattern pat)
     (list file pat)))
  (let ((lines nil))
    (with-temp-buffer
      (insert-file-contents text-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (when (string-match-p pattern line)
            (push line lines)))
        (forward-line 1)))
    (if lines
        (progn
          (dolist (line (nreverse lines))
            (insert line "\n"))
          (message "Text inserted."))
      (message "No matching lines found."))))

(defvar last-start-pattern nil "Store the last start pattern entered by the user.")

(defun insert-verses ()
  (interactive)
  (let* ((text-file "~/easy-insert-verses/kjv.txt")
         (start-pattern (read-string "Enter the start pattern: " last-start-pattern))
         (book-chap (when (string-match "\\(.*\\)|\\([0-9]+\\)|\\([0-9]+\\)|" start-pattern)
                      (format "%s|%s|"
                              (match-string 1 start-pattern)
                              (match-string 2 start-pattern))))
         (max-verse 0)
         end-pattern
         (lines nil)
         (in-range nil))
    (setq last-start-pattern start-pattern)
    (when book-chap
      (with-temp-buffer
        (insert-file-contents text-file)
        (goto-char (point-min))
        (let ((pattern (format "%s\\([0-9]+\\)|" book-chap)))
          (while (re-search-forward pattern nil t)
            (setq max-verse (max max-verse
                                 (string-to-number (match-string 1))))))))
    (setq end-pattern (if (> max-verse 0)
                          (format "%s%d|" book-chap max-verse)
                        start-pattern))
    (setq end-pattern (read-string (format "Enter the end pattern [%s]: " end-pattern)
                                   nil nil end-pattern))
    (with-temp-buffer
      (insert-file-contents text-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (when in-range
            (push line lines))
          (when (string-match-p start-pattern line)
            (setq in-range t)
            (push line lines))
          (when (and in-range (string-match-p end-pattern line))
            (setq in-range nil)))
        (forward-line 1)))
    (dolist (line (nreverse lines))
      (insert line "\n"))))

(defun search-kjv ()
  "Search the KJV for verses matching a regex and insert all hits into the buffer."
  (interactive)
  (let ((text-file "~/easy-insert-verses/kjv.txt")
        (regex (read-string "Enter regex to search KJV: "))
        (lines nil))
    (with-temp-buffer
      (insert-file-contents text-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (when (string-match-p regex line)
            (push line lines)))
        (forward-line 1)))
    (setq lines (nreverse lines))
    (if lines
        (dolist (line lines)
          (insert line "\n"))
      (message "No matches found."))))

(global-set-key (kbd "C-c i") 'insert-random-verses)
(global-set-key (kbd "C-c c") 'insert-chapters)
(global-set-key (kbd "C-c v") 'insert-verses)
(global-set-key (kbd "C-c s") 'search-kjv)

(provide 'easy-insert-verses)
