;;; snapshot-timemachine.el --- Step through (Btrfs, ZFS, ...) snapshots of files

;; Copyright (C) 2015 by Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; URL: https://github.com/mrBliss/snapshot-timemachine
;; Version: 0.1
;; Package-Requires: ()
;; Created: Apr 4 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO
;; * go to prev/next snapshot with an actual diff
;; * highlight diff in margins
;; * browse diffs?
;; * snapshot timeline?
;; * relative timestamps
;; * dired?
;; * compatibility with ZFS: http://wiki.complete.org/ZFSAutoSnapshots



;;; Code:

(require 'cl-lib)

(defvar snapper-timemachine-time-format "%a %d %b %Y %R"
  "The format to use when displaying a snapshot's time.
The default format is \"sat 14 mar 2015 10:35\".")


;; A struct representing a BTRFS snapshot made by snapper.
(cl-defstruct snapper-timemachine-snapshot
  id path date)

;;; Zipper

;; A zipper suited for tracking focus in a list.
(cl-defstruct zipper focus before after)

(defun zipper-from-list (l)
  "Make a zipper from the given list L.
The first element of the list will be focused.  Return nil when
the list was empty."
  (when l
    (make-zipper
     :focus  (car l)
     :before nil
     :after  (cdr l))))

(defun zipper-to-list (z)
  "Convert the zipper Z back to a list.
The order is preserved, but the focus is lost."
  (let ((l (cons (zipper-focus z) (zipper-after z)))
        (before (zipper-before z)))
    (while before
      (push (car before) l)
      (setq before (cdr before)))
    l))


(defun zipper-at-end (z)
  "Return non-nil when the zipper Z is at the last element of the list."
  (null (zipper-after z)))

(defun zipper-at-start (z)
  "Return non-nil when the zipper Z is at the first element of the list."
  (null (zipper-before z)))

(defun zipper-shift-next (z)
  "Shifts the zipper Z to the next element in the list.
Return Z unchanged when at the last element."
  (if (zipper-at-end z) z
    (make-zipper
     :focus  (car (zipper-after z))
     :before (cons (zipper-focus z) (zipper-before z))
     :after  (cdr (zipper-after z)))))

(defun zipper-shift-prev (z)
  "Shifts the zipper Z to the previous element in the list.
Return Z unchanged when at the first element."
  (if (zipper-at-start z) z
    (make-zipper
     :focus  (car (zipper-before z))
     :before (cdr (zipper-before z))
     :after  (cons (zipper-focus z) (zipper-after z)))))

(defun zipper-shift-end (z)
  "Shifts the zipper Z to the last element in the list.
Return Z unchanged when already at the last element in the list."
  (if (zipper-at-end z) z
    (let ((new-before (cons (zipper-focus z) (zipper-before z)))
          (after (zipper-after z)))
      (while (cdr after)
        (push (car after) new-before)
        (setq after (cdr after)))
      (make-zipper
       :focus (car after)
       :before new-before
       :after nil))))

(defun zipper-shift-start (z)
  "Shifts the zipper Z to the first element in the list.
Return Z unchanged when already at the first element in the list."
  (if (zipper-at-start z) z
    (let ((new-after (cons (zipper-focus z) (zipper-after z)))
          (before (zipper-before z)))
      (while (cdr before)
        (push (car before) new-after)
        (setq before (cdr before)))
      (make-zipper
       :focus (car before)
       :before nil
       :after new-after))))

(defun zipper-shift-to (z predicate)
  "Shift the zipper Z to an element satisfying PREDICATE.
First try the next elements, then the previous ones.  Returns nil
when no element satisfies PREDICATE."
  (or
   ;; First go all the way to the end
   (cl-loop for z* = z then (zipper-shift-next z*)
            if (funcall predicate (zipper-focus z*))
            return z*
            until (zipper-at-end z*))
   ;; If we haven't found it by then, start again from z and go all the way to
   ;; the start
   (cl-loop for z* = z then (zipper-shift-prev z*)
            if (funcall predicate (zipper-focus z*))
            return z*
            until (zipper-at-start z*))))

;;; Internal variables

(defvar-local snapper-timemachine-snapshot-dir nil
  "The snapshot directory associated with the buffer.  Is nil
  when there is none.")

(defvar-local snapper-timemachine-buffer-snapshots nil
  "A zipper of `snapper-timemachine-snapshot' structs representing
  the snapshots of the current buffer's file.")

(defvar-local snapper-timemachine-original-file nil
  "Maintains the path to the original (most recent) file.")


(defun snapper-timemachine-find-snapshot-dir (dir)
  "Find the directory containing the snapshots.
Starts in DIR and looks for a directory named \".snapshots\"."
  (let ((file (expand-file-name ".snapshots" dir)))
    ;; We can't use `locate-dominating-file' for this because it stops at ~
    (if (file-exists-p file)
        file
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (unless (equal "/" parent-dir)
          (snapper-timemachine-find-snapshot-dir parent-dir))))))

(defun snapper-timemachine-find-snapshots (snapshot-dir)
  "Collect all snapshots in the given SNAPSHOT-DIR.
For each valid snapshot directory, a
`snapper-timemachine-snapshot' struct is created."
  (cl-loop for file in (directory-files snapshot-dir t)
           for filename = (file-name-nondirectory file)
           when (string-match-p "[0-9]+" filename)
           collect (make-snapper-timemachine-snapshot
                    :id (string-to-number filename)
                    :path (concat file "/snapshot/")
                    :date (nth 5 (file-attributes file)))))

(defun snapper-timemachine-path-in-snapshot (file snapshot snapshot-dir)
  "Return the absolute path of the given FILE in SNAPSHOT.
FILE is either an absolute path or a relative path interpreted
against `default-directory'.  SNAPSHOT-DIR is the directory
containing the snapshots."
  (let* ((file* (expand-file-name file)) ;; "/home/thomas/.emacs.d/init.el"
         ;; "/home/.snapshots/182/snapshot/"
         (snapshot-path (snapper-timemachine-snapshot-path snapshot))
         ;; "/home/"
         (snapshot-root (file-name-directory
                         (directory-file-name snapshot-dir)))
         ;; "thomas/.emacs.d/init.el"
         (rel-path (s-chop-prefix snapshot-root file*)))
    ;; "/home/.snapshots/182/snapshot/thomas/.emacs.d/init.el"
    (concat snapshot-path rel-path)))

(defun snapper-timemachine-file-snapshots (file snapshot-dir)
  "Return a list of all the snapshots of this FILE in SNAPSHOT-DIR.
Snapshots in which FILE doesn't exist are discarded."
  (cl-loop for snapshot in (snapper-timemachine-find-snapshots snapshot-dir)
           for path-in-snapshot = (snapper-timemachine-path-in-snapshot
                                   file snapshot snapshot-dir)
           when (file-exists-p path-in-snapshot)
           collect snapshot))

(defun snapper-timemachine-show-focused-snapshot ()
  "Display the currently focused snapshot in the buffer.
The current snapshot is stored in
`snapper-timemachine-buffer-snapshots'."
  (let* ((snapshot (zipper-focus snapper-timemachine-buffer-snapshots))
         (time (format-time-string
                snapper-timemachine-time-format
                (snapper-timemachine-snapshot-date snapshot))))
    (setq buffer-read-only nil)
    (insert-file-contents
     (snapper-timemachine-path-in-snapshot
      snapper-timemachine-original-file snapshot
      snapper-timemachine-snapshot-dir)
     nil nil nil t)
    (setq buffer-read-only t
          buffer-file-name (snapper-timemachine-snapshot-path snapshot))
    (set-buffer-modified-p nil)
    (setq mode-line-buffer-identification
          (list (propertized-buffer-identification "%12b") "@"
                (propertize
                 (number-to-string (snapper-timemachine-snapshot-id snapshot))
                 'face 'bold)
                " " time))
    (message "Snapshot %d from %s"
             (snapper-timemachine-snapshot-id snapshot) time)))

(defun snapper-timemachine-show-next-snapshot ()
  "Show the next snapshot in time."
  (interactive)
  (if (zipper-at-end snapper-timemachine-buffer-snapshots)
      (message "Last snapshot")
    (setq snapper-timemachine-buffer-snapshots
          (zipper-shift-next snapper-timemachine-buffer-snapshots))
    (snapper-timemachine-show-focused-snapshot)))

(defun snapper-timemachine-show-prev-snapshot ()
  "Show the previous snapshot in time."
  (interactive)
  (if (zipper-at-start snapper-timemachine-buffer-snapshots)
      (message "First snapshot")
    (setq snapper-timemachine-buffer-snapshots
          (zipper-shift-prev snapper-timemachine-buffer-snapshots))
    (snapper-timemachine-show-focused-snapshot)))

(defun snapper-timemachine-show-nth-snapshot ()
  "Choose which snapshot to show."
  (interactive)
  (let* ((candidates
          (mapcar (lambda (snapshot)
                    (cons
                     (format "Snapshot %d from %s"
                             (snapper-timemachine-snapshot-id snapshot)
                             (format-time-string
                              snapper-timemachine-time-format
                              (snapper-timemachine-snapshot-date snapshot)))
                     (snapper-timemachine-snapshot-id snapshot)))
                  (zipper-to-list snapper-timemachine-buffer-snapshots)))
         (choice (cdr (assoc
                       (completing-read
                        "Choose snapshot: " (copy-list candidates) nil t)
                       candidates))))
    (when choice
      (let ((z* (zipper-shift-to
                 snapper-timemachine-buffer-snapshots
                 (lambda (s)
                   (message "ID: %d" (snapper-timemachine-snapshot-id s))
                   (= (snapper-timemachine-snapshot-id s) choice)))))
        (when z*
          (setq snapper-timemachine-buffer-snapshots z*)
          (snapper-timemachine-show-focused-snapshot))))))

(defun snapper-timemachine-quit ()
  "Exit the timemachine."
  (interactive)
  (kill-buffer))

(define-minor-mode snapper-timemachine-mode
  "TODO"
  :init-value nil
  :lighter " Timemachine"
  :keymap
  '(("n" . snapper-timemachine-show-next-snapshot)
    ("p" . snapper-timemachine-show-prev-snapshot)
    ("j" . snapper-timemachine-show-nth-snapshot)
    ("q" . snapper-timemachine-quit))
  :group 'snapper-timemachine)


;;;###autoload
(cl-defun snapper-timemachine ()
  "Start the snapper timemachine for the current file.
TODO"
  (interactive)
  (if (not (buffer-file-name))
      (message "The current buffer isn't visiting a file.")
    (let ((snapshot-dir
           (snapper-timemachine-find-snapshot-dir default-directory)))
      (if (null snapshot-dir)
          (message "Snapshot folder '%s' not found" snapshot-dirname)
        (let ((snapshots (cl-sort
                          (snapper-timemachine-file-snapshots
                           (buffer-file-name) snapshot-dir)
                          #'< :key #'snapper-timemachine-snapshot-id)))
          (if (null snapshots)
              (message "No snapshots found")
            (let* ((timemachine-buffer (format "snapshot:%s" (buffer-name)))
                   (cur-line (line-number-at-pos))
                   (mode major-mode)
                   (file-name (buffer-file-name))
                   ;; We already did a null check, so `zipper-from-list'
                   ;; shouldn't fail.
                   (snapshot-zipper (zipper-shift-end
                                     (zipper-from-list snapshots))))
              (with-current-buffer (get-buffer-create timemachine-buffer)
                (switch-to-buffer timemachine-buffer)
                (funcall mode)
                (setq snapper-timemachine-original-file    file-name
                      snapper-timemachine-buffer-snapshots snapshot-zipper
                      snapper-timemachine-snapshot-dir     snapshot-dir)
                (snapper-timemachine-show-focused-snapshot)
                (goto-char (point-min))
                (forward-line (1- cur-line))
                (snapper-timemachine-mode)))))))))




(provide 'snapshot-timemachine)
;;; snapshot-timemachine.el ends here
