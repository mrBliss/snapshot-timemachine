;;; snapper-timemachine.el --- Step through Snapper's BTRFS snapshots of files

;; Copyright (C) 2015 by Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; URL: https://github.com/mrBliss/snapper-timemachine
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
;; * benchmark zipper implementations
;; * go to prev/next snapshot with an actual diff
;; * highlight diff in margins
;; * browse diffs?
;; * snapshot timeline?
;; * dired?
;; * compatibility with ZFS: http://wiki.complete.org/ZFSAutoSnapshots



;;; Code:

(require 'cl-lib)
(require 'eieio)

(defvar snapper-timemachine-time-format "%a %d %b %Y %R"
  "The format to use when displaying a snapshot's time.
The default format is \"sat 14 mar 2015 10:35\".")


;; A struct representing a BTRFS snapshot made by snapper.
(cl-defstruct snapper-timemachine-snapshot
  id path date)

;;; Zipper

(defclass zipper ()
  ((focus  :initarg :focus)
   (before :initarg :before)
   (after  :initarg :after))
  "A zipper suited for tracking focus in a list.")

(cl-defstruct zipper-struct focus before after)

(defalias 'zipper-list-focus 'car)
(defalias 'zipper-list-before 'cadr)
(defalias 'zipper-list-after 'caddr)

(defun make-zipper-list (focus before after)
  (list focus before after))

(defun zipper-from-list (l)
  "Make a zipper from the given list L.
The first element of the list will be focused.  Return nil when
the list was empty."
  (when l
    (make-instance
     'zipper
     :focus (car l)
     :before nil
     :after (cdr l))))

(defun zipper-struct-from-list (l)
  "Make a zipper from the given list L.
The first element of the list will be focused.  Return nil when
the list was empty."
  (when l
    (make-zipper-struct
     :focus  (car l)
     :before nil
     :after  (cdr l))))

(defun zipper-list-from-list (l)
  "Make a zipper from the given list L.
The first element of the list will be focused.  Return nil when
the list was empty."
  (when l (make-zipper-list (car l) nil (cdr l))))

(defun zipper-to-list (z)
  "Convert the zipper Z back to a list.
The order is preserved, but the focus is lost."
  (with-slots (focus before after) z
    (append (nreverse before) (cons focus after))))

(defun zipper-struct-to-list (z)
  "Convert the zipper Z back to a list.
The order is preserved, but the focus is lost."
  (append (nreverse (zipper-struct-before z))
          (cons (zipper-struct-focus z)
                (zipper-struct-after z))))


(defun zipper-list-to-list (z)
  "Convert the zipper Z back to a list.
The order is preserved, but the focus is lost."
  (append (nreverse (zipper-list-before z))
          (cons (zipper-list-focus z)
                (zipper-list-after z))))

(defun zipper-focus (z)
  "Return the value at the `focus' slot of the zipper Z."
  (oref z :focus))

(defun zipper-at-end (z)
  "Return non-nil when the zipper Z is at the last element of the list."
  (null (oref z :after)))

(defun zipper-struct-at-end (z)
  "Return non-nil when the zipper Z is at the last element of the list."
  (null (zipper-struct-after z)))

(defun zipper-list-at-end (z)
  "Return non-nil when the zipper Z is at the last element of the list."
  (null (zipper-list-after z)))

(defun zipper-at-start (z)
  "Return non-nil when the zipper Z is at the first element of the list."
  (null (oref z :before)))

(defun zipper-struct-at-start (z)
  "Return non-nil when the zipper Z is at the first element of the list."
  (null (zipper-struct-before z)))

(defun zipper-list-at-start (z)
  "Return non-nil when the zipper Z is at the first element of the list."
  (null (zipper-list-before z)))

(defun zipper-shift-next (z)
  "Shifts the zipper Z to the next element in the list.
Return Z unchanged when at the last element."
  (if (zipper-at-end z) z
    (with-slots (focus before after) z
      (make-instance 'zipper
                     :focus  (car after)
                     :before (cons focus before)
                     :after  (cdr after)))))

(defun zipper-struct-shift-next (z)
  "Shifts the zipper Z to the next element in the list.
Return Z unchanged when at the last element."
  (if (zipper-struct-at-end z) z
    (make-zipper-struct
     :focus  (car (zipper-struct-after z))
     :before (cons (zipper-struct-focus z) (zipper-struct-before z))
     :after  (cdr (zipper-struct-after z)))))

(defun zipper-list-shift-next (z)
  "Shifts the zipper Z to the next element in the list.
Return Z unchanged when at the last element."
  (if (zipper-list-at-end z) z
    (make-zipper-list
     (car (zipper-list-after z))
     (cons (zipper-list-focus z) (zipper-list-before z))
     (cdr (zipper-list-after z)))))

(defun zipper-shift-prev (z)
  "Shifts the zipper Z to the previous element in the list.
Return Z unchanged when at the first element."
  (if (zipper-at-start z) z
    (with-slots (focus before after) z
      (make-instance 'zipper
                     :focus  (car before)
                     :before (cdr before)
                     :after  (cons focus after)))))


(defun zipper-struct-shift-prev (z)
  "Shifts the zipper Z to the previous element in the list.
Return Z unchanged when at the first element."
  (if (zipper-struct-at-start z) z
    (make-zipper-struct
     :focus  (car (zipper-struct-before z))
     :before (cdr (zipper-struct-before z))
     :after  (cons (zipper-struct-focus z) (zipper-struct-after z)))))

(defun zipper-list-shift-prev (z)
  "Shifts the zipper Z to the previous element in the list.
Return Z unchanged when at the first element."
  (if (zipper-list-at-start z) z
    (make-zipper-list
     (car (zipper-list-before z))
     (cdr (zipper-list-before z))
     (cons (zipper-list-focus z) (zipper-list-after z)))))

(defun zipper-shift-end (z)
  "Shifts the zipper Z to the last element in the list.
Return Z unchanged when already at the last element in the list."
  (if (zipper-at-end z) z
    (with-slots (focus before after) z
      (let ((new-before (cons focus before)))
        (while (cdr after)
          (push (car after) new-before)
          (setq after (cdr after)))
        (make-instance 'zipper
                       :focus (car after)
                       :before new-before
                       :after nil)))))

(defun zipper-struct-shift-end (z)
  "Shifts the zipper Z to the last element in the list.
Return Z unchanged when already at the last element in the list."
  (if (zipper-struct-at-end z) z
    (let ((new-before (cons (zipper-struct-focus z)
                            (zipper-struct-before z)))
          (after (zipper-struct-after z)))
      (while (cdr after)
        (push (car after) new-before)
        (setq after (cdr after)))
      (make-zipper-struct
       :focus (car after)
       :before new-before
       :after nil))))

(defun zipper-list-shift-end (z)
  "Shifts the zipper Z to the last element in the list.
Return Z unchanged when already at the last element in the list."
  (if (zipper-list-at-end z) z
    (let ((new-before (cons (zipper-list-focus z)
                            (zipper-list-before z)))
          (after (zipper-list-after z)))
      (while (cdr after)
        (push (car after) new-before)
        (setq after (cdr after)))
      (make-zipper-list
       (car after)
       new-before
       nil))))

(defun zipper-shift-start (z)
  "Shifts the zipper Z to the first element in the list.
Return Z unchanged when already at the first element in the list."
  (if (zipper-at-start z) z
    (with-slots (focus before after) z
      (let ((new-after (cons focus after)))
        (while (cdr before)
          (push (car before) new-after)
          (setq before (cdr before)))
        (make-instance 'zipper
                       :focus (car before)
                       :before nil
                       :after new-after)))))


(defun zipper-struct-shift-start (z)
  "Shifts the zipper Z to the first element in the list.
Return Z unchanged when already at the first element in the list."
  (if (zipper-struct-at-start z) z
    (let ((new-after (cons (zipper-struct-focus z)
                           (zipper-struct-after z)))
          (before (zipper-struct-before z)))
      (while (cdr before)
        (push (car before) new-after)
        (setq before (cdr before)))
      (make-zipper-struct
       :focus (car before)
       :before nil
       :after new-after))))

(defun zipper-list-shift-start (z)
  "Shifts the zipper Z to the first element in the list.
Return Z unchanged when already at the first element in the list."
  (if (zipper-list-at-start z) z
    (let ((new-after (cons (zipper-list-focus z)
                           (zipper-list-after z)))
          (before (zipper-list-before z)))
      (while (cdr before)
        (push (car before) new-after)
        (setq before (cdr before)))
      (make-zipper-list
       (car before)
       nil
       new-after))))


(let ((l (number-sequence 1 5000)))
  (benchmark-run 100
    (zipper-list-shift-start
     (zipper-list-shift-end
      (zipper-list-shift-prev
       (zipper-list-shift-next (zipper-list-from-list l)))))))
(0.7567493789999999 2 0.3762416620000124)
(0.7461273350000001 2 0.34710963399999173)
(0.7791949859999999 2 0.33725133200002233)
(0.7954909250000001 2 0.364250262000013)
(0.720289693 2 0.32975402499999973)

(let ((l (number-sequence 1 5000)))
  (benchmark-run 100
    (zipper-struct-shift-start
     (zipper-struct-shift-end
      (zipper-struct-shift-prev
       (zipper-struct-shift-next (zipper-struct-from-list l)))))))
(0.761351521 2 0.34140165599998795)
(0.764098266 2 0.36085432100000503)
(0.736548686 2 0.32736425299998473)
(0.756832489 2 0.3602591809999751)
(0.9457590699999999 3 0.540813944000007)

;; NOTE: only 10 runs!
(let ((l (number-sequence 1 5000)))
  (benchmark-run 10
    (zipper-shift-start
     (zipper-shift-end
      (zipper-shift-prev
       (zipper-shift-next (zipper-from-list l)))))))
(2.812881636 9 1.5561805919999472)
(2.643988546 8 1.375335917000001)
(2.613156732 8 1.3341268579999905)
(2.717466446 8 1.4040672449999931)
(2.895762801 9 1.575489095000023)






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
  (let ((snapshot (zipper-focus snapper-timemachine-buffer-snapshots)))
    (setq buffer-read-only nil)
    (insert-file-contents
     (snapper-timemachine-path-in-snapshot
      snapper-timemachine-original-file snapshot
      snapper-timemachine-snapshot-dir)
     nil nil nil t)
    (setq buffer-read-only t
          buffer-file-name (snapper-timemachine-snapshot-path snapshot))
    (set-buffer-modified-p nil)
    (message "Snapshot %d from %s"
             (snapper-timemachine-snapshot-id snapshot)
             (format-time-string
              snapper-timemachine-time-format
              (snapper-timemachine-snapshot-date snapshot)))))

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
  "TODO"
  (interactive)
  ())

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
    ("g" . snapper-timemachine-show-nth-snapshot)
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
            (let* ((timemachine-buffer (format "timemachine:%s" (buffer-name)))
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




(provide 'snapper-timemachine)
;;; snapper-timemachine.el ends here
