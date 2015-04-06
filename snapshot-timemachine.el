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
;; * highlight diff in margins
;; * browse diffs?
;; * relative timestamps
;; * dired?
;; * compatibility with ZFS (http://wiki.complete.org/ZFSAutoSnapshots) and
;;   snapshot systems. Make it easy to adapt to your specific needs. Introduce
;;   snapshot-name.



;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar snapshot-timemachine-time-format "%a %d %b %Y %R"
  "The format to use when displaying a snapshot's time.
The default format is \"sat 14 mar 2015 10:35\".")

;; A struct representing a snapshot.
(cl-defstruct snapshot
  ;; An ascending numerical identifier for lookup and sorting
  id
  ;; The path to the snapshot directory, e.g. /home/.snapshots/2/snapshot/
  path
  ;; The date of the snapshot, format: (HIGH LOW USEC PSEC)
  date
  ;; The number of lines added/removed compared to the previous snapshot,
  ;; format: (ADDED . REMOVED). Can be nil when uninitialised.
  diffstat
  )

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

(defun zipper-shift-forwards-to (z predicate)
  "Shift the zipper Z forwards to an element satisfying PREDICATE.
Returns nil when no element satisfies PREDICATE."
  (cl-loop for z* = z then (zipper-shift-next z*)
           if (funcall predicate (zipper-focus z*))
           return z*
           until (zipper-at-end z*)))

(defun zipper-shift-backwards-to (z predicate)
  "Shift the zipper Z backwards to an element satisfying PREDICATE.
Returns nil when no element satisfies PREDICATE."
  (cl-loop for z* = z then (zipper-shift-prev z*)
           if (funcall predicate (zipper-focus z*))
           return z*
           until (zipper-at-start z*)))

(defun zipper-shift-to (z predicate)
  "Shift the zipper Z to an element satisfying PREDICATE.
First try the next elements, then the previous ones.  Returns nil
when no element satisfies PREDICATE."
  (or
   (zipper-shift-forwards-to z predicate)
   (zipper-shift-backwards-to z predicate)))

;;; Internal variables

(defvar-local snapshot-timemachine-snapshot-dir nil
  "The snapshot directory associated with the buffer.  Is nil
  when there is none.")

(defvar-local snapshot-timemachine-buffer-snapshots nil
  "A zipper of `snapshot' structs representing
  the snapshots of the current buffer's file.")

(defvar-local snapshot-timemachine-original-file nil
  "Maintains the path to the original (most recent) file.")

;;; Locating snapshots

(defun snapshot-timemachine-find-snapshot-dir (dir)
  "Find the directory containing the snapshots.
Starts in DIR and looks for a directory named \".snapshots\"."
  (let ((file (expand-file-name ".snapshots" dir)))
    ;; We can't use `locate-dominating-file' for this because it stops at ~
    (if (file-exists-p file)
        file
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (unless (equal "/" parent-dir)
          (snapshot-timemachine-find-snapshot-dir parent-dir))))))

(defun snapshot-timemachine-find-snapshots (snapshot-dir)
  "Collect all snapshots in the given SNAPSHOT-DIR.
For each valid snapshot directory, a
`snapshot' struct is created."
  (cl-loop for file in (directory-files snapshot-dir t)
           for filename = (file-name-nondirectory file)
           when (string-match-p "[0-9]+" filename)
           collect (make-snapshot
                    :id (string-to-number filename)
                    :path (concat file "/snapshot/")
                    :date (nth 5 (file-attributes file)))))

(defun snapshot-timemachine-path-in-snapshot (file snapshot snapshot-dir)
  "Return the absolute path of the given FILE in SNAPSHOT.
FILE is either an absolute path or a relative path interpreted
against `default-directory'.  SNAPSHOT-DIR is the directory
containing the snapshots."
  (let* ((file* (expand-file-name file)) ;; "/home/thomas/.emacs.d/init.el"
         ;; "/home/.snapshots/182/snapshot/"
         (snapshot-path (snapshot-path snapshot))
         ;; "/home/"
         (snapshot-root (file-name-directory
                         (directory-file-name snapshot-dir)))
         ;; "thomas/.emacs.d/init.el"
         (rel-path (string-remove-prefix snapshot-root file*)))
    ;; "/home/.snapshots/182/snapshot/thomas/.emacs.d/init.el"
    (concat snapshot-path rel-path)))

(defun snapshot-timemachine-file-snapshots (file snapshot-dir)
  "Return a list of all the snapshots of this FILE in SNAPSHOT-DIR.
Snapshots in which FILE doesn't exist are discarded."
  (cl-loop for snapshot in (snapshot-timemachine-find-snapshots snapshot-dir)
           for path-in-snapshot = (snapshot-timemachine-path-in-snapshot
                                   file snapshot snapshot-dir)
           when (file-exists-p path-in-snapshot)
           collect snapshot))

;;; Interactive timemachine functions and their helpers

(defun snapshot-timemachine-show-focused-snapshot ()
  "Display the currently focused snapshot in the buffer.
The current snapshot is stored in
`snapshot-timemachine-buffer-snapshots'."
  (let* ((snapshot (zipper-focus snapshot-timemachine-buffer-snapshots))
         (snapshot-file (snapshot-timemachine-path-in-snapshot
                         snapshot-timemachine-original-file snapshot
                         snapshot-timemachine-snapshot-dir))
         (time (format-time-string
                snapshot-timemachine-time-format
                (snapshot-date snapshot))))
    (setq buffer-read-only nil)
    (insert-file-contents snapshot-file nil nil nil t)
    (setq buffer-read-only t
          buffer-file-name snapshot-file
          default-directory (file-name-directory snapshot-file))
    (set-buffer-modified-p nil)
    (setq mode-line-buffer-identification
          (list (propertized-buffer-identification "%12b") "@"
                (propertize
                 (number-to-string (snapshot-id snapshot))
                 'face 'bold)
                " " time))
    (message "Snapshot %d from %s"
             (snapshot-id snapshot) time)))

(defun snapshot-timemachine-show-next-snapshot ()
  "Show the next snapshot in time."
  (interactive)
  (if (zipper-at-end snapshot-timemachine-buffer-snapshots)
      (message "Last snapshot")
    (setq snapshot-timemachine-buffer-snapshots
          (zipper-shift-next snapshot-timemachine-buffer-snapshots))
    (snapshot-timemachine-show-focused-snapshot)))

(defun snapshot-timemachine-show-prev-snapshot ()
  "Show the previous snapshot in time."
  (interactive)
  (if (zipper-at-start snapshot-timemachine-buffer-snapshots)
      (message "First snapshot")
    (setq snapshot-timemachine-buffer-snapshots
          (zipper-shift-prev snapshot-timemachine-buffer-snapshots))
    (snapshot-timemachine-show-focused-snapshot)))

(defun snapshot-timemachine-show-first-snapshot ()
  "Show the first snapshot in time."
  (interactive)
  (if (zipper-at-start snapshot-timemachine-buffer-snapshots)
      (message "Already at first snapshot")
    (setq snapshot-timemachine-buffer-snapshots
          (zipper-shift-start snapshot-timemachine-buffer-snapshots))
    (snapshot-timemachine-show-focused-snapshot)))

(defun snapshot-timemachine-show-last-snapshot ()
  "Show the last snapshot in time."
  (interactive)
  (if (zipper-at-end snapshot-timemachine-buffer-snapshots)
      (message "Already at last snapshot")
    (setq snapshot-timemachine-buffer-snapshots
          (zipper-shift-end snapshot-timemachine-buffer-snapshots))
    (snapshot-timemachine-show-focused-snapshot)))

(defun snapshot-timemachine-show-nth-snapshot ()
  "Choose which snapshot to show."
  (interactive)
  (let* ((candidates
          (mapcar (lambda (snapshot)
                    (cons
                     (format "Snapshot %d from %s"
                             (snapshot-id snapshot)
                             (format-time-string
                              snapshot-timemachine-time-format
                              (snapshot-date snapshot)))
                     (snapshot-id snapshot)))
                  (zipper-to-list snapshot-timemachine-buffer-snapshots)))
         (choice (cdr (assoc
                       (completing-read
                        "Choose snapshot: " candidates nil t)
                       candidates))))
    (when choice
      (let ((z* (zipper-shift-to
                 snapshot-timemachine-buffer-snapshots
                 (lambda (s)
                   (message "ID: %d" (snapshot-id s))
                   (= (snapshot-id s) choice)))))
        (when z*
          (setq snapshot-timemachine-buffer-snapshots z*)
          (snapshot-timemachine-show-focused-snapshot))))))

(defun snapshot-timemachine-snapshots-differ (s1 s2)
  "Return t when the snapshots S1 and S2 of the file differ.
The file is stored in `snapshot-timemachine-original-file'."
  (unless (string-empty-p
           (shell-command-to-string
            (format "diff -q \"%s\" \"%s\""
                    (snapshot-timemachine-path-in-snapshot
                     snapshot-timemachine-original-file s1
                     snapshot-timemachine-snapshot-dir)
                    (snapshot-timemachine-path-in-snapshot
                     snapshot-timemachine-original-file s2
                     snapshot-timemachine-snapshot-dir))))
    t))

(defun snapshot-timemachine-show-next-interesting-snapshot ()
  "Show the next snapshot in time that differs from the current one."
  (interactive)
  (if (zipper-at-end snapshot-timemachine-buffer-snapshots)
      (message "Last snapshot")
    (let* ((current-snapshot
            (zipper-focus snapshot-timemachine-buffer-snapshots))
           (z* (zipper-shift-forwards-to
                snapshot-timemachine-buffer-snapshots
                (lambda (s)
                  (snapshot-timemachine-snapshots-differ
                   s current-snapshot)))))
      (if (null z*)
          (message "No next differing snapshot found.")
        (setq snapshot-timemachine-buffer-snapshots z*)
        (snapshot-timemachine-show-focused-snapshot)))))

(defun snapshot-timemachine-show-prev-interesting-snapshot ()
  "Show the previous snapshot in time that differs from the current one."
  (interactive)
  (if (zipper-at-start snapshot-timemachine-buffer-snapshots)
      (message "First snapshot")
    (let* ((current-snapshot
            (zipper-focus snapshot-timemachine-buffer-snapshots))
           (z* (zipper-shift-backwards-to
                snapshot-timemachine-buffer-snapshots
                (lambda (s)
                  (snapshot-timemachine-snapshots-differ
                   s current-snapshot)))))
      (if (null z*)
          (message "No previous differing snapshot found.")
        (setq snapshot-timemachine-buffer-snapshots z*)
        (snapshot-timemachine-show-focused-snapshot)))))

(defun snapshot-timemachine-show-timeline ()
  "Display the snapshot timeline of the given file.
Leaves the point on the line of the snapshot that was active in
the time machine."
  (interactive)
  (let ((focused-snapshot-id
         (snapshot-id (zipper-focus snapshot-timemachine-buffer-snapshots))))
    (with-current-buffer
        (snapshot-timeline-create
         snapshot-timemachine-original-file
         (zipper-to-list snapshot-timemachine-buffer-snapshots)
         snapshot-timemachine-snapshot-dir)
      ;; Go to the snapshot that was active in the timemachine
      (cl-loop for pos = (point-min)
               then (progn (forward-line) (point))
               while (< pos (point-max))
               until (= focused-snapshot-id (tabulated-list-get-id pos))))))


(defun snapshot-timemachine-quit ()
  "Exit the timemachine."
  (interactive)
  (kill-buffer))

;;; Minor-mode for snapshots

(define-minor-mode snapshot-timemachine-mode
  "Step through snapshots of files."
  :init-value nil
  :lighter " Timemachine"
  :keymap
  '(("n" . snapshot-timemachine-show-next-snapshot)
    ("p" . snapshot-timemachine-show-prev-snapshot)
    ("N" . snapshot-timemachine-show-next-interesting-snapshot)
    ("P" . snapshot-timemachine-show-prev-interesting-snapshot)
    ("<" . snapshot-timemachine-show-first-snapshot)
    (">" . snapshot-timemachine-show-last-snapshot)
    ("j" . snapshot-timemachine-show-nth-snapshot)
    ("t" . snapshot-timemachine-show-timeline)
    ("l" . snapshot-timemachine-show-timeline)
    ("q" . snapshot-timemachine-quit))
  :group 'snapshot-timemachine)


;;; Launcher helper function and macro

(defun snapshot-validate (file fn)
  "Call FN with the snaphot directory and snapshots of FILE.
FN must be a function expecting the snapshot directory as first
and the snapshots as second argument.  Finds the snapshot
directory with `snapshot-timemachine-find-snapshot-dir' and the
snapshots with `snapshot-timemachine-file-snapshots'.  FN is only
called when there is a snapshot directory and at least one
snapshot, otherwise the user is notified of the respective
problem."
  (let ((snapshot-dir
         (snapshot-timemachine-find-snapshot-dir default-directory)))
    (if (null snapshot-dir)
        (message "Snapshot folder not found")
      (let ((snapshots (cl-sort
                        (snapshot-timemachine-file-snapshots file snapshot-dir)
                        #'< :key #'snapshot-id)))
        (if (null snapshots)
            (message "No snapshots found")
          (funcall fn snapshot-dir snapshots))))))

(defmacro with-snapshots (file args &rest body)
  "Call `snapshot-validate' with FILE passing a lambda with ARGS and BODY.
ARGS should be a list of two arguments, the snapshot directory
will be bound to the first argument, and the snapshots will be
bound to the second argument."
  (declare (indent 2))
  `(snapshot-validate file (lambda (,@args) ,@body)))

;;; Timemachine launcher

(defun snapshot-timemachine-create (file snapshots snapshot-dir &optional id)
  "Create and return a snapshot time machine buffer.
The snapshot timemachine will be of FILE using the SNAPSHOTS
located in SNAPSHOT-DIR.  SNAPSHOTS must be a non-empty list.
The snapshot with ID is displayed unless ID is not passed (or
nil), in which case the last snapshot is displayed."
  (let* ((timemachine-buffer
          (format "snapshot:%s" (file-name-nondirectory file)))
         ;; We say is must be non-empty, so `zipper-from-list' shouldn't fail.
         (z (zipper-from-list snapshots))
         (shifted-z (when id (zipper-shift-to
                              z (lambda (s) (= (snapshot-id s) id)))))
         ;; Shifting can still fail if ID is missing
         (z* (or shifted-z (zipper-shift-end z))))
    (cl-destructuring-bind (cur-line mode)
        (with-current-buffer (find-file-noselect file t)
          (list (line-number-at-pos) major-mode))
      (with-current-buffer (get-buffer-create timemachine-buffer)
        (switch-to-buffer timemachine-buffer)
        (funcall mode)
        (setq snapshot-timemachine-original-file    file
              snapshot-timemachine-buffer-snapshots z*
              snapshot-timemachine-snapshot-dir     snapshot-dir)
        (snapshot-timemachine-show-focused-snapshot)
        (goto-char (point-min))
        (forward-line (1- cur-line))
        (snapshot-timemachine-mode)))))

;;;###autoload
(defun snapshot-timemachine (&optional file)
  "Start the snapshot timemachine for FILE.
FILE defaults to the file the current buffer is visiting."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-snapshots file (snapshot-dir snapshots)
      (snapshot-timemachine-create file snapshots snapshot-dir))))


;;; Interactive timeline functions and their helpers

(defun snapshot-timeline-diffstat (file1 file2)
  "Calculate a diffstat between FILE1 and FILE2.
The result is cons cell (ADDED . REMOVED) of the number of lines
added and the number of lines removed going from FILE1 to FILE2.
Return nil when one of the two files is missing (or nil)."
  (when (and file1 file2 (file-exists-p file1) (file-exists-p file2))
    (let ((diff-output
           (shell-command-to-string
            (format "diff %s %s %s \"%s\" \"%s\""
                    "--old-line-format='-'"
                    "--new-line-format='+'"
                    "--unchanged-line-format=''"
                    file1 file2))))
      (cl-loop for c across diff-output
               count (eq c ?+) into p
               count (eq c ?-) into m
               finally return (cons p m)))))

(defun snapshot-timeline-format-diffstat (diffstat &optional width)
  "Format DIFFSTAT as plus and minus signs with a maximum width of WIDTH.
WIDTH defaults to 64 characters.  When there DIFFSTAT is nil
or (0 . 0), an empty string is returned.  Otherwise, a string
consisting a plus sign (with face `diff-added') for each added
line and a minus sign (with face `diff-removed') for each removed
line.  If the total number of signs would exceed WIDTH, the
number of plus and minus sign is relative to WIDTH."
  (destructuring-bind (pluses . minuses) diffstat
    (let ((width (or width 64))
          (total (+ pluses minuses)))
      (when (> total width)
        (setq pluses (round (* width (/ pluses (float total))))
              minuses (- width pluses)))
      (concat (propertize (make-string pluses ?+)
                          'face 'diff-added)
              (propertize (make-string minuses ?-)
                          'face 'diff-removed)))))

(defun snapshot-timeline-add-diffstats (snapshots)
  "Add a diffstat with the previous snapshot to each snapshot of SNAPSHOTS.
The diffstat is stored as a cons (ADDED . REMOVED) in the
`diffstat' field of the snapshot structs.  If the `diffstat'
already contains a diffstat, it is not recalculated.  Modify the
SNAPSHOTS in-place and return them."
  (cl-labels ((diffstat (s1 s2)
                        (snapshot-timeline-diffstat
                         (snapshot-timemachine-path-in-snapshot
                          snapshot-timemachine-original-file s1
                          snapshot-timemachine-snapshot-dir)
                         (snapshot-timemachine-path-in-snapshot
                          snapshot-timemachine-original-file s2
                          snapshot-timemachine-snapshot-dir))))
    (unless (cl-some (lambda (s) (consp (snapshot-diffstat s))) snapshots)
      (cl-loop
       for s in snapshots and s-prev in (cons nil snapshots)
       for diffstat = (when s-prev (message "DIFFSTAT") (diffstat s-prev s))
       do (setf (snapshot-diffstat s) diffstat)))
    snapshots))

(defun snapshot-timeline-interesting-diffstatp (diffstat)
  "Return t when the given DIFFSTAT (format: (ADDED . REMOVED)) is interesting.
A diffstat is interesting when it is not nil and both ADDED and
REMOVED are greater than zero."
  (and diffstat (< 0 (car diffstat)) (< 0 (cdr diffstat))))

;; TODO include current version of file
(defun snapshot-timeline-format-snapshots (snapshots &optional interesting-only)
  "Format SNAPSHOTS to be used as `tabulated-list-entries'.
An entry consists of the snapshot's name, its date and a diffstat
with the previous snapshot.  If INTERESTING-ONLY is non-nil, only
snapshots in which the file was changed are returned."
  (cl-loop
   for s in snapshots
   for diffstat = (snapshot-diffstat s)
   unless (and interesting-only
               (not (snapshot-timeline-interesting-diffstatp diffstat)))
   collect (list (snapshot-id s)
                 (vector
                  (format "%5s"
                          ;; We do it like this because we don't want the padding
                          ;; spaces to be underlined
                          (propertize (number-to-string (snapshot-id s))
                                      'face 'button))
                  (format-time-string
                   snapshot-timemachine-time-format
                   (snapshot-date s))
                  (if diffstat
                      (snapshot-timeline-format-diffstat diffstat 40)
                    "")))))

(defun snapshot-timeline-toggle-interesting-only ()
  "Toggle between showing all and only interesting snapshots.
A snapshot is interesting when it differs from the previous
snapshot."
  (interactive)
  ;; When there are as many entries shown as there are snapshots, we assume
  ;; we're displaying all entries, so switch to interesting-only, and vice
  ;; versa.  The condition can also be true when all snapshots are
  ;; interesting, in which case toggling doesn't make sense.
  (setq tabulated-list-entries
        (snapshot-timeline-format-snapshots
         snapshot-timemachine-buffer-snapshots
         (= (length tabulated-list-entries)
            (length snapshot-timemachine-buffer-snapshots))))
        (tabulated-list-print t))

(defun snapshot-timeline-show-snapshot ()
  "Show the snapshot under the point in the snapshot time machine."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (null id)
        (message "Not on a snapshot"))
    (snapshot-timemachine-create
     snapshot-timemachine-original-file
     snapshot-timemachine-buffer-snapshots
     snapshot-timemachine-snapshot-dir
     id)))

(defun snapshot-timeline-show-diff ()
  "TODO"
  (interactive)
  nil)

(defun snapshot-timeline-show-snapshot-or-diff ()
  "Show the snapshot under the point or the diff, depending on the column.
If the point is located in the Diffstat column, a diff with the
previous snapshot is shown (`snapshot-timeline-show-diff'),
otherwise the snapshot of the file is
shown (`snapshot-timeline-show-snapshot-or-diff')."
  (interactive)
  (if (equal "Diffstat"
             (get-text-property (point) 'tabulated-list-column-name))
      (snapshot-timeline-show-diff)
    (snapshot-timeline-show-snapshot)))

(defun snapshot-timeline-unmark (c)
  "Remove all tags equal to character C from the time line."
  (save-excursion
    (cl-loop for pos = (progn (goto-char (point-min)) (point))
             then (progn (forward-line) (point))
             while (< pos (point-max))
             if (eq c (char-after pos))
             do (progn (goto-char pos)
                       (tabulated-list-put-tag "")))))

(defun snapshot-timeline-mark-as-A ()
  "Mark a snapshot to use as file A of a diff."
  (interactive)
  (snapshot-timeline-unmark ?A)
  (tabulated-list-put-tag "A"))

(defun snapshot-timeline-mark-as-B ()
  "Mark a snapshot to use as file B of a diff."
  (interactive)
  (snapshot-timeline-unmark ?B)
  (tabulated-list-put-tag "B"))

(defun snapshot-timeline-goto-start ()
  "Go to the first snapshot in the time line.
The first snapshot in the time line is not always chronologically
the first snapshot, for example when the order is reversed."
  (interactive)
  (goto-char (point-min)))

(defun snapshot-timeline-goto-end ()
  "Go to the last snapshot in the time line.
The last snapshot in the time line is not always chronologically
the last snapshot, for example when the order is reversed."
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

;;; Minor-mode for timeline

(defvar snapshot-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'snapshot-timeline-show-snapshot-or-diff)
    (define-key map (kbd "v")   'snapshot-timeline-show-snapshot)
    (define-key map (kbd "i")   'snapshot-timeline-toggle-interesting-only)
    (define-key map (kbd "a")   'snapshot-timeline-mark-as-A)
    (define-key map (kbd "b")   'snapshot-timeline-mark-as-B)
    (define-key map (kbd "<")   'snapshot-timeline-goto-start)
    (define-key map (kbd ">")   'snapshot-timeline-goto-end)
    map)
  "Local keymap for `snapshot-timeline-mode' buffers.")

(define-derived-mode snapshot-timeline-mode tabulated-list-mode
  "Snapshot Timeline"
  "Display a timeline of snapshots of a file."
  :group 'snapshot-timemachine
  (let ((time-width (length
                     (format-time-string
                      snapshot-timemachine-time-format '(0 0 0 0)))))
    (setq tabulated-list-padding 1
          tabulated-list-format
          ;; TODO make widths configurable
          `[("Snapshot" 8 t)
            ("Time" ,time-width nil) ;; TODO make sortable
            ("Diffstat" 40 nil)])
    (tabulated-list-init-header)))

;;; Timeline launcher

(defun snapshot-timeline-create (file snapshots snapshot-dir)
  "Create and return a snapshot timeline buffer.
The snapshot timeline will be of FILE using the SNAPSHOTS located
in SNAPSHOT-DIR."
  (let ((timeline-buffer
         (format "timeline:%s" (file-name-nondirectory file))))
    (with-current-buffer (get-buffer-create timeline-buffer)
      (snapshot-timeline-mode)
      (setq snapshot-timemachine-original-file file
            snapshot-timemachine-snapshot-dir  snapshot-dir
            snapshot-timemachine-buffer-snapshots
            (snapshot-timeline-add-diffstats
             snapshots)
            tabulated-list-entries
            (snapshot-timeline-format-snapshots
             snapshot-timemachine-buffer-snapshots))
      (tabulated-list-print)
      (hl-line-mode 1)
      (switch-to-buffer timeline-buffer))))

(defun snapshot-timeline (&optional file)
  "Display a timeline of snapshots of FILE.
FILE defaults to the file the current buffer is visiting."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-snapshots file (snapshot-dir snapshots)
      (snapshot-timeline-create file snapshots snapshot-dir))))


(provide 'snapshot-timemachine)
;;; snapshot-timemachine.el ends here
