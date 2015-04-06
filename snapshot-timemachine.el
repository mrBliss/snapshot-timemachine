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
;; * let (S-)n/p in timeline behave like in magit-log (maybe an option?)
;; * sync next/previous between timemachine and timeline
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

(defvar snapshot-timemachine-diff-switches "-u"
  "The switches to pass to diff when viewing a diff between
snapshots of a file.  See `diff-switches'.")

(defvar snapshot-timemachine-include-current t
  "Include the current version of the files when stepping through
  the snapshots.")

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
  (cl-loop for z* = (zipper-shift-next z) then (zipper-shift-next z*)
           if (funcall predicate (zipper-focus z*))
           return z*
           until (zipper-at-end z*)))

(defun zipper-shift-backwards-to (z predicate)
  "Shift the zipper Z backwards to an element satisfying PREDICATE.
Returns nil when no element satisfies PREDICATE."
  (cl-loop for z* = (zipper-shift-prev z) then (zipper-shift-prev z*)
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

;; A struct representing a snapshot.
(cl-defstruct snapshot
  ;; An ascending numerical identifier for lookup and sorting
  id
  ;; The name of the buffer to display
  name
  ;; The path to the snapshot directory, e.g. /home/.snapshots/2/snapshot/
  path
  ;; The date of the snapshot, format: (HIGH LOW USEC PSEC)
  date
  ;; The number of lines added/removed compared to the previous snapshot,
  ;; format: (ADDED . REMOVED). Can be nil when uninitialised.
  diffstat
  )

(defun snapshot-file (s)
  "Return the full filename of `snapshot-timemachine-original-file' in snapshot S.
Both `snapshot-timemachine-original-file' and
`snapshot-timemachine-snapshot-dir' must be set."
  (snapshot-timemachine-path-in-snapshot
   snapshot-timemachine-original-file s
   snapshot-timemachine-snapshot-dir))

(defun snapshot-timemachine-interesting-diffstatp (diffstat)
  "Return t when the given DIFFSTAT (format: (ADDED . REMOVED)) is interesting.
A diffstat is interesting when it is not nil and ADDED or REMOVED
is greater than zero."
  (and diffstat
       (or (< 0 (car diffstat))
           (< 0 (cdr diffstat)))))

(defun snapshot-interestingp (s)
  "Return t when snapshot S's diffstat is interesting.
See `snapshot-timemachine-interesting-diffstatp' to know what
'interesting' means in this context."
  (snapshot-timemachine-interesting-diffstatp (snapshot-diffstat s)))

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
                    :name filename
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
Snapshots in which FILE doesn't exist are discarded.  Includes
the current file when `snapshot-timemachine-include-current' is
non-nil."
  (let ((snapshots
         (cl-loop for snapshot in (snapshot-timemachine-find-snapshots
                                   snapshot-dir)
                  for path-in-snapshot = (snapshot-timemachine-path-in-snapshot
                                          file snapshot snapshot-dir)
                  when (file-exists-p path-in-snapshot)
                  collect snapshot)))
    (if snapshot-timemachine-include-current
        (nconc snapshots
               (list (make-snapshot
                      :id most-positive-fixnum
                      :name "current"
                      :path (file-name-directory
                             (directory-file-name snapshot-dir))
                      :date (nth 5 (file-attributes file)))))
      snapshots)))

;;; Interactive timemachine functions and their helpers

(defun snapshot-timemachine-show-focused-snapshot ()
  "Display the currently focused snapshot in the buffer.
The current snapshot is stored in
`snapshot-timemachine-buffer-snapshots'."
  (let* ((snapshot (zipper-focus snapshot-timemachine-buffer-snapshots))
         (file (snapshot-file snapshot))
         (time (format-time-string
                snapshot-timemachine-time-format
                (snapshot-date snapshot))))
    (setq buffer-read-only nil)
    (insert-file-contents file nil nil nil t)
    (setq buffer-read-only t
          buffer-file-name file
          default-directory (file-name-directory file))
    (set-buffer-modified-p nil)
    (setq mode-line-buffer-identification
          (list (propertized-buffer-identification "%12b") "@"
                (propertize
                 (snapshot-name snapshot)
                 'face 'bold)
                " " time))
    (message "Snapshot %s from %s"
             (snapshot-name snapshot) time)))

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
                     (format "Snapshot %s from %s"
                             (snapshot-name snapshot)
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
                   (= (snapshot-id s) choice)))))
        (when z*
          (setq snapshot-timemachine-buffer-snapshots z*)
          (snapshot-timemachine-show-focused-snapshot))))))

(defun snapshot-timemachine-show-next-interesting-snapshot ()
  "Show the next snapshot in time that differs from the current one."
  (interactive)
  (if (zipper-at-end snapshot-timemachine-buffer-snapshots)
      (message "Last snapshot")
    (let ((z* (zipper-shift-forwards-to
               snapshot-timemachine-buffer-snapshots
               #'snapshot-interestingp)))
      (if (null z*)
          (message "No next differing snapshot found.")
        (setq snapshot-timemachine-buffer-snapshots z*)
        (snapshot-timemachine-show-focused-snapshot)))))

(defun snapshot-timemachine-show-prev-interesting-snapshot ()
  "Show the previous snapshot in time that differs from the current one."
  (interactive)
  (if (zipper-at-start snapshot-timemachine-buffer-snapshots)
      (message "First snapshot")
    (let ((z* (zipper-shift-backwards-to
               snapshot-timemachine-buffer-snapshots
               #'snapshot-interestingp)))
      (if (null z*)
          (message "No previous differing snapshot found.")
        (setq snapshot-timemachine-buffer-snapshots z*)
        (snapshot-timemachine-show-focused-snapshot)))))

(defun snapshot-timemachine-show-timeline ()
  "Display the snapshot time line of the given file.
Leaves the point on the line of the snapshot that was active in
the time machine."
  (interactive)
  (let ((focused-snapshot-id
         (snapshot-id (zipper-focus snapshot-timemachine-buffer-snapshots))))
    (with-current-buffer
        ;; TODO add function that finds the matching timeline buffer (and vice
        ;; versa)
        (or (switch-to-buffer
             (get-buffer (format "timeline:%s"
                                 (file-name-nondirectory (buffer-file-name)))))
            (snapshot-timeline-create
             snapshot-timemachine-original-file
             (zipper-to-list snapshot-timemachine-buffer-snapshots)
             snapshot-timemachine-snapshot-dir))
      ;; Go to the snapshot that was active in the timemachine
      (cl-loop for pos = (progn (goto-char (point-min)) (point-min))
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
  `(snapshot-validate file (lambda ,args ,@body)))

;;; Timemachine launcher

(defun snapshot-timemachine-add-diffstats (snapshots)
  "Add a diffstat with the previous snapshot to each snapshot of SNAPSHOTS.
The diffstat is stored as a cons (ADDED . REMOVED) in the
`diffstat' field of the snapshot structs.  If the `diffstat'
already contains a diffstat, it is not recalculated.  Modify the
SNAPSHOTS in-place and return them."
  (cl-labels ((diffstat (s1 s2)
                        (snapshot-timeline-diffstat
                         (snapshot-file s1) (snapshot-file s2))))
    (unless (cl-some (lambda (s) (consp (snapshot-diffstat s))) snapshots)
      (cl-loop
       for s in snapshots and s-prev in (cons nil snapshots)
       for diffstat = (when s-prev (diffstat s-prev s))
       do (setf (snapshot-diffstat s) diffstat)))
    snapshots))

(defun snapshot-timemachine-create (file snapshots snapshot-dir &optional id)
  "Create and return a snapshot time machine buffer.
The snapshot timemachine will be of FILE using the SNAPSHOTS
located in SNAPSHOT-DIR.  SNAPSHOTS must be a non-empty list.
The snapshot with ID is displayed unless ID is not passed (or
nil), in which case the last snapshot is displayed.  Return the
created buffer."
  (let* ((timemachine-buffer
          (format "snapshot:%s" (file-name-nondirectory file))))
    (cl-destructuring-bind (cur-line mode)
        (with-current-buffer (find-file-noselect file t)
          (list (line-number-at-pos) major-mode))
      (with-current-buffer (get-buffer-create timemachine-buffer)
        (funcall mode)
        (setq snapshot-timemachine-original-file file
              snapshot-timemachine-snapshot-dir  snapshot-dir
              ;; `snapshot-timemachine-add-diffstats' needs the above two
              ;; buffer-local variables.
              snapshot-timemachine-buffer-snapshots
              ;; We say is must be non-empty, so `zipper-from-list' shouldn't
              ;; fail.
              (let* ((z (zipper-from-list
                         (snapshot-timemachine-add-diffstats
                          snapshots)))
                     (shifted-z
                      (when id (zipper-shift-to
                                z (lambda (s) (= (snapshot-id s) id)))))
                     ;; Shifting can still fail if ID is missing
                     (z* (or shifted-z (zipper-shift-end z))))
                z*))
        (snapshot-timemachine-show-focused-snapshot)
        (goto-char (point-min))
        (forward-line (1- cur-line))
        (snapshot-timemachine-mode)
        (current-buffer)))))

;;;###autoload
(defun snapshot-timemachine (&optional file)
  "Start the snapshot timemachine for FILE.
FILE defaults to the file the current buffer is visiting."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-snapshots file (snapshot-dir snapshots)
      (switch-to-buffer
       (snapshot-timemachine-create file snapshots snapshot-dir)))))

;;; Interactive time line functions and their helpers

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

;; TODO include current version of file
(defun snapshot-timeline-format-snapshots (snapshots &optional interesting-only)
  "Format SNAPSHOTS to be used as `tabulated-list-entries'.
An entry consists of the snapshot's name, its date and a diffstat
with the previous snapshot.  If INTERESTING-ONLY is non-nil, only
snapshots in which the file was changed are returned."
  (cl-loop
   for s in snapshots
   for diffstat = (snapshot-diffstat s)
   unless (and interesting-only (not (snapshot-interestingp s)))
   collect (list (snapshot-id s)
                 (vector
                  (format "%5s" ;; TODO configurable
                          ;; We do it like this because we don't want the padding
                          ;; spaces to be underlined
                          (propertize (snapshot-name s)
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

(defun snapshot-timeline-show-snapshot ()
  "Show the snapshot under the point in the snapshot time machine.
Open the time machine buffer in the same window."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (null id)
        (message "Not on a snapshot"))
    (switch-to-buffer
     (snapshot-timemachine-create
      snapshot-timemachine-original-file
      snapshot-timemachine-buffer-snapshots
      snapshot-timemachine-snapshot-dir
      id))))

(defun snapshot-timeline-view-snapshot ()
  "Show the snapshot under the point in the snapshot time machine.
Open the time machine buffer in another window and leave the
timeline window focused."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (null id)
        (message "Not on a snapshot"))
    ;; TODO other window is focused
    (switch-to-buffer-other-window
     (snapshot-timemachine-create
        snapshot-timemachine-original-file
        snapshot-timemachine-buffer-snapshots
        snapshot-timemachine-snapshot-dir
        id) t)))

(defun snapshot-timeline-show-diff ()
  "Show the diff between this snapshot and the previous one.
When there is no previous snapshot or there are no changes, a
message will tell the user so."
  (interactive)
  (let* ((id1 (save-excursion (forward-line -1) (tabulated-list-get-id)))
         (id2 (tabulated-list-get-id))
         (s1 (snapshot-timeline-snapshot-by-id id1))
         (s2 (snapshot-timeline-snapshot-by-id id2)))
    (cond ((or (null s1) (null s2))
           (message "No diff here"))
          ((not (snapshot-interestingp s2))
           (message "No changes between snapshots"))
          (t (snapshot-timeline-show-diff-between s1 s2)))))

(defun snapshot-timeline-snapshot-by-id (id)
  "Return the snapshot in `snapshot-timemachine-buffer-snapshots' with ID.
Return nil when no snapshot matches the ID."
  (car (cl-member id snapshot-timemachine-buffer-snapshots
                  :key #'snapshot-id)))

(defun snapshot-timeline-get-A-and-B ()
  "Return a cons cell of the ids of the marked snapshots.
Format: (A . B) where A is an int or nil when it's not set, idem
for B."
  (let (a b)
    (save-excursion
      (cl-loop for pos = (progn (goto-char (point-min)) (point))
               then (progn (forward-line) (point))
               while (< pos (point-max))
               if (eq ?A (char-after pos))
               do (setq a (tabulated-list-get-id))
               if (eq ?B (char-after pos))
               do (setq b (tabulated-list-get-id))
               finally return (cons a b)))))

(defun snapshot-timeline-show-diff-between (s1 s2)
  "Show the diff between snapshots S1 and S2."
  (diff (snapshot-file s1) (snapshot-file s2)
        snapshot-timemachine-diff-switches))

(defun snapshot-timeline-validate-A-B (fn)
  "Check that A and B are marked, then call FN with the corresponding snapshots.
The user is informed of missing marks.  FN must accept two
arguments, the snapshots on which the A and B marks are placed."
  (destructuring-bind (a . b) (snapshot-timeline-get-A-and-B)
    (if (or (null a) (null b))
        (message "Please mark both A and B.")
      (funcall fn
               (snapshot-timeline-snapshot-by-id a)
               (snapshot-timeline-snapshot-by-id b)))))

(defmacro with-A-B (args &rest body)
  "Call `snapshot-timeline-validate-A-B' passing a lambda with ARGS and BODY.
ARGS should be a list of two arguments, snapshots indicated by
marks A and B will be bound to them."
  (declare (indent 1))
  `(snapshot-timeline-validate-A-B (lambda ,args ,@body)))

(defun snapshot-timeline-show-diff-A-B ()
  "Show the diff between the snapshots marked as A and B.
The user is informed of missing marks."
  (interactive)
  (with-A-B (a b) (snapshot-timeline-show-diff-between a b)))

(defun snapshot-timeline-ediff-A-B ()
  "Start an ediff session between the snapshots marked as A and B.
The user is informed of missing marks."
  (interactive)
  (with-A-B (a b) (ediff (snapshot-file a) (snapshot-file b))))

(defun snapshot-timeline-emerge-A-B ()
  "Start an emerge session between the snapshots marked as A and B.
The user is informed of missing marks."
  (interactive)
  (with-A-B (a b) (emerge-files nil (snapshot-file a) (snapshot-file b) nil)))

(defun snapshot-timeline-mark-as-A ()
  "Mark a snapshot to use as file A of a diff."
  (interactive)
  (snapshot-timeline-unmark-all ?A)
  (tabulated-list-put-tag "A" t))

(defun snapshot-timeline-mark-as-B ()
  "Mark a snapshot to use as file B of a diff."
  (interactive)
  (snapshot-timeline-unmark-all ?B)
  (tabulated-list-put-tag "B" t))

(defun snapshot-timeline-unmark ()
  "Remove the mark on the current line."
  (interactive)
  (tabulated-list-put-tag "" t))

(defun snapshot-timeline-unmark-all (&optional c)
  "Remove all marks (equal to C when passed) from the time line.
When C is passed and non-nil, only marks matching C are removed,
otherwise all marks are passed."
  (interactive)
  (save-excursion
    (cl-loop for pos = (progn (goto-char (point-min)) (point))
             then (progn (forward-line) (point))
             while (< pos (point-max))
             if (or (null c) (eq c (char-after pos)))
             do (progn (goto-char pos)
                       (tabulated-list-put-tag "")))))

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

(defun snapshot-timeline-goto-next-interesting-snapshot ()
  "Go to the next snapshot in the time line that differs from the current one."
  (interactive)
  (cl-loop for pos = (progn (forward-line) (point))
           while (< pos (point-max))
           for id = (tabulated-list-get-id)
           for s = (snapshot-timeline-snapshot-by-id id)
           until (and s (snapshot-interestingp s))))

(defun snapshot-timeline-goto-prev-interesting-snapshot ()
  "Go to the previous snapshot in the time line that differs from the current one."
  (interactive)
  (cl-loop for pos = (progn (forward-line -1) (point))
           while (< (point-min) pos)
           for id = (tabulated-list-get-id)
           for s = (snapshot-timeline-snapshot-by-id id)
           until (and s (snapshot-interestingp s))))

;;; Minor-mode for time line

(defvar snapshot-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'snapshot-timeline-show-snapshot-or-diff)
    (define-key map (kbd "a")   'snapshot-timeline-mark-as-A)
    (define-key map (kbd "b")   'snapshot-timeline-mark-as-B)
    (define-key map (kbd "d")   'snapshot-timeline-show-diff-A-B)
    (define-key map (kbd "e")   'snapshot-timeline-ediff-A-B)
    (define-key map (kbd "i")   'snapshot-timeline-toggle-interesting-only)
    (define-key map (kbd "m")   'snapshot-timeline-emerge-A-B)
    (define-key map (kbd "N")   'snapshot-timeline-goto-next-interesting-snapshot)
    (define-key map (kbd "P")   'snapshot-timeline-goto-prev-interesting-snapshot)
    (define-key map (kbd "u")   'snapshot-timeline-unmark)
    (define-key map (kbd "U")   'snapshot-timeline-unmark-all)
    (define-key map (kbd "v")   'snapshot-timeline-view-snapshot)
    (define-key map (kbd "<")   'snapshot-timeline-goto-start)
    (define-key map (kbd ">")   'snapshot-timeline-goto-end)
    (define-key map (kbd "=")   'snapshot-timeline-show-diff)
    map)
  "Local keymap for `snapshot-timeline-mode' buffers.")

(define-derived-mode snapshot-timeline-mode tabulated-list-mode
  "Snapshot Timeline"
  "Display a time line of snapshots of a file."
  :group 'snapshot-timemachine
  ;; TODO revert-buffer
  (let ((time-width (length
                     (format-time-string
                      snapshot-timemachine-time-format '(0 0 0 0)))))
    (setq tabulated-list-padding 2
          tabulated-list-format
          ;; TODO make widths configurable
          `[("Snapshot" 8 t)
            ("Time" ,time-width nil) ;; TODO make sortable
            ("Diffstat" 40 nil)])
    (tabulated-list-init-header)))

;;; Time line launcher

(defun snapshot-timeline-create (file snapshots snapshot-dir)
  "Create and return a snapshot time line buffer.
The snapshot time line will be of FILE using the SNAPSHOTS located
in SNAPSHOT-DIR."
  (let ((timeline-buffer
         (format "timeline:%s" (file-name-nondirectory file))))
    (with-current-buffer (get-buffer-create timeline-buffer)
      (snapshot-timeline-mode)
      (setq snapshot-timemachine-original-file file
            snapshot-timemachine-snapshot-dir  snapshot-dir
            snapshot-timemachine-buffer-snapshots
            (snapshot-timemachine-add-diffstats
             snapshots)
            tabulated-list-entries
            (snapshot-timeline-format-snapshots
             snapshot-timemachine-buffer-snapshots))
      (tabulated-list-print)
      (hl-line-mode 1)
      (switch-to-buffer timeline-buffer))))

(defun snapshot-timeline (&optional file)
  "Display a time line of snapshots of FILE.
FILE defaults to the file the current buffer is visiting."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-snapshots file (snapshot-dir snapshots)
      (snapshot-timeline-create file snapshots snapshot-dir))))


(provide 'snapshot-timemachine)
;;; snapshot-timemachine.el ends here
