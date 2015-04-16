# snapshot-timemachine

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [snapshot-timemachine](#snapshot-timemachine)
    - [Timemachine](#timemachine)
    - [Timeline](#timeline)
    - [Installation](#installation)
    - [Customisation](#customisation)
    - [Your own snapshot system](#your-own-snapshot-system)
    - [Licence](#licence)

<!-- markdown-toc end -->


Snapshot-timemachine provides a polished interface to step through the
snapshots of a file made by a third-party snapshot or backup facility, e.g.
Btrfs, ZFS, etc.

Out of the box, it can detect snapshots made by [Snapper], but it provides a
simple interface to add support for other snapshot facilities.

[Snapper]: http://snapper.io

It provides two views: the [timemachine](#timemachine) and the
[timeline](#timeline).

## Timemachine

Invoke with `M-x snapshot-timemachine`. Inspired by [git-timemachine][]. Opens
a new buffer viewing the current snapshot of the visited file. You can easily
go back and forth in time and view the state of the file in the snapshots you
made.

[git-timemachine]: https://github.com/pidu/git-timemachine

![snapshot-timemachine](../img/timemachine.gif?raw=true)


Available bindings:

* `n` Show the next snapshot
* `p` Show the previous snapshot
* `N` Show the next snapshot that differs from the current one
* `P` Show the previous snapshot that differs from the current one
* `<` Show the first recorded snapshot
* `>` Show the last (current) recorded snapshot
* `j` Pick a snapshot to show
* `t` or `l` Activate the [timeline](#timeline)
* `q` Quit
* `r` Restore the file to the shown snapshot
* `s` Save the shown snapshot as a new file

## Timeline

Invoke with `M-x snapshot-timeline`. Opens a new buffer listing all the
snapshots made of the visited file. The Diffstat column indicates the changes
between snapshots. You can step through the list, view snapshots, open diffs,
revert a snapshot, etc. You can mark two snapshots as A and B to quickly
compare them or to start an `ediff` or `emerge` session using them.

![snapshot-timeline](../img/timeline.gif?raw=true)

Available bindings:

* `RET` Show the selected snapshot in the [timemachine](#timemachine) or the
  diff when the point is on a diffstat
* `SPC` or `v` Show the selected snapshot in the [timemachine](#timemachine)
  in another window
* `=` Show the diff between the previous snapshot and the selected one
* `r` Restore the file to the shown snapshot
* `q` Quit
* `n` Show the next snapshot
* `p` Show the previous snapshot
* `N` Show the next snapshot that differs from the current one
* `P` Show the previous snapshot that differs from the current one
* `<` Show the first recorded snapshot
* `>` Show the last (current) recorded snapshot
* `i` Only show snapshots with changes (toggle)
* `a` Mark the selected snapshot as A
* `b` Mark the selected snapshot as B
* `u` Unmark the current snapshot
* `U` Unmark all snapshots
* `d` Show the diff between snapshots A and B
* `e` Start an `ediff` session with snapshots A and B
* `m` Start an `emerge` session with snapshots A and B

## Installation


* Manual install:

        (add-to-list 'load-path "/path/to/snapshot-timemachine")
        (require 'snapshot-timemachine)

## Customisation

There are a couple of options that can be tweaked:

* `snapshot-timemachine-time-format` (default: `"%a %d %b %Y %R"`) the format
  used to display the dates.
* `snapshot-timemachine-diff-switches` (default: `"-u"`) the switches to pass
  to `diff` when calculating a diff between snapshots.
* `snapshot-timemachine-include-current` (default: `t`) include the current
  state of the file in the timeline and timemachine.
* `snapshot-timemachine-sync-with-timeline` (default: `t`) when scrolling
  through the timeline, show the selected snapshot in the timemachine when
  active. If for some reason loading a snapshot takes a while (e.g. stored on
  remote storage), setting this to `nil` will make moving around in the
  timeline more responsive.


## Your own snapshot system

To use `snapshot-timemachine` for a snapshot system other than [Snapper], you
have to define your own function to find the snapshots.

Given an absolute path to a file as only argument, the function must return a
list of `snapshot` structs of the existing snapshots of the file. When
`snapshot-timemachine-include-current` is `t`, the current version of the file
will be added to this list for you, so you don't have to include it.

The `snapshot` struct has the following slots:

* `id` An ascending numerical identifier for internal lookups. Users will not
  see this, so you can just generate this.

* `name` The name of the snapshot that will be displayed in the timemachine
  and the timeline.

* `file` The absolute path to the snapshotted file, e.g.
  `"/home/.snapshots/2/snapshot/thomas/.emacs.d/init.el"`.

* `date` The date/time at which the snapshot was made, format:
  `(HIGH LOW USEC PSEC)`, a standard Emacs time object.

* `diffstat` The number of lines added/removed compared to the previous
  snapshot, format: `(ADDED . REMOVED)`. You can leave this `nil`, it will be
  calculated for you.

When you have defined your function, store it in the variable
`snapshot-timemachine-snapshot-finder`. That's it!

You can have a look at `snapshot-timemachine-snapper-snapshot-finder` for
inspiration.

## Licence

Distributed under the [GNU General Public License](LICENSE).
