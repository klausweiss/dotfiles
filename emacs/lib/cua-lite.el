;;; @(#) cua-lite.el -- a light-weight, extendable & toggle-able CUA emulator
;;; @(#) $Id: cua-lite.el,v 1.4 2003/08/17 03:16:09 jcasa Exp $

;; This file is not part of Emacs

;; Copyright (C) 2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         May 27, 2001
;; Keywords:        convenience emulations CUA
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  cua-lite is a CUA emulator package for Emacs.  There are three
;;  things that make this package different from the others already
;;  out there:
;;
;;  1) it is a quasi-minor mode, meaning that it can be turned on &
;;     off easily (though only globally; hence the "quasi-")
;;  2) it is easily extensible
;;  3) it does NOT attempt to override the existing use of <Ctrl><X>
;;     and <Ctrl><C> (use CUA-mode for that)

;;; --------------------------------------------------------------------
;;; Special Note for XEmacs users:
;;
;;  XEmacs uses a very different paradigm for defining what the region
;;  is, whether or not it is highlighted by default, and whether or
;;  not it remains highlighted when point is moved.  This is analagous
;;  to GNU Emacs' transient-mode, and at first glance it seems to make
;;  it much easier to implement functions which affect it.  However,
;;  it is vastly different then GNU Emacs, and would essentially
;;  require a total re-write of cua-lite to support it, or a great
;;  deal of hoop jumping, neither of which I have the time for.  So,
;;  (for the moment) cua-lite will not support XEmacs -- sorry!  If
;;  it's any consolation, XEmacs does half of what cua-lite does
;;  already, and makes a great deal of the rest of it easier to
;;  implement.
;;; --------------------------------------------------------------------

;;; Installation:
;;
;;  There are two ways to install this package.  The first is to
;;  install it and load it immediately.  This is the appropriate
;;  method if you know you want to load it all of the time.
;;
;;  The second method is to use a bootstrap package,
;;  `cua-lite-bootstrap'.  This package will set up an autoload
;;  statement for cua-lite, and (optionally) define a hotkey to toggle
;;  the package on and off.  This is to encourage people to have the
;;  package available for use by others even if they themselves do not
;;  wish to use it, while at the same time utilizing almost no
;;  resources.  See the bootstrap file for more details.
;;
;;  To load and enable each time (method #1), put this file on your
;;  Emacs-Lisp load path and add the following to your ~/.emacs
;;  startup file:
;;
;;     (require 'cua-lite)
;;     (cua-lite 1)
;;
;;  To add a directory to your load-path, use something like the following:
;;
;;      (add-to-list 'load-path (expand-file-name "/some/load/path"))

;;; Usage:
;;
;;  M-x `cua-lite'
;;     Toggles the package on and off.  With optional parameter it
;;     turns cua-lite on iff the arg is a positive integer.
;;
;;     Generally speaking, turning cua-lite on will bind certain
;;     movement commands to keys and enable some CUA-like features
;;     within Emacs (see specifics below).  Movement commands are
;;     functions that move point and movement keys are the keys bound
;;     to those commands.  Simple examples include the up arrow and
;;     the down arrow, which are bound to `previous-line' and
;;     `next-line', respectively.  What's special about them in
;;     cua-lite, though, is that when the Shift key is held down while
;;     pressing the key bound to the movement command, the current
;;     selection is extended to cover the area traversed by the
;;     movement command.  For example, say point is right here -><-
;;     and I hit the 'home' key.  Normally (at least in a CUA
;;     emulation mode) point would move to the beginning of the line,
;;     column 0.  If the Shift key were held down while I hit the
;;     'home' key, though, then everything from the current point to
;;     the beginning of the line would be selected.  Hitting a
;;     movement key while a selection is activated, but without the
;;     Shift key being pressed, will de-activate the selection.  It's
;;     all pretty standard behavior in a CUA application (though I'm
;;     not quite sure if CUA defines this behavior explicitly or not).
;;
;;     Non-movement keys bind common Emacs commands to CUA-standard
;;     keys.  Examples would be <Ctrl><S> for Save (`save-buffer') and
;;     <Ctrl><A> for Select All (`mark-whole-buffer').  None of these
;;     commands work on or are affected by the current selection,
;;     though.
;;
;;     One set of CUA keys that are explicitly NOT bound are the Cut,
;;     Copy & Paste key combinations, <Ctrl><X>, <Ctrl><C> and
;;     <Ctrl><V>, respectively.  This is because Emacs has a great
;;     deal of function bound to C-x and C-c by default, and CUA
;;     provides another mechanism for Cut, Copy & Paste
;;     (<Shift><Delete>, <Control><Insert> and <Shift><Insert>,
;;     respectively).  If that particular set of key bindings is of
;;     paramount importance, I suggest using CUA-mode
;;     (http://hjem.get2net.dk/storm/emacs/).
;;
;;     When cua-lite is enabled, the following keys are bound (some
;;     are optional):
;;
;;       Movement keys (pressing Shift with any of these extends the
;;       current selection):
;;         o <left>, <right>, <up>, <down> - standard definitions
;;         o C-<right>, C-<left> - forward & backward word
;;         o C-<up>, C-<down> - forward & backward paragraph
;;         o M-<up>, M-<down> - forward & backward paragraph
;;           see `cua-lite-use-simplified-paragraph-movement'
;;         o <prior>, <next> - page up & down (scroll down & up, respectively)
;;         o C-<prior>, C-<next> - top & bottom of page
;;           see `cua-lite-use-page-movement-keys'
;;         o <home>, <end> - beginning & end of line
;;           see `cua-lite-use-home-key-bounce' & `cua-lite-use-end-key-bounce'
;;         o C-<home>, C-<end> - beginning & end of buffer
;;
;;       Non-movement keys:
;;         o C-<backspace> - delete word backward
;;           see `cua-lite-use-backward-delete-word'
;;         o C-a - Select All (`mark-whole-buffer')
;;         o C-f - Find (`isearch-forward')
;;         o M-f - Find Backwards (`isearch-backward')
;;         o C-o - Open (`find-file')
;;         o C-r - Replace (`replace-string')
;;         o C-s - Save (`save-buffer')
;;         o M-s - Save As (`write-file')
;;         o C-w - Close (`kill-buffer' or `delete-frame')
;;           see `cua-lite-what-is-control-w'
;;         o M-<f4> - Close (`kill-buffer' or `delete-frame')
;;           see `cua-lite-what-is-alt-f4'
;;         o C-z - Undo (`undo' / `advertised-undo')
;;
;;     In addition, certain CUA-like features of Emacs are enabled.
;;     This is accomplished by modifying the values of the following
;;     variables or calling the following functions (which see):
;;         o `mark-active' - see `cua-lite-keep-current-mark'
;;         o `truncate-lines' - see `cua-lite-use-hscroll-mode'
;;         o `hscroll-global-mode' - see `cua-lite-use-hscroll-mode'
;;         o `blinking-cursor-mode' - see `cua-lite-use-blinking-cursor'
;;         o `bar-cursor-mode' - see `cua-lite-use-bar-cursor-mode'
;;         o `transient-mark-mode'
;;         o `mark-even-if-inactive' - see `cua-lite-disable-inactive-regions'
;;         o `delete-selection-mode' - see `cua-lite-use-delete-selection-mode'
;;
;;     Both of the above-mentioned keybinding concepts (movement keys
;;     and non-movement keys) can be extended to your own creations
;;     via one of four simple functions.  Using these functions to
;;     bind your keys ensures that when cua-lite is disabled, your
;;     key-bindings revert back to Emacs-normal, and when cua-lite is
;;     re-enabled, your keys come back with it.
;;
;;     I recommend that these functions be called in the hook
;;     `cua-lite-bind-keys-hook' (this hook can be customized).
;;     Please see the individual functions for more details on how
;;     they're used.  Here's an example of what I have:
;;
;;       (defun my-cua-lite-keys ()
;;         "Bunch of stuff to run for cua-lite when keys are bound."
;;         (cua-lite-bind-motion-key "C-=" 'joc-bounce-sexp nil)
;;         (cua-lite-bind-motion-key "C-+" 'joc-bounce-sexp t)
;;
;;         (if (or (eq window-system 'w32) (eq window-system win32))
;;       	  (cua-lite-bind-key-simple "C-p" 'joc-print-buffer-or-region))
;;
;;         (cua-lite-save-keystroke-for-restoration "C-<kp-enter>")
;;         (global-set-key (read-kbd-macro "C-<kp-enter>")
;;       				  '(lambda ()
;;       					 "Join current line to next line, deleting white space."
;;       					 (interactive)
;;       					 (delete-indentation 1)))
;;
;;         ;; camelCase mode - http://www.ai.mit.edu/people/caroma/tools/
;;         (cua-lite-bind-both-motion-keys "M-<right>" 'camelCase-forward-word)
;;         (cua-lite-bind-both-motion-keys "M-<left>" 'camelCase-backward-word)
;;       )
;;
;;       ;; DO NOT EDIT THESE MANUALLY
;;       (custom-set-variables
;;              ....
;;        '(cua-lite-bind-keys-hook (quote (my-cua-lite-keys)))
;;              ....
;;       )

;;; Customization:
;;
;;  There are many options to customize, too many to list here.  Each
;;  one is documented extensively, as are the groups they are in.  To
;;  customize this package do one of the following:
;;
;;     M-x customize-group cua-lite
;;
;;  or
;;
;;     M-x cua-lite-customize
;;
;;  Both of them do the same thing.

;;; Background:
;;
;;  cua-lite is yet another CUA emulator.  The two questions that beg
;;  to be answered immediately after reading that statement are:
;;
;;  1) What's CUA?
;;  2) Why does the world need another CUA emulator?
;;
;;  CUA stands for Common User Access, and it's the look-and-feel
;;  behind many common UI standards today, including Windows, Mac,
;;  Motif, Gnome, KDE and others.  One of the many radical things that
;;  the Mac gave us way back when was standardization of keystrokes
;;  for common commands like Open, Save and Print.  In the Emacs
;;  world, CUA means using keys that are familiar to many non-Emacs
;;  users, for common purposes.  So, for CUA people <Home> means
;;  beginning of line, and <Ctrl><A> means select all, while for Emacs
;;  people <Home> means beginning of buffer, and <Ctrl><A> means
;;  beginning of line.  Most Emacs users I know look down on CUA, but
;;  the lack of familiarity is what keeps most non-initiates from
;;  using Emacs, IMHO.
;;
;;  CUA also seems to imply (to many people) an alternate selection
;;  paradigm to the one that is native to Emacs.  Emacs has a
;;  point and a mark, and the way to kill and delete text (copy & cut,
;;  respectively, in CUA terms) is to set the mark, move point
;;  somewhere else, and kill or delete the text between them.  This
;;  means that you need to remember where you set mark, since by
;;  default there is no on-screen indicator.
;;
;;  Emacs natively gives you the tools to make the selection of text
;;  more CUA-like, by allowing the currently selected region to be
;;  highlighted on screen, which most CUA users will be familiar with.
;;  One of the hidden pitfalls, though, is that the region (the area
;;  between point and mark) is still defined even if it's not
;;  selected, which can lead to some strange behavior.  Fortunately,
;;  Emacs provides a way to disable this "feature", too.  Combining all
;;  of this with the ability to use the arrow keys (and other cursor
;;  movement keys) to select the text by holding down the shift key
;;  adds the final bit of CUA-like behavior to the mix.
;;
;;  So why does the world need another CUA emulator mode for Emacs?
;;  Because mine's better, of course!  Seriously though, I do believe
;;  that mine IS better, for me at least.  I've attempted to take the
;;  best of several other packages out there and roll it into
;;  something that's better than any of them individually.
;;
;;  The two major CUA emulator modes out there that I know of are
;;  CUA-mode and pc-select.  CUA-Mode is a rather heavy mode,
;;  providing many standard CUA functions and a whole lot more, and
;;  it's the one that I used for the first year-and-a-half of my Emacs
;;  experience.  They've solved the tricky problem of providing
;;  standard cut, copy & paste keys (<Ctrl><X>, <Ctrl><C> and
;;  <Ctrl><V> respectively), which is no small feat considering the
;;  amount of function bound to <Ctrl><X> and <Ctrl><C> by default.
;;  They allow both to cohabitate quite nicely, and only occasionally
;;  did I find myself trying to do something that required me to use
;;  one of their work-arounds.
;;
;;  After a while, though, I switched from CUA-mode to pc-select.  The
;;  major advantage that CUA-mode provides is the aforementioned key
;;  bindings for Cut, Copy and Paste, and I found myself seldom using
;;  them.  I normally use the alternative keystrokes for these
;;  (<Shift><Del> for cut, <Ctrl><Ins> for copy and <Shift><Ins> for
;;  paste), as they are available on nearly all of the platforms and
;;  programs I use, while the more standard CUA bindings are not.
;;  pc-select also provides a much better set of text
;;  selection/highlighting functions, which I found myself looking for
;;  eventually.  Soon thereafter, though, I started thinking about
;;  writing my own package.
;;
;;  There were two design goals driving the creation of this new
;;  package.  First, there needed to be a way to turn it off (as odd
;;  as that sounds).  I've grown tired of watching people who know
;;  Emacs sit down at my computer and proceed to select the entire
;;  buffer and delete it (you know, <Ctrl><A> to go to the beginning
;;  of the line and <Ctrl><X> as the prefix to some other command they
;;  want to run).  So, I wanted something that I can use that makes me
;;  more productive, but that I could turn off easily, so that others
;;  could sit down and help me when I needed it.  Alternately, I'd
;;  like to be able to sit down at someone else's computer and switch
;;  cua-lite on easily, do the work I need to, and then turn it back
;;  off.  To that end, it is very easy and non-intrusive to simply
;;  have cua-lite hanging around, waiting to be turned on when some
;;  fool like me sits down (see `cua-lite-bootstrap' for more
;;  details).
;;
;;  Secondly, I wanted something that was easily extendable,
;;  particularly as concerns text selection.  I'm a man who likes to
;;  tweak his editor, so I have key bindings for things that are
;;  outside the norm, and I wanted them to be able to extend the
;;  selection if I hold the shift key down, and to deactivate the
;;  selection if I don't.  Having switched from CUA-mode to pc-select
;;  helped me to implement this, but the pc-select package didn't make
;;  it easy enough IMHO.  That alone wasn't enough to write my own
;;  CUA-emulation mode, but combined with the first design goal, I now
;;  had a mission!

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Credits:
;;
;;  The selection code is conceptually based on the functions in
;;  pc-select.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of cua-lite was developed and tested with NTEmacs
;;  21.1.1 under Windows 2000 and Emacs 20.7.1 under Linux
;;  (RH7).  Please, let me know if it works with other OS and versions
;;  of Emacs.

;;; Change Log:
;;
;;  see http://www.northbound-train.com/emacs/cua-lite.log

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

(eval-when-compile
  ;; silence the old byte-compiler
  (defvar byte-compile-dynamic)
  (set (make-local-variable 'byte-compile-dynamic) t)

  ;; silence lint
  (defvar minor-mode-alist)
  (defvar transient-mark-mode)
  (defvar mark-even-if-inactive)
  (defvar this-command))

;;; **************************************************************************
;;; ***** customization
;;; **************************************************************************
(defgroup cua-lite nil
  "Emulate CUA bindings."
  :group 'editing-basics
  :group 'convenience)

;; ---------------------------------------------------------------------------
(defun cua-lite-customize ()
  "Customization of the group 'cua-lite'."
  (interactive)
  (customize-group "cua-lite"))

;; ---------------------------------------------------------------------------
(defcustom cua-lite-display-status-in-mode-line t
  "Used to show or hide mode-line indicator."
  :type 'boolean
  :group 'cua-lite)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-mode-line-string " CUA"
  "String to display in mode-line when 'cua-lite' is active."
  :type 'string
  :group 'cua-lite
  :set (lambda (symbol newval)
		 (setq cua-lite-mode-line-string newval)
		 (let ((cell (assoc 'cua-lite minor-mode-alist)))
		   (when cell
			 (setcdr cell (list cua-lite-mode-line-string))
			 (force-mode-line-update)))))

;; ---------------------------------------------------------------------------
(defcustom cua-lite-default-keybindings 3
  "Select which keys are bound when 'cua-lite' is activated.

If nil, other CUA keys will be bound -- see function `cua-lite' for
more details."
  :type '(choice (const :tag "Movement keys only" 1)
				 (const :tag "Non-movement keys only" 2)
				 (other :tag "Both movement & non-movement-keys" 3))

  :group 'cua-lite)

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
(defgroup cua-lite-basic-options nil
  "Toggle use of many basic CUA or CUA-like options.

In general, the default value reflects what I think most people would
consider as normal behavior in a CUA environment.  If the default
value is nil, the implication is that I think this is a useful
extension worthy of your consideration.  If the default is t, then
this is something I think people may have reason to want to turn off."
  :group 'cua-lite)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-ignore-key-list ()
  "List of non-movement keys to ignore when enabling cua-lite.

The value for this should be the string representation of the key to
ignore.  For example, to instruct cua-lite NOT to bind 'C-a' to
`mark-whole-buffer' you would add 'C-a' (without the quotes) to this list.

NOTE: this is for non-movement keys only (see function `cua-lite' for
more details on what is a movement key and what is a non-movement
key).  Optional movement keys are customizable individually or in
pairs, as they generally require more explanation then the
non-movement keys."
  :type '(repeat string)
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-keep-current-mark nil
  "Determines whether or not mark is cleared when 'cua-lite' is enabled.

t - current mark is kept
nil - current mark is NOT kept

If the current mark is kept when 'cua-lite' is enabled then there will
be a selection active when the mode is first enabled, which is
probably not the desired behavior.  If this optional is nil and for
whatever reason you DO wish to activate the previous mark, typing
\\[exchange-point-and-mark] twice will do the trick nicely."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-home-key-bounce nil
  "Cause 'home' to bounce between column zero and the first non-blank column.

t - use home key bounce
nil - do NOT use home key bounce

If the user presses 'home' when in any column other than zero, point
is placed in column zero.  If it's pressed when in column zero, point
moves to the first non-whitespace column (if there is one)."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-end-key-bounce nil
  "Cause 'end' to bounce between the last column and the last non-blank column.

t - use end key bounce
nil - do NOT use end key bounce

If the user presses 'end' when in any column other than the last
column in the line, point is placed in the last column.  If it's
pressed when in the last column, point moves to the last
non-whitespace column (if there is one)."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-disable-inactive-regions t
  "Deactivate the region when nothing is explicity selected/highlighted.

t - disable inactive regions
nil - do NOT disable inactive regions

See the variable `mark-even-if-inactive' for more information."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-simplified-paragraph-movement nil
  "Use simplified definitions of `forward-paragraph' and `backward-paragraph'.

t - use the simplified functions
nil - use the normal functions

The normal versions of `forward-paragraph' and `backward-paragraph'
use a mode-specific definition of what a paragraph is.  The simplified
versions just look for one or more empty lines to distinguish a
paragraph.

See also `cua-lite-rebind-normal-paragraph-movement-fns'."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-rebind-normal-paragraph-movement-fns nil
  "Bind normal (mode-specific) paragraph movement functions to alternate keys.

t - re-bind the normal functions (see below)
nil - do not re-bind the normal functions to anything

If true, re-binds `forward-paragraph' and `backward-paragraph' to
'M-up' and 'M-down', respectively.  This variable has no effect if
`cua-lite-use-simplified-paragraph-movement' is not true."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-page-movement-keys t
  "Bind 'C-prior' and 'C-next' to top- and bottom-of-page, respectively.

t - bind the keys
nil - do not bind the keys

top-of-page moves point to the top line of the current window without
scrolling.  Similarly, bottom-of-page moves point to the last line of
the current window without scrolling."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-backward-delete-word t
  "Binds 'C-backspace' to `cua-lite-backward-delete-word'.

t - use cua-backward-delete-word
nil - do not use cua-backward-delete-word

This is like `backward-kill-word' except that the word is deleted, not
killed (i.e. it is not saved to the kill-ring/clipboard)."
  :type 'boolean
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-what-is-control-w 'buffer
  "Select what (if anything) to bind 'C-w' to.

Options are:

- Delete the current buffer - deletes the current buffer; you will (of
  course) be prompted to save if needed, etc. (see `kill-buffer')

- Delete the current frame - deletes the current frame (window).
  Note: Emacs does not let you delete the last frame, so you will not
  accidently exit Emacs this way. (see `delete-frame')

- Do nothing - do not bind 'C-w' to anything.

See also: `cua-lite-what-is-alt-f4'."
  :type '(choice
		  (const :tag "Delete the current buffer (`kill-buffer')" buffer)
		  (const :tag "Delete the current frame (`delete-frame')" frame)
		  (const :tag "Do nothing" nil))
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-what-is-alt-f4 'frame
  "Select what (if anything) to bind 'M-<f4>' to.

Options are:

- Delete the current buffer - deletes the current buffer; you will (of
  course) be prompted to save if needed, etc.  (see `kill-buffer')

- Delete the current frame - deletes the current frame (window).
  Note: Emacs does not let you delete the last frame, so you will not
  accidently exit Emacs this way.  (see `delete-frame')

- Do nothing - do not bind 'M-<f4>' to anything.

See also: `cua-lite-what-is-control-w'."
  :type '(choice
		  (const :tag "Delete the current buffer (`kill-buffer')" buffer)
		  (const :tag "Delete the current frame (`delete-frame')" frame)
		  (const :tag "Do nothing" nil))
  :group 'cua-lite-basic-options)

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
(defgroup cua-lite-third-party-packages nil
  "Toggle use of third-party packages in cua-lite mode.

In general, if the package ships with Emacs it's turned on by default,
and if not, it's turned off.  Simply by being listed here, the implication
is that I recommend its use."
  :group 'cua-lite)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-pager-fns t
  "Toggles use of the 'pager' package (which-see).

Latest version is available at:

	http://www.docs.uu.se/~mic/emacs.html"
  :type 'boolean
  :group 'cua-lite-third-party-packages)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-hscroll-mode t
  "Toggles use of the 'hscroll' package (which-see).

This option is ignored in Emacs 21 (which has horizontal scrolling
built in)."
  :type 'boolean
  :group 'cua-lite-third-party-packages)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-delete-selection-mode t
  "Toggles use of the 'delsel' package (which-see)."
  :type 'boolean
  :group 'cua-lite-third-party-packages)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-blinking-cursor nil
  "Toggles use of the 'blinking-cursor' package (which-see).

Requires third-party package not normally shipped with Emacs.  It is
available at:

	http://www.wonderworks.com

This option is ignored in Emacs 21 (which has cursor blinking built
in).  The 'blinking-cursor' package provides methods for setting the
blink-rate and blink colors (which see)."
  :type 'boolean
  :group 'cua-lite-third-party-packages)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-use-bar-cursor nil
  "Toggles use of the 'bar-cursor' package (which-see).

Requires third-party package not normally shipped with Emacs.  It is
available at:

	http://www.northbound-train.com/emacs.html"
  :type 'boolean
  :group 'cua-lite-third-party-packages)

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
(defgroup cua-lite-disable-effects nil
  "Controls what happens when the mode is toggled off.

By default, most cua-lite options turn off when cua-lite does."
  :group 'cua-lite)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-pager-fns t
  "Controls whether or not 'pager' is turned off when 'cua-lite' is disabled.

t - pager is NOT turned off
nil - pager IS turned off

This variable has no effect if `cua-lite-use-pager-fns' is not true."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-hscroll-mode t
  "Controls if 'hscroll-mode' is turned off when 'cua-lite' is disabled.

t - 'hscroll-mode' is NOT turned off
nil - 'hscroll-mode' IS turned off

This variable has no effect if `cua-lite-use-hscroll-mode' is not true."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-delete-selection-mode t
  "Controls if 'delete-selection-mode' is active when 'cua-lite' is disabled.

t - 'delete-selection-mode' is NOT turned off
nil - 'delete-selection-mode' IS turned off

This variable has no effect if `cua-lite-use-delete-selection' is not true.
See command `delete-selection-mode' for more information."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-blinking-cursor nil
  "Controls if 'blinking-cursor-mode' is turned off when 'cua-lite' is disabled.

t - 'blinking-cursor-mode' is NOT turned off
nil - 'blinking-cursor-mode' IS turned off

This variable has no effect if `cua-lite-use-blinking-cursor' is not true.
See command `blinking-cursor-mode' for more information."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-bar-cursor nil
  "Controls if 'bar-cursor-mode' is turned off when 'cua-lite' is disabled.

t - 'bar-cursor-mode' is NOT turned off
nil - 'bar-cursor-mode' IS turned off

This variable has no effect if `cua-lite-use-bar-cursor' is not true.
See command `bar-cursor-mode' for more information."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-transient-mark nil
  "Controls if 'transient-mark' is turned off when 'cua-lite' is disabled.

t - 'transient-mark-mode' is NOT turned off
nil - 'transient-mark-mode' IS turned off

See the variable `transient-mark-mode' for more information."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-retain-suppress-inactive-regions nil
  "Controls if inactive regions are suppressed when 'cua-lite' is disabled.

t - inactive regions are still suppressed
nil - inactive regions are NOT suppressed

This variable has no effect if `cua-lite-disable-inactive-regions' is not true.
See the variable `mark-even-if-inactive' for more information."
  :type 'boolean
  :group 'cua-lite-disable-effects)

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
(defgroup cua-lite-hooks nil
  "Hooks for use in cua-lite mode."
  :group 'cua-lite)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-load-hook nil
  "Hook to run when package is loaded."
  :type 'hook
  :group 'cua-lite-hooks)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-hook nil
  "Hook called when 'cua-lite' mode is toggled.

Hook is run before ON or OFF hooks are run."
  :type 'hook
  :group 'cua-lite-hooks)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-on-hook nil
  "Hook called when 'cua-lite' mode is turned on.

Hook is run after all other enable actions are taken."
  :type 'hook
  :group 'cua-lite-hooks)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-off-hook nil
  "Hook called when 'cua-lite' mode is turned off.

Hook is run after all other disable actions are taken."
  :type 'hook
  :group 'cua-lite-hooks)

;; ---------------------------------------------------------------------------
(defcustom cua-lite-bind-keys-hook nil
  "Hook called when 'cua-lite' keys are bound.

The hook is called after all 'cua-lite' keys are bound."
  :type 'hook
  :group 'cua-lite-hooks)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst cua-lite-version
  "$Revision: 1.4 $"
  "Version number for 'cua-lite' package.")

;; ---------------------------------------------------------------------------
(defun cua-lite-version-number ()
  "Return 'cua-lite' version number."
  (string-match "[0123456789.]+" cua-lite-version)
  (match-string 0 cua-lite-version))

;; ---------------------------------------------------------------------------
(defun cua-lite-display-version ()
  "Display 'cua-lite' version."
  (interactive)
  (message "cua-lite version <%s>." (cua-lite-version-number)))

;;; **************************************************************************
;;; ***** constants
;;; **************************************************************************
(defconst cua-lite-is-emacs-21
  (if (string-match "Emacs 21" (emacs-version)) t nil)
  "Are we running 'cua-lite' under Emacs 21 or not?")

(defconst cua-lite-is-xemacs-21
  (if (string-match "XEmacs 21" (emacs-version)) t nil)
  "Are we running 'cua-lite' under Emacs 21 or not?")

;;; **************************************************************************
;;; ***** mini-mode setup
;;; **************************************************************************
(defvar cua-lite nil "Non-nil if 'cua-lite' is enabled.")

;; ---------------------------------------------------------------------------
(defvar cua-lite-key-restoration-list ()
  "Used to store current key bindings to aid in restoration of key bindings.")

;; ---------------------------------------------------------------------------
;;;###autoload
(defun cua-lite (&optional arg)
  "Light-weight CUA emulator that is expandable and toggle-able.

Optional ARG turns mode on iff ARG is a positive integer.  If mode is
already in the state requested, no actions are taken.

When enabled, the following keys are bound (some are optional):
  Movement keys:
    o <left>, <right>, <up>, <down> - standard definitions
    o C-<right>, C-<left> - forward & backward word
    o C-<up>, C-<down> - forward & backward paragraph
    o M-<up>, M-<down> - forward & backward paragraph
      see `cua-lite-use-simplified-paragraph-movement'
    o <prior>, <next> - page up & down (scroll down & up, respectively)
    o C-<prior>, C-<next> - top & bottom of page
      see `cua-lite-use-page-movement-keys'
    o <home>, <end> - beginning & end of line
      see `cua-lite-use-home-key-bounce' & `cua-lite-use-end-key-bounce'
    o C-<home>, C-<end> - beginning & end of buffer

  Standard non-movement keys:
    o C-<backspace> - delete word backward
      see `cua-lite-use-backward-delete-word'
    o C-a - Select All (`mark-whole-buffer')
    o C-f - Find (`isearch-forward')
    o M-f - Find Backwards (`isearch-backward')
    o C-o - Open (`find-file')
    o C-r - Replace (`replace-string')
    o C-s - Save (`save-buffer')
    o M-s - Save As (`write-file')
    o C-w - Close (`kill-buffer' or `delete-frame')
      see `cua-lite-what-is-control-w'
    o M-<f4> - Close (`kill-buffer' or `delete-frame')
      see `cua-lite-what-is-alt-f4'
    o C-z - Undo (`undo' / `advertised-undo')

In addition, certain CUA-like features of Emacs are enabled.
This is accomplished by modifying the values of the following
variables or calling the following functions (which see):
    o `mark-active' - see `cua-lite-keep-current-mark'
    o `truncate-lines' - see `cua-lite-use-hscroll-mode'
    o `hscroll-global-mode' - see `cua-lite-use-hscroll-mode'
    o `blinking-cursor-mode' - see `cua-lite-use-blinking-cursor'
    o `bar-cursor-mode' - see `cua-lite-use-bar-cursor-mode'
    o `transient-mark-mode'
    o `mark-even-if-inactive' - see `cua-lite-disable-inactive-regions'
    o `delete-selection-mode' - see `cua-lite-use-delete-selection-mode'"
  (interactive "P")

  ;; toggle on and off
  (let ((old-mode cua-lite))
	(setq cua-lite
		  (if arg (or (listp arg)
					  (> (prefix-numeric-value arg) 0))
			(not cua-lite)))

	;; nothing is ever done unless we're actually changing modes
	(if (not (equal old-mode cua-lite))
		(progn
		  ;; general hook
		  (if cua-lite-hook
			  (run-hooks 'cua-lite-hook))

		  ;; off or on?
		  (if cua-lite
			  (cua-lite-internal-enable-cmd)
			(cua-lite-internal-disable-cmd))

		  ;; we're done
		  (when (interactive-p)
			(message "cua-lite %s." (if cua-lite "enabled" "disabled"))))

	  ;; signal an error
	  (when (interactive-p)
		(error "Package 'cua-lite' is already %s" (if cua-lite "enabled" "disabled"))))
	))

;; ---------------------------------------------------------------------------
(defun cua-lite-internal-enable-cmd ()
  "Internal function called when 'cua-lite' is enabled."

  ;; (re-)bind CUA keys
  (cua-lite-reset-keybindings)

  ;; some autoloads
  (when cua-lite-use-hscroll-mode
	(autoload 'hscroll-global-mode "hscroll"))

  (when cua-lite-use-pager-fns
	(autoload 'pager-page-up "pager")
	(autoload 'pager-page-down "pager"))

  (when cua-lite-use-blinking-cursor
	(autoload 'blinking-cursor-mode "blinking-cursor"))

  (when cua-lite-use-bar-cursor
	(autoload 'bar-cursor-mode "bar-cursor"))

  ;; get rid of current mark (if any)
  (unless cua-lite-keep-current-mark
	(setq mark-active nil))

  ;; other CUA-like things
  (when (and cua-lite-use-hscroll-mode
			 (not cua-lite-is-emacs-21))
	(setq-default truncate-lines t)
	(hscroll-global-mode 1))

  (when (and cua-lite-use-blinking-cursor
			 (not cua-lite-is-emacs-21))
	(blinking-cursor-mode 1))

  (when cua-lite-use-bar-cursor
	(bar-cursor-mode 1))

  ;; transient mode mark
  (setq transient-mark-mode t)

  ;; inactive mark
  (when cua-lite-disable-inactive-regions
	(setq mark-even-if-inactive nil))

  ;; delete selection mode
  (when cua-lite-use-delete-selection-mode
	(delete-selection-mode 1))

  ;; hook when turning on
  (when cua-lite-on-hook
	(run-hooks 'cua-lite-on-hook)))

;; ---------------------------------------------------------------------------
(defun cua-lite-internal-disable-cmd ()
  "External function called when 'cua-lite' is disabled."

  ;; restore old keymap
  (cua-lite-restore-orig-keys)

  ;; disable pager fns maybe
  (when (and cua-lite-use-pager-fns
			 cua-lite-retain-pager-fns)
	(global-set-key (kbd "<prior>") 'pager-page-up)
	(global-set-key (kbd "<next>") 'pager-page-down))

  ;; turn off hscroll mode maybe
  (when (and cua-lite-use-hscroll-mode
			 (not cua-lite-retain-hscroll-mode)
			 (not cua-lite-is-emacs-21))
	(hscroll-global-mode nil))

  ;; turn off blinking cursor mode maybe
  (when (and cua-lite-use-blinking-cursor
			 (not cua-lite-retain-blinking-cursor)
			 (not cua-lite-is-emacs-21))
	(blinking-cursor-mode 0))

  ;; turn off bar cursor mode maybe
  (when (and cua-lite-use-bar-cursor
			 (not cua-lite-retain-bar-cursor))
	(bar-cursor-mode 0))

  ;; turn off transient mark mode, maybe
  (when (not cua-lite-retain-transient-mark)
	(setq transient-mark-mode nil))

  ;; turn off disable inactive regions, maybe
  (when (and cua-lite-disable-inactive-regions
			 (not cua-lite-retain-suppress-inactive-regions))
	(setq mark-even-if-inactive t))

  ;; turn off delete selection mode, maybe
  (when (and cua-lite-use-delete-selection-mode
			 (not cua-lite-retain-delete-selection-mode))
	(delete-selection-mode 0))

  ;; hook when turning off
  (when cua-lite-off-hook
	(run-hooks 'cua-lite-off-hook)))

;; ---------------------------------------------------------------------------
;; add to minor-mode-alist if not there already
(when cua-lite-display-status-in-mode-line
  (or
   (assq 'cua-lite minor-mode-alist)
   (setq minor-mode-alist
		 (cons
		  (list 'cua-lite cua-lite-mode-line-string)
		  minor-mode-alist))))

;;; **************************************************************************
;;; ***** key-binding fns
;;; **************************************************************************
(defun cua-lite-save-keystroke-for-restoration (keystroke)
  "Save KEYSTROKE for possible restoration if 'cua-lite' is ever disabled."
  (setq cua-lite-key-restoration-list
		(append (vector (list keystroke (key-binding (read-kbd-macro keystroke))))
				cua-lite-key-restoration-list)))

;; ---------------------------------------------------------------------------
(defun cua-lite-bind-key-simple (keystroke fn &optional ignore-list)
  "Save current binding for KEYSTROKE and set new binding to FN.

KEYSTROKE should be a string suitable to be passed into `read-kbd-macro'.

Current binding of KEYSTROKE is saved for possible restoration later
if 'cua-lite' is ever disabled.

IGNORE-LIST is a list of keys to ignore.  Interally, the value of
`cua-lite-ignore-key-list' is passed in, allowing you to selectively
disable individual keybindings."
  ;; check ignore list
  (unless (member keystroke cua-lite-ignore-key-list)
	;; save it for later restoration
	(cua-lite-save-keystroke-for-restoration keystroke)
	(global-set-key (read-kbd-macro keystroke) fn)))

;; ---------------------------------------------------------------------------
(defun cua-lite-bind-both-motion-keys (keystroke fn &optional imitate)
  "Call `cua-lite-bind-motion-key' for KEYSTROKE & S-KEYSTROKE.

`cua-lite-bind-motion-key' is called once for KEYSTROKE and again
for Shift-KEYSTROKE, with ACT-MARK values of nil and t, respectively.
See `cua-lite-bind-motion-key' for more details on KEYSTROK, FN and
IMITATE."
  (let ((shifted (concat "S-" keystroke)))
	(cua-lite-bind-motion-key keystroke fn nil imitate)
	(cua-lite-bind-motion-key shifted fn t imitate))
  )

;; ---------------------------------------------------------------------------
(defun cua-lite-bind-motion-key (keystroke fn act-mark &optional imitate read-only)
  "Save current binding for KEYSTROKE and set new binding to FN.

KEYSTROKE should be a string suitable to be passed into `read-kbd-macro'.

Current binding of KEYSTROKE is saved for possible restoration later
if 'cua-lite' is ever disabled.

If ACT-MARK is nil, KEYSTROKE is bound to a lambda expression that
deactivates mark and calls FN.  If ACT-MARK is non-nil, KEYSTROKE is
bound to a lambda expression that activates mark and calls FN.  In
either case, the original FN can be imitated by passing in a non-nil
value to IMITATE.  Finally, if READ-ONLY is non-nil, the `interactive'
form of the lambda expression will be set to \"*p\", otherwise it will
be set to just \"p\"."
  (let ((doc-string (concat (if act-mark "Activate" "Deactivate")
							" mark and call `" (symbol-name fn) "' interactively."))
		(interactive-string (if read-only "*p" "p")))

	;; save current definition for possible later restoration
	(cua-lite-save-keystroke-for-restoration keystroke)

	;; bind the new key
	(global-set-key (read-kbd-macro keystroke)
					`(lambda (prefix)
					   ,doc-string
					   (interactive ,interactive-string)
					   (cua-lite-ensure-mark ,act-mark)
					   (call-interactively ',fn)
					   (when ,imitate
						 (setq this-command ',fn)))
					)
	))

;; ---------------------------------------------------------------------------
(defun cua-lite-restore-orig-keys ()
  "Restore original key-bindings of all keys bound thru the 'cua-lite' package."
  (mapcar (lambda (binding)
			(let ((keystroke (car binding))
				  (fn (cadr binding)))
			  (if fn
				  (global-set-key (read-kbd-macro keystroke) fn)
				(global-unset-key (read-kbd-macro keystroke)))))
		  cua-lite-key-restoration-list))

;;; **************************************************************************
;;; ***** util functions
;;; **************************************************************************
(defsubst cua-lite-ensure-mark (activate)
  "Ensures that mark is in the desired state.

If ACTIVATE is nil, mark will be turned off (if it's not off already).
if ACTIVATE is non-nil, mark will be activated if it's not already."
  (if activate
	  (or mark-active (set-mark-command nil))
	(deactivate-mark)
	))

;; ---------------------------------------------------------------------------
(defun cua-lite-reset-keybindings ()
  "Reset current global keymap to CUA bindings."
  ;; clear out old restoration list
  (setq cua-lite-key-restoration-list '())

  ;; bind standard motion keys
  (unless (= cua-lite-default-keybindings 2)
	(cua-lite-bind-both-motion-keys "<left>" 'backward-char)
	(cua-lite-bind-both-motion-keys "<right>" 'forward-char)
	(cua-lite-bind-both-motion-keys "<up>" 'previous-line t)
	(cua-lite-bind-both-motion-keys "<down>" 'next-line t)

	(cua-lite-bind-both-motion-keys "C-<right>" 'forward-word)
	(cua-lite-bind-both-motion-keys "C-<left>" 'backward-word)

	(if cua-lite-use-simplified-paragraph-movement
		(progn
		  (cua-lite-bind-both-motion-keys "C-<up>" 'cua-lite-simple-backward-paragraph)
		  (cua-lite-bind-both-motion-keys "C-<down>" 'cua-lite-simple-forward-paragraph)
		  (when cua-lite-rebind-normal-paragraph-movement-fns
			(cua-lite-bind-both-motion-keys "M-<up>" 'backward-paragraph)
			(cua-lite-bind-both-motion-keys "M-<down>" 'forward-paragraph)))
	  (cua-lite-bind-both-motion-keys "C-<up>" 'backward-paragraph)
	  (cua-lite-bind-both-motion-keys "C-<down>" 'forward-paragraph))

	(when cua-lite-use-page-movement-keys
	  (cua-lite-bind-both-motion-keys "C-<prior>" 'cua-lite-top-of-page)
	  (cua-lite-bind-both-motion-keys "C-<next>" 'cua-lite-bottom-of-page))

	(if cua-lite-use-home-key-bounce
		(cua-lite-bind-both-motion-keys "<home>" 'cua-lite-home-key-bounce)
	  (cua-lite-bind-both-motion-keys "<home>" 'beginning-of-line))
	(if cua-lite-use-end-key-bounce
		(cua-lite-bind-both-motion-keys "<end>" 'cua-lite-end-key-bounce)
	  (cua-lite-bind-both-motion-keys "<end>" 'end-of-line t))

	(cua-lite-bind-both-motion-keys "C-<home>" 'beginning-of-buffer)
	(cua-lite-bind-both-motion-keys "C-<end>" 'end-of-buffer)

	(if cua-lite-use-pager-fns
		(progn
		  (cua-lite-bind-both-motion-keys "<prior>" 'pager-page-up)
		  (cua-lite-bind-both-motion-keys "<next>" 'pager-page-down))
	  (cua-lite-bind-both-motion-keys "<prior>" 'scroll-down)
	  (cua-lite-bind-both-motion-keys "<next>" 'scroll-up))
	)

  (unless (= cua-lite-default-keybindings 1)
	(when cua-lite-use-backward-delete-word
	  (cua-lite-bind-motion-key "C-<backspace>" 'cua-lite-backward-delete-word nil nil t))

	;; bind CUA-esque keys
	(cua-lite-bind-key-simple "C-a" 'mark-whole-buffer)
	(cua-lite-bind-key-simple "C-f" 'isearch-forward)
	(cua-lite-bind-key-simple "M-f" 'isearch-backward)
	(cua-lite-bind-key-simple "C-o" 'find-file)
	(cua-lite-bind-key-simple "C-r" 'replace-string)
	(cua-lite-bind-key-simple "C-s" 'save-buffer)
	(cua-lite-bind-key-simple "M-s" 'write-file)

	(cond
	 ((eq cua-lite-what-is-control-w 'buffer)
	  (cua-lite-bind-key-simple "C-w" 'kill-buffer))
	 ((eq cua-lite-what-is-control-w 'frame)
	  (cua-lite-bind-key-simple "C-w" 'delete-frame)))

	(cond
	 ((eq cua-lite-what-is-alt-f4 'buffer)
	  (cua-lite-bind-key-simple "M-<f8>" 'kill-buffer))
	 ((eq cua-lite-what-is-alt-f4 'frame)
	  (cua-lite-bind-key-simple "M-<f8>" 'delete-frame)))

	(cua-lite-bind-key-simple "C-z" 'undo)
	)

  ;; finally, call hook
  (run-hooks 'cua-lite-bind-keys-hook)
  )

;; ---------------------------------------------------------------------------
(defun cua-lite-home-key-bounce ()
  "Causes point to alternate between column 0 & the first non-blank column.

See `cua-lite-use-home-key-bounce' for more information."
  (interactive)
  (let ((bolp (bolp))
		(orig (point)))
	(beginning-of-line)
	(if (and bolp (bolp))
		(let ((eol (line-end-position)))
		  (skip-chars-forward " \t\n" eol)
		  (if (and (= eol (point))
				   (/= eol orig))
			  (beginning-of-line))
		  ))))

;; ---------------------------------------------------------------------------
(defun cua-lite-end-key-bounce ()
  "Cause point to alternate between the last column & the last non-blank column.

See `cua-lite-use-end-key-bounce' for more information."
  (interactive)
  (let ((eolp (eolp))
		(orig (point)))
	(end-of-line)
	(if (and eolp (eolp))
		(let ((bol (line-beginning-position)))
		  (skip-chars-backward " \t\n" bol)
		  (if (and (= bol (point))
				   (/= bol orig))
			  (end-of-line))
		  )))

  ;; imitate end-of-line
  (setq this-command 'end-of-line))

;; ---------------------------------------------------------------------------
(defun cua-lite-top-of-page ()
  "Causes point to move to the top of the current window.

See `cua-lite-use-page-movement-keys' for more information."
  (interactive)
  (move-to-window-line 0))

;; ---------------------------------------------------------------------------
(defun cua-lite-bottom-of-page ()
  "Causes point to move to the bottom of the current window.

See `cua-lite-use-page-movement-keys' for more information."
  (interactive)
  (move-to-window-line -1))

;; ---------------------------------------------------------------------------
(defun cua-lite-simple-forward-paragraph (prefix)
  "Move point forward to the end of the next paragraph.

With prefix argument PREFIX, do this that many times.

See `cua-lite-use-simplified-paragraph-movement' for more
information."
  (interactive "p")
  (save-match-data

	(let ((lcv 0))
	  ;; do N times
	  (while (< lcv prefix)
		;; go forward to next non-empty line
		(re-search-forward "^\\s-*\\S-+" (point-max) 1)

		;; and then find the next empty line
		(re-search-forward "^\\s-*$" (point-max) 1)

		(setq lcv (1+ lcv))))))

;; ---------------------------------------------------------------------------
(defun cua-lite-simple-backward-paragraph (prefix)
  "Move point backward to the beginning of the previous paragraph.

With prefix argument PREFIX, do this that many times.

See `cua-lite-use-simplified-paragraph-movement' for more
information."
  (interactive "p")
  (save-match-data

	(let ((lcv 0))
	  (beginning-of-line)

	  ;; do N times
	  (while (< lcv prefix)
		;; go backward to next non-empty line
		(while (looking-at "^\\s-*$")
		  (previous-line 1))

		;; and then find the next (previous) empty line
		(re-search-backward "^\\s-*$" (point-min) 1)
		(setq lcv (1+ lcv))))))

;; ---------------------------------------------------------------------------
(defun cua-lite-delete-word (arg)
  "Delete characters forward until encountering the end of a word.

With prefix argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; ---------------------------------------------------------------------------
(defun cua-lite-backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.

With prefix argument ARG, do this that many times."
  (interactive "p")
  (cua-lite-delete-word (- arg)))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'cua-lite)
(run-hooks 'cua-lite-load-hook)

;;; cua-lite.el ends here
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************
