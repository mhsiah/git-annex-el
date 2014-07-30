;;; git-annex.el --- Mode for easy editing of git-annex'd files

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 20 Oct 2012
;; Version: 1.0
;; Keywords: files data git annex
;; X-URL: https://github.com/jwiegley/git-annex-el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Typing C-x C-q in an annexed file buffer causes Emacs to run "git annex
;; edit".  When the buffer is killed Emacs will run "git annex add && git
;; commit -m Updated".
;;
;; Dired has been extended to not show Annex symlinks, but instead to color
;; annexed files green (and preserve the "l" in the file modes).  You have the
;; following commands available in dired-mode on all marked files (or the
;; current file):
;;
;;     @ a    Add file to Git annex
;;     @ e    Edit an annexed file

(eval-when-compile
  (require 'dired nil t)          ; for variable dired-mode-map
  (require 'dired-aux nil t)      ; for function dired-relist-file
  (require 'cl))

(defgroup git-annex nil
  "Mode for easy editing of git-annex'd files"
  :group 'files)

(defcustom git-annex-commit t
  "If not nil, git-annex command will commit by default.

otherwise you will have to commit by hand.")

(defcustom git-annex-debug-messages t
  "If not nil, print git and git-annex commands to *Messages* buffer.")

;; These two DEFVAR + MAKE-VARIABLE-BUFFER-LOCAL calls could be
;; DEFVAR-LOCALs from 24.3 onwards.
(defvar buffer-git-dir nil
  "The GITDIR associated to the current buffer.

It is initially NIL, then either set to the appropriate GITDIR or
to the empty string if the buffer doesn't correspond to a
git-annexed file.")

(make-variable-buffer-local 'buffer-git-dir)

(defvar buffer-file-annexname nil
  "The git-annex-friendly name for the current buffer, if the
current buffer is managed by git-annex, nil otherwise

A git-annex-friendly path is one in which all the symlinks have
been expanded except the one that points into
.git/annex/objects/.")

(make-variable-buffer-local 'buffer-file-annexname)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun debug-message (cmd args)
  (when git-annex-debug-messages
    (let* ((strargs (format "%s" args))
           (pretty-args (substring strargs 1 (1- (length strargs)))))
    (message "Annex operation: git --git-dir %s --work-tree %s %s %s"
             buffer-git-dir (parent-directory buffer-git-dir)
             cmd pretty-args))))

(defun git (cmd &rest args)
  "Run git command CMD with arguments ARGS."
  (debug-message cmd args)
  (let ((dir buffer-git-dir))
    (with-temp-buffer
      (let* ((res (apply #'call-process "git" nil t nil
                         "--git-dir" dir
                         "--work-tree" (parent-directory dir)
                         cmd args))
             (msg (chomp (buffer-string))))
        (message "Result: %s" msg)
        (unless (zerop res)
          (message "Command \"git %s %s\" failed with error:\n  %s"
                   cmd args msg))
        (cons res msg)))))

(defun git-annex (cmd &rest args)
  "Run git-annex command CMD with arguments ARGS."
  (apply #'git "annex" cmd args))

(defun expand-symlinks (fname)
  "If the filename FNAME refers to a symlink, expand it
repeatedly until either the result is a a symlink managed by
git-annex (i.e. it points into '.git/annex/objects'), or FNAME
refers to a regular file.  If FNAME refers to a regular file to
begin with, it is simply returned."
  (let ((lnk (file-symlink-p fname)))
    (if lnk
        (if (string-match "\\.git/annex/objects" lnk)
            fname
            (expand-symlinks lnk))
        fname)))

(defun get-buffer-file-annexname (fname)
  "If the filename FNAME is not nil, return a cons whose car is
the 'truename' of the containing directory and whose cdr the
git-annex managed filename.  Return nil if FNAME is nil."
  (if fname
      (let* ((deref (expand-symlinks fname))
             (f (file-name-nondirectory deref))
             (d (file-name-directory deref)))
        (concat (file-truename d) f))))

(defun parent-directory (dir)
  "Given a directory name DIR, return the name of the parent
directory of DIR, or nil if it is the root directory."
  (if (not (equal "/" dir))
    (file-name-directory (directory-file-name dir))))

(defun git-dir-of-file (fname)
  "Given a filename FNAME, return the path containing the
git-annex repository that manages FNAME, or nil if not found."
  (let ((d (parent-directory fname)))
    (when d
      (if (file-directory-p (concat d ".git/annex"))
          (concat d ".git")
          (git-dir-of-file d)))))

(defun buffer-file-pathinfo ()
  "Return a list containing a git-annex-friendly absolute path to
bufname and the buffer-git-dir absolute path."
  (let ((path (get-buffer-file-annexname buffer-file-name)))
    (cons path (git-dir-of-file path))))

(defun buffer-was-modified ()
  "Return true if git-annex thinks the buffer was modified, false
otherwise."
  (let ((msg (cdr (git "status" "-s" buffer-file-annexname))))
    (not (string= msg ""))))

(defun git-annex-add-file ()
  "Run 'git-annex add' on the current buffer, and 'git commit' if
GIT-ANNEX-COMMIT is true and the file had been modified."
  (git-annex "add" buffer-file-annexname)
  ;; "git commit" does not permit empty commits, so we check that
  ;; there's something to commit before trying.
  (when git-annex-commit (and (buffer-was-modified))
    (git "commit" "-m" "Updated")))

(defun revert-while-maintaining-position ()
  "Revert the current buffer while attempting to maintain the
position of point."
  (let ((here (point-marker)))
    (unwind-protect
        (revert-buffer nil t t)
      (goto-char here))))

(defun unlock-annexed-file ()
  "Unlock the git-annex-managed current buffer."
  (when (zerop (car (git-annex "edit" buffer-file-annexname)))
    (revert-while-maintaining-position)
    (add-hook 'kill-buffer-hook #'git-annex-add-file nil t)
    (setq buffer-read-only t)))

(defun lock-annexed-file ()
  "Lock the git-annex-managed current buffer."
  (let ((res (git "diff-files" "--diff-filter=T"
                  "-G^[./]*\\.git/annex/objects/" "--name-only"
                  "--" buffer-file-annexname)))
    (unless (and (zerop (car res)) (string= (cdr res) ""))
      (git-annex-add-file)
      (revert-while-maintaining-position)
      (setq buffer-read-only nil))))

(defun buffer-is-annexed-p ()
  "Return true if the current buffer is managed by git-annex,
false otherwise."
  (when buffer-file-name
    (unless buffer-file-annexname
      (let ((path (buffer-file-pathinfo)))
        (setq buffer-file-annexname (car path))
        (setq buffer-git-dir (or (cdr path) ""))))
    (not (string= buffer-git-dir ""))))

(defun git-annex-toggle-lock ()
  "Toggle whether the current buffer is read-only; if the buffer
is managed by git-annex, toggle its locked status."
  (when (buffer-is-annexed-p)
    (cond ((and buffer-read-only (file-symlink-p buffer-file-annexname))
           (unlock-annexed-file))
          ((and (not buffer-read-only) 
                (not (file-symlink-p buffer-file-annexname))))
          (t (lock-annexed-file)))))

;; toggle-read-only is obsolete as of Emacs 24.3; C-x C-q is now bound
;; to read-only-mode.
(defadvice toggle-read-only (before git-annex-edit-file activate)
  (git-annex-toggle-lock))

(defadvice read-only-mode (before git-annex-edit-file activate)
  (git-annex-toggle-lock))


(defface git-annex-dired-annexed-available
  '((((class color) (background dark))
     (:foreground "dark green"))
    (((class color) (background light))
     (:foreground "dark green")))
  "Face used to highlight git-annex'd files."
  :group 'git-annex)

(defface git-annex-dired-annexed-unavailable
  '((((class color) (background dark))
     (:foreground "firebrick"))
    (((class color) (background light))
     (:foreground "firebrick")))
  "Face used to highlight git-annex'd files."
  :group 'git-annex)

(defvar git-annex-dired-annexed-available 'git-annex-dired-annexed-available
  "Face name used to highlight available git-annex'd files.")
(defvar git-annex-dired-annexed-unavailable 'git-annex-dired-annexed-unavailable
  "Face name used to highlight unavailable git-annex'd files.")
(defvar git-annex-dired-annexed-invisible
  '(face git-annex-dired-annexed-available invisible t)
  "Face name used to hide a git-annex'd file's annex path.")

(defun git-annex-lookup-file (limit)
  (and (re-search-forward " -> \\(.*\\.git/annex/.+\\)" limit t)
       (file-exists-p
        (expand-file-name (match-string 1) (dired-current-directory)))))

(eval-after-load "dired"
  '(progn
     (add-to-list 'dired-font-lock-keywords
                  (list " -> .*\\.git/annex/"
                        '("\\(.+\\)\\( -> .+\\)" (dired-move-to-filename) nil
                          (1 git-annex-dired-annexed-unavailable)
                          (2 git-annex-dired-annexed-invisible))))
     (add-to-list 'dired-font-lock-keywords
                  (list 'git-annex-lookup-file
                        '("\\(.+\\)\\( -> .+\\)" (dired-move-to-filename) nil
                          (1 git-annex-dired-annexed-available)
                          (2 git-annex-dired-annexed-invisible))))))

(defvar git-annex-dired-map
  (let ((map (make-keymap)))
    (define-key map "a" 'git-annex-dired-add-files)
    (define-key map "d" 'git-annex-dired-drop-files)
    (define-key map "e" 'git-annex-dired-edit-files)
    (define-key map "g" 'git-annex-dired-get-files)
    map)
  "Git-annex keymap for `dired-mode' buffers.")

(add-hook 'dired-mode-hook
          (lambda () (define-key dired-mode-map "@" git-annex-dired-map)))

(defun git-annex-dired--apply (command file-list)
  (let ((here (point)))
    (unwind-protect
        (mapc #'(lambda (file)
                  (git-annex command file)
                  (dired-relist-file (expand-file-name file)))
              file-list)
      (goto-char here))))

(defmacro git-annex-dired-do-to-files (cmd msg &optional commit-after)
  `(defun ,(intern (concat "git-annex-dired-" cmd "-files"))
     (file-list &optional arg)
     (interactive
      (let ((files (dired-get-marked-files t current-prefix-arg)))
        (list files current-prefix-arg)))
     (git-annex-dired--apply ,cmd file-list)
     (let ((msg (format ,msg (length file-list))))
       ,(if commit-after
            `(when git-annex-commit (call-process "git" nil nil nil "commit" "-m" msg)))
       (message msg))))

(git-annex-dired-do-to-files "add" "Annex: updated %d file(s)" t)
(git-annex-dired-do-to-files "drop" "Annex: dropped %d file(s)")
(git-annex-dired-do-to-files "edit" "Annex: unlocked %d file(s) for editing")
(git-annex-dired-do-to-files "get" "Annex: got %d file(s)")

(provide 'git-annex)

;;; git-annex.el ends here
