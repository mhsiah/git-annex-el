;;; git-annex.el --- Mode for easy editing of git-annex'd files

;; Copyright (C) 2012 John Wiegley
;; Copyright (C) 2014 Hamish Ivey-Law

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

(defcustom git-annex-debug-messages nil
  "If not nil, print git and git-annex commands to *Messages* buffer.")

;; These two DEFVAR + MAKE-VARIABLE-BUFFER-LOCAL calls could be
;; DEFVAR-LOCALs from 24.3 onwards.
(defvar git-annex--buffer-work-dir nil
  "The git-annex working directory associated to the current buffer.

It is initially NIL, then either set to the root of the git annex
or to the empty string if the buffer doesn't correspond to a
git-annexed file.  Essentially GIT-ANNEX-BUFFER-WORK-DIR is the
work tree associated with the annex and
GIT-ANNEX-BUFFER-WORK-DIR/.git is its GITDIR.")

(make-variable-buffer-local 'git-annex--buffer-work-dir)

(defvar git-annex--buffer-file-annexname nil
  "The git-annex-friendly name for the current buffer.

If the current buffer is managed by git-annex, return a
git-annex-friendly name for it, otherwise return nil. A
git-annex-friendly path is one in which all the symlinks have
been expanded except the one that points into
GIT-ANNEX-BUFFER-WORK-DIR/.git/annex/objects/.  The name is
always relative to GIT-ANNEX-BUFFER-WORK-DIR.")

(make-variable-buffer-local 'git-annex--buffer-file-annexname)

(defun git-annex--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun git-annex--message (prefix cmd args)
  "Print a message describing the git call with CMD and ARGS."
  (when git-annex-debug-messages
    (let* ((strargs (format "%s" args))
           (pretty-args (substring strargs 1 (1- (length strargs)))))
    (message "%s: [in %s] git %s %s"
             prefix git-annex--buffer-work-dir cmd pretty-args))))

(defun git-annex--cleanup-message (msg)
  "Convert a multi-line git message into a one-line git message."
  (replace-regexp-in-string "\n" "; " (git-annex--chomp msg)))

(defmacro git-annex--in-directory (d &rest body)
  (let ((olddir (cl-gensym))
        (res (cl-gensym)))
    `(let ((,olddir default-directory))
       (cd ,d)
       (let ((,res (progn ,@body)))
         (cd ,olddir)
         ,res))))

(defun git-annex--git (cmd &rest args)
  "Run git command CMD with arguments ARGS."
  (git-annex--message "Annex command" cmd args)
  (git-annex--in-directory git-annex--buffer-work-dir
    (with-temp-buffer
      (let* ((res (apply #'call-process "git" nil t nil cmd args))
             (msg (git-annex--cleanup-message (buffer-string))))
        (when git-annex-debug-messages
          (if (zerop res)
              (message "Result: %s" msg)
              (message "Command \"git %s %s\" failed with error: %s"
                       cmd args msg)))
        (cons res msg)))))

(defun git-annex--git-annex (cmd &rest args)
  "Run git-annex command CMD with arguments ARGS."
  (apply #'git-annex--git "annex" cmd args))

(defun git-annex--expand-symlinks (fname)
  "If the filename FNAME refers to a symlink, expand it
repeatedly until either the result is a a symlink managed by
git-annex (i.e. it points into '.git/annex/objects'), or FNAME
refers to a regular file.  If FNAME refers to a regular file to
begin with, it is simply returned."
  (let ((lnk (file-symlink-p fname)))
    (if lnk
        (if (string-match "\\.git/annex/objects" lnk)
            fname
            (git-annex--expand-symlinks lnk))
        fname)))

(defun git-annex--buffer-file-truename (fname)
  "If the filename FNAME is not nil, return a cons whose car is
the 'truename' of the containing directory and whose cdr the
git-annex managed filename.  Return nil if FNAME is nil."
  (when fname
      (let* ((deref (git-annex--expand-symlinks fname))
             (f (file-name-nondirectory deref))
             (d (file-name-directory deref)))
        (concat (file-truename d) f))))

(defun git-annex--parent-directory (dir)
  "Given a directory name DIR, return the name of the parent
directory of DIR, or nil if it is the root directory."
  (when (not (equal "/" dir))
    (file-name-directory (directory-file-name dir))))

(defun git-annex--work-dir-of-file (fname)
  "Given a filename FNAME, return the path containing the
git-annex repository that manages FNAME, or nil if not found."
  (let ((d (git-annex--parent-directory fname)))
    (when d
      (if (file-directory-p (concat d ".git/annex"))
          d
          (git-annex--work-dir-of-file d)))))

(defun git-annex--buffer-file-pathinfo ()
  "Return a list containing a git-annex-friendly absolute path to
bufname and the git-annex--buffer-work-dir absolute path."
  (let* ((absname (git-annex--buffer-file-truename buffer-file-name))
         (dir (git-annex--work-dir-of-file absname)))
    (if dir
        (cons (file-relative-name absname dir) dir)
        (cons absname nil))))

(defun git-annex--buffer-was-modified ()
  "Return true iff git-annex thinks the buffer was modified."
  (let ((msg (cdr (git-annex--git "status" "-s"
                                  git-annex--buffer-file-annexname))))
    (not (string= msg ""))))

(defun git-annex-add-file ()
  "Run 'git-annex add' on the current buffer, and 'git commit' if
GIT-ANNEX-COMMIT is true and the file has been modified."
  (git-annex--git-annex "add" git-annex--buffer-file-annexname)
  ;; "git commit" returns an error (i.e. a non-zero return value to
  ;; calling process) if there is nothing to commit, so we check that
  ;; there's something to commit before trying.
  (when (and git-annex-commit (git-annex--buffer-was-modified))
    (git-annex--git "commit" "-m" "Updated" "--"
                    git-annex--buffer-file-annexname)))

(defun git-annex--revert-while-maintaining-position ()
  "Revert the current buffer while attempting to maintain the
position of point."
  (let ((here (point-marker)))
    (unwind-protect
        (revert-buffer nil t t)
        (goto-char here))))

(defun git-annex-lock-annexed-file ()
  "Lock the git-annex-managed current buffer."
  (save-buffer)
  (let ((res (git-annex--git
              "diff-files" "--diff-filter=T"
              "-G^[./]*\\.git/annex/objects/" "--name-only"
              "--" git-annex--buffer-file-annexname)))
    (unless (and (zerop (car res)) (string= (cdr res) ""))
      (git-annex-add-file)
      (git-annex--revert-while-maintaining-position)
      (setq buffer-read-only nil))))

(defun git-annex-unlock-annexed-file ()
  "Unlock the git-annex-managed current buffer."
  (when (zerop (car (git-annex--git-annex
                     "edit" git-annex--buffer-file-annexname)))
    (git-annex--revert-while-maintaining-position)
    (add-hook 'kill-buffer-hook #'git-annex-lock-annexed-file nil t)
    (setq buffer-read-only t)))

(defun git-annex-buffer-is-annexed-p ()
  "Return true iff the current buffer is managed by git-annex.

As a side effect, this function sets the variable
GIT-ANNEX--WORK-DIR, which is necessary for most of the rest of
the functionality."
  (when buffer-file-name
    (unless git-annex--buffer-file-annexname
      (let ((path (git-annex--buffer-file-pathinfo)))
        (setq git-annex--buffer-file-annexname (car path))
        (setq git-annex--buffer-work-dir (cdr path))))
    git-annex--buffer-work-dir))

(defun git-annex-toggle-lock ()
  "Toggle whether the current buffer is read-only; if the buffer
is managed by git-annex, toggle its locked status."
  (when (git-annex-buffer-is-annexed-p)
    (let ((issymlnk (file-symlink-p
                     (concat git-annex--buffer-work-dir
                             git-annex--buffer-file-annexname))))
      (cond ((and buffer-read-only issymlnk)
             (git-annex-unlock-annexed-file))
            ((not (or buffer-read-only issymlnk))
             (git-annex-lock-annexed-file))))))

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
                  (apply #'call-process "git" nil nil nil "annex" '(command file))
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
