;;; grunt.el --- Some glue to stick Emacs and Gruntfiles together
;; Version: 1.2.1

;; Copyright (C) 2014  Daniel Gempesaw

;; Author: Daniel Gempesaw <dgempesaw@sharecare.com>
;; Keywords: convenience, grunt
;; URL: https://github.com/gempesaw/grunt.el
;; Package-Requires: ((dash "2.9.0") (ansi-color "3.4.2"))
;; Created: 2014 Apr 1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I got tired of managing shells and one off Async Command buffers to
;; kick off Grunt tasks. This package provides rudimentary access to
;; the tasks in a Gruntfile.

;; When your default-directory is somewhere in a JS project with a
;; Gruntfile, invoke `grunt-exec' or bind something to it. You can
;; either execute one of the suggested registered tasks, or input a
;; custom task of your own. It will create one buffer per project per
;; task, killing any existing buffers by default.

;;; Code:

(require 'dash)
(require 'ansi-color)

(defgroup grunt nil
  "Execute grunt tasks from your Gruntfile from Emacs"
  :group 'convenience)

(defcustom grunt-kill-existing-buffer t
  "Whether or not to kill the existing process buffer.

Defaults to t. When not nil, we will try to kill the buffer name
that we construct to do our task.  Of course, if you rename your
buffer, we won't be able to kill it."
  :type 'boolean
  :group 'grunt)

(defcustom grunt-base-command (executable-find "grunt")
  "The path to the grunt binary.

You may have to fix this if `grunt' isn't in your PATH."
  :type 'string
  :group 'grunt)

(defcustom grunt-help-command (format "%s --help --no-color" grunt-base-command)
  "Command to get the help section from grunt."
  :type 'string
  :group 'grunt)

(defcustom grunt-verbose t
  "Whether to be verbose with messaging."
  :type '(boolean)
  :group 'grunt)

(defcustom grunt-options ""
  "Additional options to pass to grunt."
  :type '(string)
  :group 'grunt)

(defcustom grunt-current-path ""
  "Path to the current gruntfile.

We'll try to find this on our own."
  :type '(string)
  :group 'grunt)

(defcustom grunt-current-dir ""
  "Path to the directory of the current gruntfile.

We'll try to find this on our own."
  :type '(string)
  :group 'grunt)

(defcustom grunt-current-project ""
  "Name of the current project in which the Gruntfile is found."
  :type '(string)
  :group 'grunt)

(defcustom grunt-show-all-tasks t
  "Which tasks you would like to read.

If t it will suggest all of the tasks, including the ones loaded
by grunt modules.

If nil it will suggest only the user registered tasks.

The default value is t which means that we resolve the tasks
using the grunt-help-command method.  Since shelling out to run
`grunt --help` can be slow, we also default to caching the tasks
for the current project; see `grunt-cache-tasks' for more."
  :type '(choice
          (const :tag "Read all tasks including ones loaded by grunt modules" t)
          (const :tag "Read only user registered tasks" nil))
  :group 'grunt)

(defcustom grunt-cache-tasks t
  "Whether or not to cache the tasks until a project change occurs.

If t then running `grunt-exec' will cache the tasks until the
path to the Gruntfile.js being used changes.  That is, when you
switch projects to one with a different Gruntfile, that's the
next time we'll invoke `grunt --help`.  This improves the speed of
`grunt-exec', but won't pick up changes to the content of the
current Gruntfile.js.

To have us suggest new/changed tasks after editing the current
Gruntfile, you can refresh the cache manually by using a prefix
argument when invoking `grunt-exec'."
  :type 'boolean
  :group 'grunt)

(defvar grunt-current-tasks-cache nil
  "The cache of current grunt tasks.")

(defvar grunt-previous-task nil
  "Previous task that was run.")

;;;###autoload
(defun grunt-exec (&optional pfx)
  "Run tasks from gruntfile.  Calling with PFX will clear the cache of tasks.

You can also manually enter in any valid task at the prompt, even
if it's not suggested.  It will get/create one buffer per task
per project, as needed.

When invoked with a prefix argument, we'll clear the tasks cache
for you.  Note that if `grunt-show-all-tasks' is nil, the
cache (and the prefix argument functionality of this function) is
immaterial."
  (interactive "p")
  (unless (grunt-locate-gruntfile)
    (error "Sorry, we couldn't find a gruntfile.  Consider setting `grunt-current-path' manually?"))
  (when (and pfx (> pfx 1)) (grunt-clear-tasks-cache))
  (let* ((task (ido-completing-read
                "Execute which task: "
                (grunt-resolve-registered-tasks) nil nil)))
    (setq grunt-previous-task task)
    (grunt--run task)))

(defun grunt-rerun ()
  "Rerun the previous grunt task."
  (interactive)
  (unless grunt-previous-task
    (error "You have not run a grunt task yet.  Run `grunt-exec` first"))
  (grunt--run grunt-previous-task))

(defun grunt--run (task)
  "Set up the process buffer and run TASK."
  (let ((cmd (grunt--command task))
        (buf (grunt--project-task-buffer task))
        (proc nil))
    (grunt--message (format "%s" cmd))
    (setq proc (start-process-shell-command (buffer-name buf) buf cmd))
    (set-process-filter proc #'grunt--process-filter)
    (grunt--set-process-dimensions buf)
    (grunt--set-process-read-only buf)
  proc))

(defun grunt--process-filter (proc string)
  "Filter function for process PROC to apply ansi color and highlight links in STRING."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (regexp "\\(/[a-z0-9-\._/]+\\):\\([0-9]+\\):\\([0-9]+\\)")
            (start-point (point-min)))
       ;; Calculate the point of the last Path found
       (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
         (setq start-point (match-end 0))))
       (insert string)
       (ansi-color-apply-on-region (process-mark proc) (point))
       ;; Make buttons out of matched paths from previous last one
       (save-excursion
        (goto-char last-button-point)
        (while (re-search-forward regexp nil t)
         (when (match-string 0) (grunt--make-stack-trace-button (match-beginning 0) (match-end 0) 'match-string))))
			 (set-marker (process-mark proc) (point))))))

(defun grunt--make-stack-trace-button (beg end m)
 "Make a button from BEG to END which will click through to match found in M."
 (make-button beg end
	'file-name (funcall m 1) 'line-num (funcall m 2) 'char-num (funcall m 3)
	'action 'grunt--go-to-stack-trace))

(defun grunt--go-to-stack-trace (args)
 "Go to stack trace file from a button action callback with ARGS."
 (let ((file-name (button-get args 'file-name))
			 (line-num (string-to-number (button-get args 'line-num)))
			 (char-num (string-to-number (button-get args 'char-num))))
	(ring-insert find-tag-marker-ring (point-marker))
	(find-file file-name)
	(goto-line line-num)
	(forward-char (+ -1 char-num))))

(defun grunt--project-task-buffer (task)
  "Create a process buffer for the grunt TASK."
  (let* ((bufname (format "*grunt-%s*<%s>" task grunt-current-project))
         (buf (get-buffer bufname))
         (proc (get-buffer-process buf)))
    (when (and grunt-kill-existing-buffer buf proc)
      (set-process-query-on-exit-flag proc nil)
      (kill-buffer bufname))
    (grunt--clear-task-buffer buf)
    (get-buffer-create bufname)))

(defun grunt--clear-task-buffer (buf)
  "Clears the task buffer BUF.
Sets the buffer to non read only mode when it's erased, this should be reset
as soon as the task begins running."
  (when (buffer-live-p buf)
    (with-current-buffer buf (read-only-mode 0) (erase-buffer))))

(defun grunt-resolve-registered-tasks ()
  "Build a list of Grunt tasks.

Based on the `grunt-show-all-tasks' it will load all tasks or
just the user registerdTasks."
  (if grunt-show-all-tasks
      (grunt--resolve-registered-tasks-from-grunthelp)
    (grunt--resolve-registered-tasks-from-gruntfile)))

(defun grunt--resolve-registered-tasks-from-grunthelp ()
  "Build a list of potential Grunt tasks from grunt-help-command.

The list is constructed performing the `grunt --help` command, or
similar, and narrowing down to the Available tasks section before
extracting the tasks using regexp."
  (if (and grunt-cache-tasks grunt-current-tasks-cache)
      ;; If caching is turned on and a cached value exists
      grunt-current-tasks-cache
    (let* ((contents (grunt--get-help-tasks))
           (result
            (-non-nil
             (-map (lambda (line) (when (string-match "^[\s\t]*\\([a-zA-Z:\-]+?\\)  " line)
                                    (match-string 1 line))) contents))))
      (if grunt-cache-tasks (setq grunt-current-tasks-cache result) result))))

(defun grunt--resolve-registered-tasks-from-gruntfile ()
  "Build a list of potential Grunt tasks from the gruntfile.

The list is constructed by searching for registerTask in the
Gruntfile at `grunt-current-path'.  This is incredibly fragile
and will break on something as simple as an alternate quoting
scheme or indentation, and it _only_ supports manually registered
tasks.

To suggest all valid tasks, see `grunt-show-all-tasks'."
  (let* ((contents (with-temp-buffer
                     (insert-file-contents grunt-current-path)
                     (split-string (buffer-string) "\n"))))
    (-map (lambda (line)
            (string-match "[\"']\\\(.*?\\\)[\"\']" line)
            (match-string 1 line))
          (-filter (lambda (line)
                     (string-match-p "registerTask" line))
                   contents))))

(defun grunt--get-help-tasks ()
  "Return a list of lines from the tasks region from the `grunt-help-command'."
  (with-temp-buffer
    (insert (grunt--get-help))
    (goto-char 0)
    (let* ((tasks-start (search-forward "Available tasks" nil t))
           (tasks-end (re-search-forward "^$" nil t)))
      (when tasks-start
        (narrow-to-region tasks-start tasks-end)
        (split-string (buffer-string) "\n")))))

(defun grunt--get-help ()
  "Run grunt-help-cmd for the current grunt-project.

This function will return the cached version of the command if
the cache is not empty."
  (grunt--message "Building task list from grunt --help, one moment...")
  (shell-command-to-string
   (format "cd %s; %s" grunt-current-dir grunt-help-command)))

(defun grunt-resolve-options ()
  "Set up the arguments to the grunt binary.

This lets us invoke grunt properly from any directory with any
gruntfile and pulls in the user specified `grunt-options'"
  (format "%s %s"
          (mapconcat
           (lambda (item)
             (format "--%s %s" (car item) (shell-quote-argument (cadr item))))
           `(("base" ,grunt-current-dir)
             ("gruntfile" ,grunt-current-path))
           " ")
          grunt-options))

(defun grunt--command (task)
  "Return the grunt command for the specified TASK."
  (unless grunt-base-command
    (setq grunt-base-command (executable-find "grunt")))
  (mapconcat 'identity `(,grunt-base-command ,(grunt-resolve-options) ,task) " "))

(defun grunt--message (s)
  "Print a string message S if in verbose mode."
  (when grunt-verbose (message s)))

(defun grunt-locate-gruntfile (&optional directory)
  "Search the current DIRECTORY and upwards for a Gruntfile."
  (let ((gruntfile-dir (locate-dominating-file
                        (if directory
                            directory
                          default-directory) "Gruntfile.js")))
    (when gruntfile-dir
      (when (and grunt-cache-tasks
                 (not (string= (file-truename gruntfile-dir) grunt-current-dir)))
        ;; Caching turned on and different gruntfile - clear cache
        (grunt-clear-tasks-cache))
      (setq gruntfile-dir (file-truename gruntfile-dir)
            grunt-current-dir gruntfile-dir
            grunt-current-project (car (last (split-string gruntfile-dir "/" t)))
            grunt-current-path (format "%sGruntfile.js" gruntfile-dir)))))

(defun grunt-clear-tasks-cache ()
  "Clear the cache of tasks."
  (interactive)
  (setq grunt-current-tasks-cache nil)
  (setq grunt-previous-task nil))

(defun grunt--set-process-dimensions (buf)
  "Set the dimensions of the process buffer BUF."
  (let ((process (get-buffer-process buf)))
    (when process
      (display-buffer buf '(nil (allow-no-window . t)))
      (set-process-window-size process
                               (window-height)
                               (window-width)))))

(defun grunt--set-process-read-only (buf)
  "Set the buffer BUF to behave like a compilation buffer.

This means making it read only and locally binding the 'q' key to quit."
  (with-current-buffer buf
    (read-only-mode)
    (grunt-process-minor-mode)))

(defvar grunt-process-minor-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

(define-minor-mode grunt-process-minor-mode
  "Minor mode for grunt process key bindings."
  :init-value nil
  (define-key grunt-process-minor-mode-map (kbd "g") 'grunt-rerun)
  (define-key grunt-process-minor-mode-map (kbd "q") '(lambda () (interactive) (quit-window))))

(provide 'grunt)
;;; grunt.el ends here
