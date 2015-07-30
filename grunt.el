;;; grunt.el --- Some glue to stick Emacs and Gruntfiles together
;; Version: 0.0.3

;; Copyright (C) 2014  Daniel Gempesaw

;; Author: Daniel Gempesaw <dgempesaw@sharecare.com>
;; Keywords: convenience, grunt
;; URL: https://github.com/gempesaw/grunt.el
;; Package-Requires: ((dash "2.9.0"))
;; Created: 2014 Apr 1

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

(defcustom grunt-read-tasks-mode t
  "Which tasks you would like to read.

If t it will read all of the tasks, including the ones loaded by grunt modules.
If nil it will read only the user registered tasks.

The default value is t which means that we resolve the tasks using the
grunt-help-command method."
  :type '(choice
          (const :tag "Read all tasks including ones loaded by grunt modules" t)
          (const :tag "Read only user registered tasks" nil))
  :group 'grunt)

(defcustom grunt-cache-tasks nil
  "Whether or not to cache the tasks until a project change occurs.

If t then running `grunt-exec` will cache the tasks until the Gruntfile.js
being used changes.  This improves the speed of `grunt-exec` but won't get
Gruntfile.js changes."
  :type 'boolean
  :group 'grunt)

(defvar grunt-current-tasks-cache nil
  "The cache of current grunt tasks.")

;;;###autoload
(defun grunt-exec ()
  "Invoke this while in your project and it will suggest registered tasks.

You can also manually enter in a specific task that isn't
registered.  It will get/create one buffer per task per project,
as needed."
  (interactive)
  (unless (grunt-locate-gruntfile)
    (grunt-clear-tasks-cache)
    (error "Sorry, we couldn't find a gruntfile.  Consider setting `grunt-current-path' manually?"))
  (let* ((task (ido-completing-read
                "Execute which task: "
                (grunt-resolve-registered-tasks) nil nil))
         (command (grunt--command task))
         (buf (grunt--project-task-buffer task))
         (default-directory grunt-current-dir)
         (ret))
    (message "%s" command)
    (setq ret (async-shell-command command buf buf))
    ;; handle window sizing: see #6
    (grunt--set-process-dimensions buf)
    ret))

(defun grunt--project-task-buffer (task)
  (let* ((bufname (format "*grunt-%s*<%s>" task grunt-current-project))
         (buf (get-buffer bufname))
         (proc (get-buffer-process buf)))
    (when (and grunt-kill-existing-buffer buf proc)
      (set-process-query-on-exit-flag proc nil)
      (kill-buffer bufname))
    (get-buffer-create bufname)))

(defun grunt-resolve-registered-tasks ()
	"Build a list of Grunt tasks.

Based on the `grunt-read-tasks-mode` it will load all tasks or just the
user registerdTasks."
  (if grunt-read-tasks-mode
			(grunt--resolve-registered-tasks-from-grunthelp)
		(grunt--resolve-registered-tasks-from-gruntfile))
  )

(defun grunt--resolve-registered-tasks-from-grunthelp ()
  "Build a list of potential Grunt tasks from grunt-help-command.

The list is constructed performing the `grunt --help` command,
or similar, and narrowing down to the Available tasks section before extracting
the tasks using regexp."
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
Gruntfile at `grunt-current-path'.  This is incredibly fragile and
will break on something as simple as an alternate quoting scheme
or indentation, and it _only_ supports manually registered
tasks."
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
  "Return a list of lines from the tasks region from the `grunt-help-command`."
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

This function will return the cached version of the command
if the cache is not empty."
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
  (setq grunt-current-tasks-cache nil))

(defun grunt--set-process-dimensions (buf)
  (let ((process (get-buffer-process buf)))
    (when process
      (set-process-window-size process
                               (window-height)
                               (window-width)))))

(provide 'grunt)
;;; grunt.el ends here
