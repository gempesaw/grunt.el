;;; grunt.el --- Some glue to stick Emacs and Gruntfiles together
;; Version: 0.0.2

;; Copyright (C) 2014  Daniel Gempesaw

;; Author: Daniel Gempesaw <dgempesaw@sharecare.com>
;; Keywords: convenience, grunt
;; URL: https://github.com/gempesaw/grunt.el
;; Package-Requires: ((dash "2.6.0"))
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
;; task.

;;; Code:

(require 'dash)

(defgroup grunt nil
  "Execute grunt tasks from your Gruntfile from Emacs"
  :group 'convenience)

(defcustom grunt-base-command (executable-find "grunt")
  "The path to the grunt binary.

You may have to fix this if `grunt' isn't in your PATH."
  :type 'string
  :group 'grunt)

(defcustom grunt-options ""
  "Additional options to pass to grunt"
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
  "Name of the current project in which the Gruntfile is found"
  :type '(string)
  :group 'grunt)

;;;###autoload
(defun grunt-exec ()
  "Invoke this while in your project and it will suggest
registered tasks.

You can also manually enter in a specific task that isn't
registered. It will get/create one buffer per task per project,
as needed."
  (interactive)
  (unless (grunt-locate-gruntfile)
    (error "Sorry, we couldn't find a gruntfile. Consider setting `grunt-current-path' manually?"))
  (let* ((task (ido-completing-read
                "Execute which task: "
                (grunt-resolve-registered-tasks) nil nil))
         (command (grunt--command task))
         (buf (get-buffer-create
               (format "*grunt-%s*<%s>" task grunt-current-project)))
         (default-directory grunt-current-dir))
    (message "%s" command)
    (async-shell-command command buf buf)))

(defun grunt-resolve-registered-tasks ()
  "Build a list of potential Grunt tasks

The list is constructed by searching for registerTask in the
Gruntfile at `grunt-current-path'. This is incredibly fragile and
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

(defun grunt-resolve-options ()
  "Set up the arguments to the grunt binary

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
  "Return the grunt command for the specified TASK"
  (unless grunt-base-command
    (setq grunt-base-command (executable-find "grunt")))
  (mapconcat 'identity `(,grunt-base-command ,(grunt-resolve-options) ,task) " "))

(defun grunt-locate-gruntfile (&optional directory)
  "Search the current directory and upwards for a Gruntfile."
  (let ((gruntfile-dir (locate-dominating-file
                        (if directory
                            directory
                          default-directory) "Gruntfile.js")))
    (when gruntfile-dir
      (setq gruntfile-dir (file-truename gruntfile-dir)
            grunt-current-dir gruntfile-dir
            grunt-current-project (car (last (split-string gruntfile-dir "/" t)))
            grunt-current-path (format "%sGruntfile.js" gruntfile-dir)))))



(provide 'grunt)
;;; grunt.el ends here
