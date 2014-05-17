;;; grunt.el --- Some glue to stick Emacs and Gruntfiles together

;; Copyright (C) 2014  Daniel Gempesaw

;; Author: Daniel Gempesaw <dgempesaw@sharecare.com>
;; Keywords: convenience, grunt
;; Version: 0.0.1
;; Package-Requires: ((f "0.16.2") (dash "2.6.0"))
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

;;; Code:

(require 'f)
(require 'dash)

(defvar grunt-base-command (executable-find "grunt")
  "The base grunt command; you may have to fix this if `grunt'
  isn't in your PATH")

(defvar grunt-options ""
  "Additional options that will be passed to grunt when invoked
  through `grunt-exec'")

(defvar grunt-current-path ""
  "Path to the current gruntfile.

We'll try to find this on our own.")

(defvar grunt-current-dir ""
  "Path to the directory of the current gruntfile.

We'll try to find this on our own.")

(defvar grunt-current-project ""
  "Name of the current project in which the Gruntfile is found")

;;;###autoload
(defun grunt-exec (&optional pfx)
  "Invoke this while in your project and it will suggest
registered tasks. You can also manually enter in a specific task
that isn't registered."
  (interactive "P")
  (if (eq nil (grunt-locate-gruntfile))
      (error "Sorry, we couldn't find a gruntfile. Consider setting `grunt-current-path' manually?")
    )
  (let* ((task (ido-completing-read "Execute which task: " (grunt-resolve-registered-tasks) nil nil))
         (command (grunt--command task))
         (buf (get-buffer-create
               (format "*grunt-%s*<%s>" task grunt-current-project)))
         (default-directory grunt-current-dir))
    (async-shell-command command buf buf)))

(defun grunt-resolve-registered-tasks ()
  "Build an incomplete list of potential tasks by searching for
registerTask in the Gruntfile. This is incredibly fragile and
will break on something as simple as an alternate quoting scheme
or indentation, and it _only_ supports manually registered
tasks."
  (let* ((contents (with-temp-buffer
                     (insert-file-contents grunt-current-path)
                     (split-string (buffer-string) "\n"))))
    (-map (lambda (line)
            (string-match "registerTask('\\(.*?\\)'" line)
            (match-string 1 line))
          (-filter (lambda (line)
                     (string-match-p "registerTask('" line))
                   contents))))

(defun grunt-resolve-options ()
  "Set up the arguments to the grunt binary so that we can call
it from any directory with any gruntfile."
  (setq grunt-options
        (let ((opts))
          (mapc
           (lambda (item)
             (setq opts (format "--%s %s" (car item) (cadr item))))
           `(("base" ,grunt-current-dir)
             ("gruntfile" ,grunt-current-path)))
          opts)))

(defun grunt--command (task)
  "Return the grunt command for the specified task, ready to be
executed."
  (grunt-resolve-options)
  (mapconcat 'identity `(,grunt-base-command ,grunt-options ,task) " "))

(defun grunt-locate-gruntfile (&optional directory)
  "Search the current directory and upwards for a Gruntfile."
  (let ((gruntfile-dir (f--traverse-upwards
                        (f-exists? (format "%s/Gruntfile.js" it))
                        (f-expand (if directory
                                      directory
                                    (cadr (split-string (pwd) " " t)))))))
    (if gruntfile-dir
        (progn
          (setq grunt-current-path (format "%s/Gruntfile.js" gruntfile-dir)
                grunt-current-dir gruntfile-dir
                grunt-current-project (cadr (reverse (split-string grunt-current-path "/"))))
          grunt-current-path)
      nil)))

(provide 'grunt)
;;; grunt.el ends here
