(require 'f)

;;; set up copied blatantly from
;;; http://tuxicity.se/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

;;; thanks rejeep!

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(defvar root-sandbox-path
  (f-expand "sandbox" root-test-path))

(require 'grunt (f-expand "grunt.el" root-code-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body
     (f-delete root-sandbox-path :force)))

(defmacro with-grunt-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory (f-expand "has-gruntfile" root-sandbox-path)))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path default-directory)
     (f-touch (f-expand "Gruntfile.js" default-directory))
     ,@body
     (f-delete root-sandbox-path :force)))

(ert-deftest should-locate-gruntfiles ()
  (with-sandbox
   (let* ((new-dir "has-gruntfile")
          (default-directory (f-expand new-dir root-sandbox-path)))
     (f-mkdir default-directory)
     (f-touch (f-expand "Gruntfile.js" default-directory))
     (should (string-suffix-p (format "%s/Gruntfile.js" new-dir) (grunt-locate-gruntfile))))))

(ert-deftest should-locate-gruntfiles-from-inside ()
  (with-sandbox
   (let* ((root "has-gruntfile")
          (new-dir (format "%s/and/some/nested/directories/" root))
          (default-directory (f-expand "has-gruntfile" root-sandbox-path)))
     (f-mkdir default-directory)
     (f-touch (f-expand "Gruntfile.js" default-directory))
     (should (string-suffix-p (format "%s/Gruntfile.js" root) (grunt-locate-gruntfile))))))

(ert-deftest should-fail-if-gruntfile-is-missing ()
  (with-sandbox
   (let* ((new-dir "has-gruntfile")
          (default-directory (f-expand new-dir root-sandbox-path)))
     (f-mkdir default-directory)
     (should (equal nil (grunt-locate-gruntfile))))))

(ert-deftest should-locate-current-project ()
  (with-sandbox
   (let* ((root "has-gruntfile")
          (new-dir (format "%s/and/some/nested/directories/" root))
          (default-directory (f-expand "has-gruntfile" root-sandbox-path)))
     (f-mkdir default-directory)
     (f-touch (f-expand "Gruntfile.js" default-directory))
     (grunt-locate-gruntfile)
     (should (string= root grunt-current-project)))))

(ert-deftest should-resolve-registered-tasks ()
  (with-sandbox
   (let* ((new-dir "has-gruntfile")
          (default-directory (f-expand new-dir root-sandbox-path)))
     (f-mkdir default-directory)
     (f-write "grunt.registerTask('build', ["
              'utf-8
              (f-expand "Gruntfile.js" default-directory))
     (should (string= "build" (car (grunt-resolve-registered-tasks)))))))

(ert-deftest should-include-custom-options ()
  (with-grunt-sandbox
   (let ((grunt-options "expected-option-string"))
     (grunt-locate-gruntfile)
     (should (string-match-p grunt-options (grunt-resolve-options))))))
