(require 'f)
(require 'noflet)

;;; set up copied blatantly from
;;; http://tuxicity.se/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

;;; thanks rejeep!

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(defvar root-sandbox-path
  (f-expand "sandbox" root-test-path))

(defvar mock-gruntfile-dir "has-gruntfile")

(defun mock-grunt-help ()
    "Return stub data for the grunt-help command."
    (with-temp-buffer
      (insert-file-contents (f-expand "grunt-help.txt" root-test-path))
      (buffer-string)))

(require 'grunt (f-expand "grunt.el" root-code-path))

(if (not (boundp 'string-suffix-p))
    (defun string-suffix-p (str1 str2 &optional ignore-case)
      (let ((begin2 (- (length str2) (length str1)))
            (end2 (length str2)))
        (when (< begin2 0) (setq begin2 0))
        (eq t (compare-strings str1 nil nil
                               str2 begin2 end2
                               ignore-case)))))

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
  `(let ((default-directory (f-expand mock-gruntfile-dir root-sandbox-path))
         (grunt-current-path "")
         (grunt-current-dir "")
         (grunt-current-project ""))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path default-directory)
     (f-touch (f-expand "Gruntfile.js" default-directory))
     (grunt-locate-gruntfile)
     ,@body
     (f-delete root-sandbox-path :force)))

(ert-deftest should-find-grunt-binary-in-path ()
  (should (not (eq nil grunt-base-command))))

(ert-deftest should-locate-gruntfiles ()
  (with-grunt-sandbox
   (should (string-suffix-p (format "%s/Gruntfile.js" mock-gruntfile-dir) (grunt-locate-gruntfile)))))

(ert-deftest should-locate-gruntfiles-from-inside ()
  (with-grunt-sandbox
   (let* ((root-dir (f-expand mock-gruntfile-dir root-sandbox-path))
          (nested-dir (f-expand "nested" root-dir))
          (default-directory nested-dir))
     (f-mkdir root-dir nested-dir)
     (should (string-suffix-p (format "%s/Gruntfile.js" mock-gruntfile-dir) (grunt-locate-gruntfile))))))

(ert-deftest should-fail-if-gruntfile-is-missing ()
  (with-sandbox
   (should (equal nil (grunt-locate-gruntfile)))))

(ert-deftest should-locate-current-project ()
  (with-grunt-sandbox
   (should (string= mock-gruntfile-dir grunt-current-project))))

(ert-deftest should-resolve-registered-tasks ()
  (with-grunt-sandbox
   (noflet ((grunt-get-help () (mock-grunt-help)))
           (should (string= "task" (car (grunt-resolve-registered-tasks))))
           (should (string= "build" (cadr (grunt-resolve-registered-tasks)))))))

(ert-deftest should-include-custom-options ()
  (with-grunt-sandbox
   (let ((grunt-options "expected-option-string"))
     (should (string-match-p grunt-options (grunt-resolve-options))))))

(ert-deftest should-construct-valid-command ()
  (with-grunt-sandbox
   (let ((cmd (grunt--command "task")))
     (should (string-match-p " --base /.*has-gruntfile" cmd))
     (should (string-match-p " --gruntfile /.*Gruntfile.js" cmd))
     (should (string-suffix-p " task" cmd))
     (should (string-match-p "grunt " cmd)))))

(ert-deftest should-execute-grunt-commands ()
  (with-grunt-sandbox
   (noflet ((ido-completing-read (&rest any) "build")
            (async-shell-command (&rest args) args))
     (let* ((args (grunt-exec))
           (cmd (car args))
           (buf (buffer-name (cadr args))))
       (should (string-suffix-p "build" cmd))
       (should (string= "*grunt-build*<has-gruntfile>" buf))))))

(ert-deftest should-kill-existing-buffer ()
  (with-grunt-sandbox
   (noflet ((ido-completing-read (&rest any) "build"))
     (grunt-exec)
     (grunt-exec)
     (should t))))

(ert-deftest should-set-column-width ()
  (with-grunt-sandbox
   (let ((process-resized 0))
     (noflet ((ido-completing-read (&rest any) "build")
              (async-shell-command (&rest args) args)
              (grunt--set-process-dimensions (buf)
                (setq process-resized (1+ process-resized))))
       (grunt-exec)
       (should process-resized)))))
