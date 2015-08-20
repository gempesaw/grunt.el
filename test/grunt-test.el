(require 'f)
(require 'noflet)
(require 'ert-async)

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
         (grunt-current-tasks-cache nil)
         (grunt-current-path "")
         (grunt-current-dir "")
         (grunt-current-project "")
         (grunt-verbose nil))
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

(ert-deftest should-resolve-registered-tasks-via-shell ()
  (with-grunt-sandbox
   (noflet ((grunt--get-help () (mock-grunt-help)))
     (let ((result (grunt-resolve-registered-tasks)))
       (should (string= "task" (car result)))
       (should (string= "build" (cadr result)))
       (should (eq 2 (length result)))))))

(ert-deftest should-use-valid-grunt-help-command ()
  (with-grunt-sandbox
   (let ((grunt-base-command nil)
         (grunt-help-command (format "%s --help --no-color" grunt-base-command)))
     ;; pretend like we couldn't resolve grunt-base-command, and that
     ;; grunt-help-command is in a similar quagmire
     (noflet ((shell-command-to-string (&rest args) (car args)))
       (should-not (string-match-p " nil --help" (grunt--get-help)))))))

(ert-deftest should-throw-when-missing-grunt-binary ()
  (with-grunt-sandbox
   (noflet ((executable-find (binary) nil))
     (let ((grunt-base-command nil))
       (should-error (grunt--command))))))

(ert-deftest should-resolve-registered-tasks-via-regex ()
  (with-grunt-sandbox
   (let ((grunt-show-all-tasks nil))
     (f-write "grunt.registerTask('test', ["
              'utf-8
              (f-expand "Gruntfile.js" default-directory))
     (should (string= "test" (car (grunt-resolve-registered-tasks))))

     (f-write "grunt.registerTask(\"test2\""
              'utf-8
              (f-expand "Gruntfile.js" default-directory))
     (should (string= "test2" (car (grunt-resolve-registered-tasks)))))))

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
   (noflet ((completing-read (&rest any) "build")
            (start-process-shell-command (&rest args) args)
            (set-process-filter (p f) nil))
     (let* ((args (grunt-exec))
            (cmd (cadr (cdr args)))
            (buf (buffer-name (cadr args))))
       (should (string-suffix-p "build" cmd))
       (should (string= "*grunt-build*<has-gruntfile>" buf))))))

(ert-deftest should-erase-contents-of-buffer ()
  (with-grunt-sandbox
      (let ((called nil)
            (grunt-kill-existing-buffer nil))
        (noflet ((completing-read (&rest any) "build")
                 (erase-buffer () (setq called t)))
          (grunt-exec)
          (should called)))))

(ert-deftest should-kill-existing-buffer ()
  (with-grunt-sandbox
   (noflet ((completing-read (&rest any) "build"))
     (grunt-exec)
     (grunt-exec)
     (should t))))

(ert-deftest should-set-column-width ()
  (with-grunt-sandbox
   (let ((process-resized 0))
     (noflet ((completing-read (&rest any) "build")
              (grunt--set-process-dimensions (buf)
                                             (setq process-resized (1+ process-resized))))
       (grunt-exec)
       (should process-resized)))))

(ert-deftest should-set-process-to-read-only ()
  (with-grunt-sandbox
   (noflet ((completing-read (&rest any) "build"))
     (grunt-exec)
     (set-buffer "*grunt-build*<has-gruntfile>")
     (should buffer-read-only))))

(ert-deftest should-set-process-filter-to-apply-ansi-color ()
  (with-grunt-sandbox
   (let ((called nil))
     (noflet ((completing-read (&rest any) "build")
              (set-process-filter (p f) (setq called t)))
             (grunt-exec)
             (should called)))))

(ert-deftest should-apply-ansi-color-to-the-string ()
  (with-grunt-sandbox
   (let ((called nil))
     (noflet ((completing-read (&rest any) "build")
              (ansi-color-apply-on-region () (setq called t)))
      (grunt-exec)
      ;; (should called)
      ))))

(ert-deftest should-not-clear-cache-with-same-gruntfile ()
  (with-grunt-sandbox
   (let ((grunt-cache-tasks t)
         (cleared-cache nil))
     (noflet ((completing-read (&rest any) "build")
              (grunt-clear-tasks-cache () (setq cleared-cache t)))
       (dotimes (i 2) (grunt-exec))
       (should (not cleared-cache))))))

(ert-deftest should-not-clear-cache-when-caching-disabled ()
  (noflet ((completing-read (&rest any) "build")
           (grunt-clear-tasks-cache () (setq cleared-cache t)))
    (let ((grunt-cache-tasks nil)
          (cleared-cache nil))
      (with-grunt-sandbox
       (grunt-exec))
      (let ((mock-gruntfile-dir "different-gruntfile"))
        (with-grunt-sandbox
         (grunt-exec)
         (should (not cleared-cache)))))))

(ert-deftest should-clear-cache-when-gruntfile-changes ()
  (noflet ((completing-read (&rest any) "build")
           (grunt-clear-tasks-cache () (setq cleared-cache t)))
    (let ((grunt-cache-tasks t)
          (cleared-cache nil))
      (with-grunt-sandbox
       (grunt-exec))
      (let ((mock-gruntfile-dir "different-gruntfile"))
        (with-grunt-sandbox
         (grunt-exec)
         (should cleared-cache))))))

(ert-deftest should-clear-cache-on-prefix-arg ()
  (with-grunt-sandbox
   (let ((cleared-cache nil))
     (noflet ((completing-read (&rest any) "build")
              (grunt-clear-tasks-cache () (setq cleared-cache t)))
       (grunt-exec 4)
       (should cleared-cache)))))

(ert-deftest should-set-the-previous-task ()
  (with-grunt-sandbox
   (noflet ((completing-read (&rest any) "build"))
     (grunt-exec)
     (should (string= "build" grunt-previous-task)))))

(ert-deftest should-set-the-buffer-local-task ()
  (with-grunt-sandbox
   (noflet ((completing-read (&rest any) "build"))
     (grunt-exec)
     (set-buffer "*grunt-build*<has-gruntfile>")
     (should (string= "build" (buffer-local-value 'grunt-buffer-task (current-buffer)))))))

(ert-deftest-async should-save-excursion-in-the-process-filter (done)
  (with-grunt-sandbox
   (let ((grunt-scroll-output nil))
     (noflet ((completing-read (&rest any) "build"))
       (let ((proc (grunt-exec)))
         (with-current-buffer (process-buffer proc)
           (goto-char (point-min))
           (set-process-sentinel proc
            (lambda (&rest any)
                (should (eq (point) (point-min)))
                (funcall done)))))))))

(ert-deftest-async should-not-save-excursion-in-the-process-filter (done)
  (with-grunt-sandbox
   (let ((grunt-scroll-output t))
     (noflet ((completing-read (&rest any) "build"))
       (let ((proc (grunt-exec)))
         (with-current-buffer (process-buffer proc)
           (goto-char (point-min))
           (set-process-sentinel proc
            (lambda (&rest any) 
                (should (eq (point) (point-max)))
                (funcall done)))))))))
