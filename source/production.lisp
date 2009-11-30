;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Production support

(def (function e) ensure-utf-8-external-format ()
  #+sbcl
  (unless (eq (sb-impl::default-external-format) :utf-8)
    (cerror "Ignore" "The default external format is ~S, but UTF-8 is strongly advised! Check your $LANG env variable..."
            (sb-impl::default-external-format))))

(def (function e) load-and-eval-config-file (system-name)
  (bind ((pathname (merge-pathnames (string+ (string-downcase system-name) ".lisp") "config/"))
         (config-file-name (system-relative-pathname system-name pathname)))
    (if (cl-fad:file-exists-p config-file-name)
        (with-local-readtable
          (bind ((*package* (find-package :hu.dwim.common)))
            ;; load using the special in-package symbol that sets up the readtable based on what the package says
            (load config-file-name))
          config-file-name)
        nil)))

(def function disabled-debugger-hook (condition &optional logger)
  (bind ((message (or (ignore-errors
                        (build-backtrace-string condition :message "Unhandled error while debugger is disabled, quitting..." :timestamp (get-universal-time)))
                      "Err, complete meltdown in DISABLED-DEBUGGER-HOOK. Sorry, no more clue is available...")))
    (when message
      (ignore-errors
        (write-string message *error-output*)
        (terpri *error-output*))
      (when logger
        (hu.dwim.logger:handle-log-message logger message hu.dwim.logger:+fatal+))))
  (quit 3))

(def (function e) disable-debugger (&optional logger)
  (declare (ignorable logger))
  #*((:sbcl (flet ((call-disabled-debugger-hook (condition hook)
                     (declare (ignore hook))
                     (disabled-debugger-hook condition logger)))
              (sb-ext:disable-debugger) ; so that we unconditionally disable LDB
              (setf sb-ext:*invoke-debugger-hook* #'call-disabled-debugger-hook)
              (setf *debugger-hook* #'call-disabled-debugger-hook)))
     (t #.(warn "~S is not fully implemented for your implementation which may lead to undesired consequences" 'disable-debugger)))
  (format *debug-io* "Disabled debugger~%"))

(def (function e) start-swank-server (port)
  (format *debug-io* "Starting Swank server on port ~A...~%" port)
  (bind (((:values started? error) (ignore-errors
                                     (with-simple-restart (continue "Ok, go on without a Swank server")
                                       (let ((swank::*loopback-interface* "127.0.0.1"))
                                         (swank:create-server :port port
                                                              :style :spawn
                                                              :dont-close #t
                                                              :coding-system "utf-8-unix")))
                                     #t)))
    (if started?
        (format *debug-io* "Swank server has been started~%")
        (warn "Swank server failed to start due to: ~A" error))))

(def (with-macro* e) with-pid-file-logic (pathname &key optional)
  (check-type pathname (or null pathname string))
  (assert (or optional pathname))
  (bind ((pid-file-has-been-created? #f))
    (labels ((cleanup-pid-file ()
               (when pid-file-has-been-created?
                 (unless (ignore-errors
                           (delete-file pathname)
                           #t)
                   (format *debug-io* "Failed to remove pid file ~S~%" pathname))))
             (startup-signal-handler (signal code scp)
               (declare (ignore signal code scp))
               (format *debug-io* "SIGTERM/SIGINT was received while starting up, exiting abnormally~%")
               (cleanup-pid-file)
               (quit 2)))
      (unwind-protect
           (progn
             #*((:sbcl
                 (sb-sys:enable-interrupt sb-unix:sigterm #'startup-signal-handler)
                 (sb-sys:enable-interrupt sb-unix:sigint #'startup-signal-handler)
                 (format *debug-io* "Temporary startup signal handlers are installed~%"))
                (t (warn "No support for installing signal handlers on your implementation, stale PID files may remain")))
             (when pathname
               (format *debug-io* "Writing pid file ~S~%" pathname)
               (when (cl-fad:file-exists-p pathname)
                 (bind ((pid (parse-integer (read-file-into-string pathname))))
                   (if (ignore-errors
                         (isys:%sys-kill pid 0)
                         #t)
                       (error "PID file ~S already exists and points to a running process ~S" pathname pid)
                       (progn
                         (format *debug-io* "Deleting stale PID file ~S pointing to non-existent PID ~S~%" pathname pid)
                         (delete-file pathname)))))
               (handler-bind ((serious-condition
                               (lambda (error)
                                 (best-effort-log-error "Failed to write PID file ~S because: ~A" pathname error)
                                 (quit 1))))
                 (with-open-file (pid-stream pathname :direction :output
                                             :element-type 'character
                                             :if-exists :error)
                   (princ (isys:%sys-getpid) pid-stream)))
               (setf pid-file-has-been-created? #t)
               (format *debug-io* "PID file is ~S, PID is ~A~%" pathname (isys:%sys-getpid)))
             (-body-))
        (cleanup-pid-file)))))

(def (with-macro e) with-save-core-and-die-restart ()
  (restart-case
      (-body-)
    #+sbcl
    (save-core-and-die ()
      :report "Save image to /tmp/sbcl.core and die"
      (mapcar
       (lambda (thread)
         (unless (eq thread sb-thread:*current-thread*)
           (sb-thread:terminate-thread thread)))
       (sb-thread:list-all-threads))
      (sb-ext:save-lisp-and-die "/tmp/sbcl.core"))))

(def (with-macro e) with-standard-toplevel-restarts ()
  (restart-case
      (with-save-core-and-die-restart
        (-body-))
    (abort nil
      :report (lambda (stream)
                (format stream "Give up starting the image and quit the VM process with exit code 2"))
      (quit 2))))
