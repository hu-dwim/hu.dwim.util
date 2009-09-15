;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Production support

(def (special-variable e) *help-command-line-option*
  '("help"
    :type boolean
    :optional #t
    :documentation "Displays this help text."))

(def (function e) process-help-command-line-argument (command-line-options command-line-arguments)
  (when-bind help (getf command-line-arguments :help)
    (command-line-arguments:show-option-help command-line-options)
    (quit-production 0)))

(def (function e) quit-production (status-code)
  #+nil
  (log.info "Quiting production image with status-code ~A" status-code)
  #+sbcl
  (sb-ext:quit :recklessly-p #t :unix-status status-code)
  #-sbcl
  (not-yet-implemented))

(def (function e) best-effort-log-error (&optional message &rest args)
  (when message
    (bind ((formatted (or (ignore-errors
                            (apply #'format nil message args))
                          (format nil "Error while formatting error message.~%  Format control: ~A~%  Argument types: ~A" message (mapcar #'type-of args)))))
      (ignore-errors
        #+nil(log.error formatted))
      (ignore-errors
        (print formatted *error-output*)))))

(def (function e) ensure-external-format-is-utf-8 ()
  #+sbcl
  (unless (eq (sb-impl::default-external-format) :utf-8)
    (cerror "Ignore" "The default external format is ~S, but UTF-8 is strongly advised! Check your $LANG env variable..."
            (sb-impl::default-external-format))))

(def (function e) load-and-eval-config-file (system-name)
  (bind ((pathname (merge-pathnames (string+ (string-downcase system-name) ".lisp") "config/"))
         (config-file-name (system-relative-pathname system-name pathname)))
    (if (cl-fad:file-exists-p config-file-name)
        (with-local-readtable
          (hu.dwim.asdf::maybe-funcall-setup-readtable-function system-name)
          (load config-file-name)
          config-file-name)
        nil)))

(def (function e) disable-debugger ()
  #+sbcl(sb-ext:disable-debugger)
  ;; TODO delme?
  #+nil
  (bind ((hook (lambda (condition hook)
                 (declare (ignore hook))
                 (ignore-errors
                   (log-error-with-backtrace project-logger condition :level '+fatal+))
                 #+sbcl
                 (best-effort-log-error "~&~@<unhandled ~S~@[ in thread ~S~]: ~2I~_~A~:>~2%"
                                        (type-of condition) sb-thread:*current-thread* condition)
                 (quit-production 1))))
    #+sbcl
    (setf sb-ext:*invoke-debugger-hook* hook)
    (setf *debugger-hook* hook))
  #+nil(log.info "Disabled debugger"))

(def (function e) start-swank-server (port)
  #+nil
  (log.info "Starting Swank server on port ~A..." port)
  (bind (((:values started? error) (ignore-errors
                                     (with-simple-restart (continue "Ok, go on without a Swank server")
                                       (let ((swank::*loopback-interface* "127.0.0.1"))
                                         (swank:create-server :port port
                                                              :style :spawn
                                                              :dont-close #t
                                                              :coding-system "utf-8-unix")))
                                     #t)))
    (if started?
        (progn
          #+nil
          (log.info "Swank server has been started"))
        (progn
          #+nil
          (log.error "Swank server failed to start due to: ~A" error)
          (warn "Swank server failed to start due to: ~A" error)))))

(def (with-macro e) with-pid-file (pathname)
  (bind ((pid-file-has-been-created? #f))
    (labels ((cleanup-pid-file ()
               (when pid-file-has-been-created?
                 (unless (ignore-errors
                           (delete-file pathname)
                           #t)
                   #+nil(log.warn "Failed to remove pid file ~S" pathname))))
             (startup-signal-handler (signal code scp)
               (declare (ignore signal code scp))
               #+nil(log.info "SIGTERM/SIGINT was received while starting up, exiting abnormally")
               (cleanup-pid-file)
               (quit-production 2)))
      (unwind-protect
           (progn
             #+sbcl
             (sb-sys:enable-interrupt sb-unix:sigterm #'startup-signal-handler)
             #+sbcl
             (sb-sys:enable-interrupt sb-unix:sigint #'startup-signal-handler)
             #+nil
             (log.info "Temporary startup signal handlers are installed")
             (when pathname
               #+nil(log.info "Writing pid file ~S" pathname)
               (when (cl-fad:file-exists-p pathname)
                 (bind ((pid (parse-integer (read-file-into-string pathname))))
                   (if (ignore-errors
                         (isys:%sys-kill pid 0)
                         #t)
                       (error "Pid file ~S already exists and points to a running process ~S" pathname pid)
                       (progn
                         #+nil(log.warn "Deleting stale pid file ~S pointing to non-existent process ~S" pathname pid)
                         (delete-file pathname)))))
               (handler-bind ((serious-condition
                               (lambda (error)
                                 (best-effort-log-error "Failed to write pid file ~S because: ~A" pathname error)
                                 (quit-production 1))))
                 (with-open-file (pid-stream pathname :direction :output
                                             :element-type 'character
                                             :if-exists :error)
                   (princ (isys:%sys-getpid) pid-stream)
                   (terpri pid-stream)))
               (setf pid-file-has-been-created? #t)
               #+nil(log.info "PID file is ~S, PID is ~A" pathname (isys:%sys-getpid)))
             (-body-))
        (cleanup-pid-file)))))

(def (with-macro e) with-save-core-and-die-restart ()
  (restart-case
      (-body-)
    (save-core-and-die ()
      :report "Save image to /tmp/sbcl.core and die"
      #+sbcl
      (mapcar
       (lambda (thread)
         (unless (eq thread sb-thread:*current-thread*)
           (sb-thread:terminate-thread thread)))
       (sb-thread:list-all-threads))
      (save-image "/tmp/sbcl.core"))))

(def (function e) save-image (file-name &rest args &key &allow-other-keys)
  #+sbcl
  (apply #'sb-ext:save-lisp-and-die file-name args))
