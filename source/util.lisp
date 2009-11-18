;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Misc

(def (function e) enable-standard-hu.dwim-syntaxes ()
  "This function sets up the common readtable modifications we (http://dwim.hu) use in almost all of our projects. Some projects enable more, but this is available almost everywhere."
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-feature-cond-syntax))

(def (macro e) eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(def (constant e) +no-error-status-code+ 0)

(def (function e) quit (status-code)
  #+nil
  (log.info "Quiting production image with status-code ~A" status-code)
  #+sbcl
  (sb-ext:quit :recklessly-p #t :unix-status status-code)
  #-sbcl
  (not-yet-implemented))

(def (function e) guess-file-type (pathname)
  ;; TODO: KLUDGE: not portable, etc.
  (bind ((type (pathname-type pathname)))
    (switch (type :test #'string=)
      ("asd" :asd)
      ("lisp" :lisp)
      (t
       ;; TODO this is a bit too heavy-weight, both on dependencies and runtime implications...
       #+nil
       (bind ((result (trivial-shell:shell-command (concatenate 'string "file " (namestring pathname)))))
         (cond ((search "text" result) :text)
               (t :binary)))
       :binary))))

;;;;;;
;;; Anaphoric extensions

(def (macro e) if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(def (macro e) when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(def (macro e) cond-bind (var &body clauses)
  "Just like COND but VAR will be bound to the result of the
  condition in the clause when executing the body of the clause."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
           (progn ,@(if body body (list var)))
           (cond-bind ,var ,@others)))
      nil))

(def (macro e) prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

;;;;;;
;;; Binding related

(def (macro e) rebind (bindings &body body)
  `(let ,(loop :for symbol-name :in bindings
               :collect (list symbol-name symbol-name))
     ,@body))

;;;;;;
;;; Development marks

(def (function e) not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate 'string "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(def (function e) operation-not-supported (&optional (datum "Operation not supported." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate 'string "Operation not supported: " datum)))
  (apply #'error datum args))

;;;;;;
;;; Thread name

(def (macro e) with-thread-name (name &body body)
  (declare (ignorable name))
  #*((:sbcl
      (with-unique-names (thread previous-name)
        `(let* ((,thread sb-thread:*current-thread*)
                (,previous-name (sb-thread:thread-name ,thread)))
           (setf (sb-thread:thread-name ,thread)
                 (concatenate 'string ,previous-name ,name))
           (unwind-protect
                (progn
                  ,@body)
             (setf (sb-thread:thread-name ,thread) ,previous-name)))))
     (t
      `(progn
         ,@body))))

(def (macro e) with-thread-activity-description ((name) &body body)
  "This is a debugging helper tool. Information provided here may show up in backtraces unless compiled without the debug helpers."
  `(with-thread-name ,(if (stringp name) ;; TODO this should be a compiler-macro in sbcl itself
                          (concatenate 'string " / " name)
                          `(concatenate 'string " / " (string ,name)))
     ,@body))

;;;;;;
;;; Otherwise

(def (function ioe) handle-otherwise (otherwise)
  (cond
    ((eq otherwise :error)
     (error "Otherwise assertion failed"))
    ((and (consp otherwise)
          (member (first otherwise) '(:error :warn) :test #'eq))
     (assert (not (null (rest otherwise))))
     (apply (ecase (first otherwise)
              (:error #'error)
              (:warn  #'warn))
            (rest otherwise)))
    ((functionp otherwise)
     (funcall otherwise))
    (t
     otherwise)))

;;;;;;
;;;; Sequence related

(def (function e) ensure-sequence (thing)
  (if (typep thing 'sequence)
      thing
      (list thing)))

(def (function e) collect-if (predicate sequence)
  "Collect elements from SEQUENCE for which the PREDICATE is true."
  (remove-if (complement predicate) sequence))

(def (function e) collect-if-typep (type sequence)
  "Collect elements from SEQUENCE with the given TYPE."
  (collect-if (lambda (element) (typep element type)) sequence))

;;;;;;
;;; Place related

(def macro notf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect `(not ,place)))))

(def (macro e) clearf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect nil))))
