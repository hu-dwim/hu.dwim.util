;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

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
;;; Path related

(def (function e) system-relative-pathname (system path)
  (merge-pathnames path (asdf:component-pathname (asdf:find-system system))))

;;;;;;
;;; Development marks

(def (function e) not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate-string "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(def (function e) operation-not-supported (&optional (datum "Operation not supported." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate-string "Operation not supported: " datum)))
  (apply #'error datum args))

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
;;; CLOS MOP related

(def (function ioe) find-slot (class-or-name slot-name &key (otherwise nil otherwise?))
  (or (find slot-name
            (the list
              (class-slots (if (symbolp class-or-name)
                               (find-class class-or-name)
                               class-or-name)))
            :key 'slot-definition-name
            :test 'eq)
      (handle-otherwise (if otherwise?
                            otherwise
                            (list :error "Cannot find slot ~A in class ~A" slot-name class-or-name)))))

;;;;;;
;;; String related

(def (function eo) concatenate-string (&rest args)
  ;; don't inline, otherwise the compiler macro is kicked
  (apply #'concatenate 'string args))

(def compiler-macro concatenate-string (&rest args)
  `(concatenate 'string ,@args))
