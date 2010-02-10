;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (constant e) +top-type+ t)

(def (constant e) +bottom-type+ nil)

;; TODO promote these to alexandria?
(def (type e) function-name ()
  '(and (or symbol
            (cons (eql setf)
                  (cons (and symbol (not (member nil t)))
                        null)))
        (not (member null t))))

(def (type e) function-designator ()
  '(or function function-name))

#||

if we strictly followed CLHS, then it should be the following:

(def (type e) function-designator ()
  '(or function '(and symbol (not (member nil t)))))

(def (type e) extended-function-designator ()
  '(or function function-name))

||#

;; TODO use def namespace if possible to make thread-safety portable
;; TODO the name CLASS-FOR-TYPE should be more specific. what kind of class?
;; (def (namespace :test 'equal :finder-name %find-class-for-type) class-for-type)
(def special-variable *class-for-types* (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl #t)) ;; TODO THL #+allegro?

;; TODO: type expand
;; TODO add error on failure or &key otherwise
(def (function e) find-class-for-type (type)
  (or (gethash type *class-for-types*)
      (setf (gethash type *class-for-types*)
            (or (when (symbolp type)
                  (find-class type nil))
		#+sbcl ;; TODO THL #+allegro?
                (first (sort (iter (for (key value) :in-hashtable sb-kernel::*classoid-cells*)
                                   (for class = (find-class key #f))
                                   (when (and class
                                              (not (typep class 'built-in-class))
                                              (subtypep class type))
                                     (collect class)))
                             (lambda (class-1 class-2)
                               (subtypep class-2 class-1))))))))

(def (function e) type-instance-count-upper-bound (type)
  #+sbcl
  (setf type (sb-kernel::type-expand type))
  (etypecase type
    (symbol
     (case type
       ((nil) 0)
       (null 1)
       (base-char 128)
       (boolean 2)
       (single-float (expt 2 32))
       (fixnum #.(expt 2 (integer-length most-positive-fixnum)))))
    (cons
     (case (first type)
       (eql 1)
       (member
        (1- (length type)))
       (integer
        (when (length= type 3)
          (1+ (- (third type) (second type)))))
       ((signed-byte unsigned-byte)
        (expt 2 (second type)))
       (simple-base-string
        (expt 128 (second type)))
       (not nil)
       (or
        (iter (for element :in (cdr type))
              (aif (type-instance-count-upper-bound element)
                   (summing it)
                   (return nil))))
       (and
        (iter (for element :in (cdr type))
              (awhen (type-instance-count-upper-bound element)
                (minimizing it))))))))

;; TODO: sort the result with some natural sort
(def (function e) type-instance-list (type)
  #+sbcl
  (setf type (sb-kernel::type-expand type))
  (etypecase type
    (symbol
     (case type
       ((nil) nil)
       (null '(nil))))
    (cons
     (case (first type)
       (eql
        (second type))
       (member
        (cdr type))
       (integer
        (iter (for i :from (second type) :to (third type))
              (collect i)))
       (unsigned-byte
        (iter (for i :from 0 :below (expt 2 (second type)))
              (collect i)))
       (signed-byte
        (iter (with limit = (expt 2 (1- (second type))))
              (for i :from (- limit) :below limit)
              (collect i)))
       (not nil)
       (or
        (reduce 'union (cdr type) :key 'type-instance-list))
       (and
        (reduce 'intersection (cddr type) :key 'type-instance-list :initial-value (type-instance-list (second type))))))))
