;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

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

(def special-variable *class-for-types* (make-hash-table :test #'equal :synchronized #t))

;; TODO: type expand
(def (function e) find-class-for-type (type)
  (or (gethash type *class-for-types*)
      (setf (gethash type *class-for-types*)
            (first (sort (iter (for (key value) :in-hashtable sb-kernel::*classoid-cells*)
                               (for class = (find-class key #f))
                               (when (and class
                                          (not (typep class 'built-in-class))
                                          (subtypep class type))
                                 (collect class)))
                         (lambda (class-1 class-2)
                           (subtypep class-2 class-1)))))))

;; TODO: type expand
(def (function e) type-instance-count-upper-bound (type)
  (etypecase type
    (symbol
     (case type
       ((nil) 0)
       (null 1)))
    (cons
     (case (first type)
       (eql 1)
       (member
        (1- (length type)))
       (integer
        (when (length= type 3)
          (1+ (- (third type) (second type)))))
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

;; TODO: type expand
(def (function e) type-instance-list (type)
  ;; TODO: sort the result with some natural sort
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
       (not nil)
       (or
        (reduce 'union (cdr type) :key 'type-instance-list))
       (and
        (reduce 'intersection (cddr type) :key 'type-instance-list :initial-value (type-instance-list (second type))))))))
