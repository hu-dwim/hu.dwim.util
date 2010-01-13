;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (function e) sum (&rest args)
  (funcall #'sum* args))

(def (function e) sum* (list &key (key #'identity) ignore-non-numbers)
  (reduce (lambda (a b)
            (if (numberp b)
                (+ a b)
                (if ignore-non-numbers
                    a
                    (error "~S is not a number" b))))
          list
          :key key
          :initial-value 0))

(def (function e) product (&rest args)
  (funcall #'product* args))

(def (function e) product* (list &key (key #'identity) ignore-non-numbers)
  (reduce (lambda (a b)
            (if (numberp b)
                (* a b)
                (if ignore-non-numbers
                    a
                    (error "~S is not a number" b))))
          list
          :key key
          :initial-value 1))

(def (function e) round* (number &optional (divisor 1))
  "CL:ROUND rounds so that .5 is rounded up for odd and down for even quotients. ROUND* always rounds .5 up."
  (bind ((result
          (if (< number 0)
              (ceiling (- number (/ divisor 2)) divisor)
              (floor (+ number (/ divisor 2)) divisor))))
    (values result (- number (* result divisor)))))
