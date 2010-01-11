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
