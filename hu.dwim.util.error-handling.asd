;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.error-handling
  :class hu.dwim.system
  :description "Various utilities used by the dwim.hu team. Contains code for complex error handling."
  :depends-on (:hu.dwim.util
               :hu.dwim.defclass-star+hu.dwim.def)
  :components ((:module "source"
                :components ((:file "error-handling")))
               (:module "integration"
                :components (#+sbcl (:file "backtrace-sbcl")))))
