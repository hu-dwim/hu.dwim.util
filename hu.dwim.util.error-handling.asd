;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.error-handling
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description "Various utilities, contains code for complex error handling."
  :depends-on (:hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "error-handling")))
               (:module "integration"
                :components (#+sbcl (:file "backtrace-sbcl")))))
