;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.test
  :class hu.dwim.test-system
  :description "Test suite for hu.dwim.util"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.def+swank
               :hu.dwim.stefil+swank
               :hu.dwim.util.all)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "util" :depends-on ("suite"))))))
