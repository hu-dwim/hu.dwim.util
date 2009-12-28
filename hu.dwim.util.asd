;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util
  :class hu.dwim.system
  :description "Various utilities, this is the most basic system that only introduce a small number of external dependencies."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:file "compact-class" :depends-on ("mop" "util"))
                             (:file "dynamic-context" :depends-on ("util"))
                             (:file "generic-operator" :depends-on ("package"))
                             (:file "hash-table" :depends-on ("package"))
                             (:file "integer-to-string" :depends-on ("package"))
                             (:file "linear-mapping" :depends-on ("type"))
                             (:file "mop" :depends-on ("package"))
                             (:file "package")
                             (:file "source" :depends-on ("string"))
                             (:file "string" :depends-on ("package"))
                             (:file "threads-early" :depends-on ("package"))
                             (:file "type" :depends-on ("package"))
                             (:file "util" :depends-on ("package"))))))
