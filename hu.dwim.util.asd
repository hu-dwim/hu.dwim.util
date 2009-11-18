;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.util::setup-readtable"
  :description "Various utilities used by the dwim.hu team. This is the most basic system that only introduce a small number of external dependencies."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:file "integer-to-string" :depends-on ("package"))
                             (:file "package")
                             (:file "string" :depends-on ("package"))
                             (:file "util" :depends-on ("package"))
                             (:file "dynamic-context" :depends-on ("util"))))))
