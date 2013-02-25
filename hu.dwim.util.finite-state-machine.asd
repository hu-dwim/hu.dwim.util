;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.finite-state-machine
  :defsystem-depends-on (:hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description ""
  :depends-on (:hu.dwim.def.namespace
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "finite-state-machine")))))
