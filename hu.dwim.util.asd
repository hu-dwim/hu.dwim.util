;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.util
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various utilities"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar+swank)
  :components ((:module "source"
                :components ((:file "configuration" :depends-on ("package"))
                             (:file "dynamic-context" :depends-on ("util"))
                             (:file "error-handling" :depends-on ("util"))
                             (:file "package")
                             (:file "util" :depends-on ("configuration"))))
               (:module "integration"
                :depends-on ("source")
                :components (#+sbcl (:file "sbcl")))))
