;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.util.test
  :class hu.dwim.test-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.util"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.util)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "util" :depends-on ("suite"))))))
