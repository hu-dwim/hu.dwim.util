;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various utilities"
  :depends-on (:cl-fad
               :command-line-arguments
               :hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar+swank ; TODO it should be :hu.dwim.syntax-sugar once the readtable supporting package definer is added
               :iolib.syscalls
               :trivial-shell)
  :components ((:module "source"
                :components ((:file "command-line" :depends-on ("configuration"))
                             (:file "configuration" :depends-on ("package"))
                             (:file "dynamic-context" :depends-on ("util"))
                             (:file "error-handling" :depends-on ("util"))
                             (:file "package")
                             (:file "production" :depends-on ("util"))
                             (:file "string" :depends-on ("configuration"))
                             (:file "util" :depends-on ("configuration"))))
               (:module "integration"
                :depends-on ("source")
                :components (#+sbcl (:file "sbcl")))))
