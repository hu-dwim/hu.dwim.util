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
  :depends-on (:hu.dwim.util-base
               :command-line-arguments
               :hu.dwim.syntax-sugar+swank ; TODO it should be :hu.dwim.syntax-sugar once the readtable supporting package definer is added
               :iolib.syscalls)
  :components ((:module "source"
                :components ((:file "command-line")
                             (:file "dynamic-context")
                             (:file "error-handling")
                             (:file "production")))
               (:module "integration"
                :depends-on ("source")
                :components (#+sbcl (:file "sbcl")))))
