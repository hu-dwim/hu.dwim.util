;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util-base
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.util::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various utilities"
  :depends-on (:cl-fad
               :trivial-shell
               :hu.dwim.common
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar
               #+#.(cl:when (cl:find-package "SWANK") '(:and))
               ;; TODO it should be :hu.dwim.syntax-sugar once the readtable supporting package definer is added
               :hu.dwim.syntax-sugar+swank)
  :components ((:module "source"
                :components ((:file "configuration" :depends-on ("package"))
                             (:file "integer-to-string" :depends-on ("configuration"))
                             (:file "package")
                             (:file "string" :depends-on ("configuration"))
                             (:file "util" :depends-on ("configuration"))))))
