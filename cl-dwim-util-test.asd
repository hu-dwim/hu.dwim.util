;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-dwim-util)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar))

(in-package #:hu.dwim.util.system)

(setf *load-as-production-p* nil)

(defsystem :cl-dwim-util-test
  :description "Tests for cl-dwim-util."
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "hu.dwim.util::setup-readtable"
  :depends-on (:metabang-bind
               :iterate
               :stefil
               :cl-def
               :cl-syntax-sugar
               :cl-dwim-util)
  :components
  ((:module :test
    :components ((:file "package")
                 (:file "suite" :depends-on ("package"))
                 (:file "util" :depends-on ("suite"))))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-dwim-util-test))))
  (in-package :hu.dwim.util.test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing"))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-dwim-util-test))))
  nil)
