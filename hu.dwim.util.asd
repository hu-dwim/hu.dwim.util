;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar)
  (asdf:oos 'asdf:load-op :asdf-system-connections))

(defpackage #:hu.dwim.util.system
  (:use :common-lisp :asdf :cl-syntax-sugar)

  (:export #:*load-as-production-p*
           #:project-relative-pathname
           ))

(in-package #:hu.dwim.util.system)

(defvar *load-as-production-p* t)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :hu.dwim.util))))

(defsystem :hu.dwim.util
  :version "1.0"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "hu.dwim.util::setup-readtable"
  :depends-on (:metabang-bind
               :alexandria
               :anaphora
               :iterate
               :defclass-star
               :closer-mop
               :cl-def
               :cl-syntax-sugar)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "configuration" :depends-on ("package"))
                 (:file "util" :depends-on ("configuration"))
                 (:file "error-handling" :depends-on ("util"))
                 (:module "integration"
                  :components (#+sbcl (:file "sbcl-integration")))))))

(defmethod perform ((op test-op) (system (eql (find-system :hu.dwim.util))))
  (operate 'load-op :hu.dwim.util.test)
  (in-package :hu.dwim.util.test)
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :hu.dwim.util))))
  nil)

(defsystem-connection cl-dwim-util-and-swank
  :requires (:hu.dwim.util :swank)
  :components
  ((:module "src"
            :components ((:module "integration"
                                  :components ((:file "swank-integration")))))))
