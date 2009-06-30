;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util.system)

(defpackage :hu.dwim.util
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :anaphora
        :iterate
        :defclass-star
        :closer-mop
        :cl-def
        :cl-syntax-sugar))

(defpackage :hu.dwim.util.user
  (:use :common-lisp
        :hu.dwim.util))
