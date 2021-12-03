;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.util
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.syntax-sugar)
  (:export
   #:host
   #:port
   #:query
   #:query-of
   #:otherwise
   #:otherwise?)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-readtime-wrapper-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)))

(def package :hu.dwim.util/error-reports
  (:use :common-lisp)
  (:documentation "This package is bound to *package* while constructing error messages. Doing so makes ~S format output print fully qualified packages."))
