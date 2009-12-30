;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.all
  :class hu.dwim.system
  :description "Various utilities, contains everything and therefore it's heavy on external dependencies."
  :depends-on (:hu.dwim.util
               :hu.dwim.util.error-handling
               :hu.dwim.util.error-handling+swank
               :hu.dwim.util.i18n
               :hu.dwim.util.mop
               :hu.dwim.util.production
               :hu.dwim.util.production+swank
               :hu.dwim.util.threads))
