;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.authorization
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description "Authorization for lisp forms."
  :depends-on (:hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.partial-eval
               :hu.dwim.util
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "authorization")))))
