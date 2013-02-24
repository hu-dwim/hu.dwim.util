;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2010 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.flexml
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description "A CXML document model that can parse into CLOS nodes"
  :depends-on (:cl-ppcre
               :cxml
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.def.namespace
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "flexml" :depends-on ("flexml-package"))
                             (:file "flexml-package")))))
