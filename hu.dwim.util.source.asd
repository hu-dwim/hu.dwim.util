;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.source
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Infrastructure to provide the lisp source forms for named global definitions. SBCL only at this point."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :swank)
  :components ((:module "source"
                :components ((:file "source")))))
