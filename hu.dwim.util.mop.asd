;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.mop
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :depends-on (:hu.dwim.util
               :closer-mop)
  :components ((:module "source"
                :components (#+sbcl(:file "compact-class" :depends-on ("mop"))
                             (:file "mop")))))
