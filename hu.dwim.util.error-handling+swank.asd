;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.error-handling+swank
  :defsystem-depends-on (:hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :depends-on (:hu.dwim.util.error-handling
               :swank)
  :components ((:module "integration"
                :components ((:file "error-handling+swank")))))
