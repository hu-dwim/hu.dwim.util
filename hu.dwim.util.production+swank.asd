;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.production+swank
  :defsystem-depends-on (:hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :depends-on (:hu.dwim.util.production
               :swank)
  :components ((:module "integration"
                :components ((:file "production+swank")))))
