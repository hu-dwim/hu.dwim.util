;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2011 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.linear-mapping
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description ""
  :depends-on (:bordeaux-threads
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "linear-mapping")))))
