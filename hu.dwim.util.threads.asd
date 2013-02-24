;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.threads
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description "Various utilities used by the dwim.hu team. Threading related utilities for a bit more dependency."
  :depends-on (:bordeaux-threads
               :hu.dwim.def.namespace
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "threads")))))
