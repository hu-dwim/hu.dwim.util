;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.temporary-files
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description ""
  :depends-on (:hu.dwim.util+iolib
               :iolib.pathnames
               :iolib.os
               :iolib.syscalls)
  :components ((:module "source"
                :components ((:file "temporary-files")))))
