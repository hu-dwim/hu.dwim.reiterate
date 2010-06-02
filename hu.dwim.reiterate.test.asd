;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.reiterate.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.reiterate
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic" :depends-on ("suite"))
                             (:file "lexenv" :depends-on ("suite"))
                             (:file "nesting" :depends-on ("suite"))
                             (:file "suite" :depends-on ("package"))))))
