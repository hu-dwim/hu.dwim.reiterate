;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.reiterate.test
  :class hu.dwim.test-system
  :defsystem-depends-on (:hu.dwim.asdf
                         :hu.dwim.logger)
  :depends-on (:hu.dwim.debug
               :hu.dwim.logger
               :hu.dwim.reiterate
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic" :depends-on ("suite"))
                             (:file "lexenv" :depends-on ("suite"))
                             (:file "nesting" :depends-on ("suite"))
                             (:file "numeric" :depends-on ("suite"))
                             (:file "source-code-identity" :depends-on ("package"))
                             (:file "suite" :depends-on ("package"))
                             (:file "types" :depends-on ("suite"))))))
