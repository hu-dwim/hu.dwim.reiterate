;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.reiterate.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.debug
               :hu.dwim.reiterate+hu.dwim.logger
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
