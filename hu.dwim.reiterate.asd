;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.reiterate
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Iterator macro inspired by iterate."
  :depends-on (:alexandria
               :anaphora
               :hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.defclass-star
               :hu.dwim.syntax-sugar
               :hu.dwim.syntax-sugar/lambda-with-bang-args
               ;; TODO :hu.dwim.util brings in many dependencies through :hu.dwim.common (including nothing less than iterate itself! :)
               :hu.dwim.util
               :hu.dwim.walker
               :metabang-bind)
  :components ((:module "source"
                :components ((:module "clauses"
                              :components ((:file "numeric")
                                           (:file "sequence")
                                           (:file "simple")
                                           (:file "collecting"))
                              :depends-on ("clause-handling" "conditions" "package" "variables" "walker"))
                             (:file "conditions" :depends-on ("package" "variables"))
                             (:file "clause-handling" :depends-on ("conditions" "form-utils" "package" "utils" "variables" "walker"))
                             (:file "expansion" :depends-on ("clause-handling" "conditions" "variables" "walker"))
                             (:file "form-utils" :depends-on ("variables"))
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:file "utils" :depends-on ("variables"))
                             (:file "variables" :depends-on ("package" "logger"))
                             (:file "walker" :depends-on ("conditions" "package" "variables"))))))

(defsystem :hu.dwim.reiterate/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.debug
               :hu.dwim.reiterate+hu.dwim.logger
               :hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.walker/test)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic" :depends-on ("suite"))
                             (:file "lexenv" :depends-on ("suite"))
                             (:file "nesting" :depends-on ("suite"))
                             (:file "numeric" :depends-on ("suite"))
                             (:file "source-code-identity" :depends-on ("package"))
                             (:file "suite" :depends-on ("package"))
                             (:file "types" :depends-on ("suite"))))))

(defsystem :hu.dwim.reiterate/iterate-compat
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "A drop in compatibility layer of hu.dwim.reiterate to replace Iterate."
  :depends-on (:hu.dwim.reiterate)
  :components ((:module "source"
                :components ((:module "iterate-compat"
                              :components ((:file "package")
                                           (:file "clause-handling" :depends-on ("package"))
                                           (:file "clauses" :depends-on ("package" "clause-handling"))))))))

(defsystem :hu.dwim.reiterate/iterate-compat/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.reiterate/iterate-compat
               :hu.dwim.reiterate/test
               :iterate
               (:feature :sbcl (:require :sb-rt))
               (:feature (:not :sbcl) :regression-test))
  :components ((:module "test"
                :components ((:module "iterate-compat"
                              :components ((:file "package")
                                           (:file "iterate-compat" :depends-on ("package"))))))))
