;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.reiterate
  :class hu.dwim.system
  :description "Iterator macro inspired by iterate."
  :depends-on (:alexandria
               :anaphora
               :hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.defclass-star
               :hu.dwim.syntax-sugar+hu.dwim.walker
               ;; TODO :hu.dwim.util brings in many dependencies through :hu.dwim.common (including nothing less than iterate itself! :)
               :hu.dwim.util
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
