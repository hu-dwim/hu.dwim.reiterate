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
  :depends-on (:hu.dwim.common
               :hu.dwim.def
               :hu.dwim.defclass-star
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "conditions" :depends-on ("package" "variables"))
                             (:file "clause-handling" :depends-on ("conditions" "form-utils" "package" "variables" "walker"))
                             (:file "expansion" :depends-on ("clause-handling" "conditions" "walker" "variables"))
                             (:file "form-utils" :depends-on ("variables"))
                             (:file "package")
                             (:file "standard-clauses" :depends-on ("clause-handling" "conditions" "package" "variables"))
                             (:file "variables" :depends-on ("package"))
                             (:file "walker" :depends-on ("conditions" "package" "variables"))))))
