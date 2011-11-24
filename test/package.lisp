;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.reiterate.test
  (:use :alexandria
        :anaphora
        :contextl
        :hu.dwim.common-lisp
        :hu.dwim.debug
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.reiterate
        :hu.dwim.logger
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.walker
        :hu.dwim.util
        :metabang-bind)
  (:shadow #:test
           #:eval
           #:macroexpand)
  (:shadowing-import-from :hu.dwim.reiterate
                          #:log)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.reiterate)))

(hu.dwim.common:import-all-owned-symbols :hu.dwim.reiterate :hu.dwim.reiterate.test)

(use-package :hu.dwim.debug :hu.dwim.reiterate)
