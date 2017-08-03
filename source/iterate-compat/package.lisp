;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.reiterate/iterate-compat
  (:documantation "Compatibility layer meant to be a drop-in replacement for Iterate.")
  (:use :alexandria
        :anaphora
        :contextl
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.reiterate
        :hu.dwim.syntax-sugar
        :hu.dwim.walker
        :hu.dwim.util
        :metabang-bind)
  (:shadow #:repeat
           #:dsetq
           #:for
           #:for/from)
  (:export #:iterate
           #:iter
           #:initially #:after-each #:finally #:finally-protected
           #:else #:if-first-time #:first-iteration-p #:first-time-p
           #:finish #:leave #:next-iteration #:next #:terminate
           #:repeat #:for #:as #:generate #:generating #:in
           #:sum #:summing #:multiply #:multiplying
           #:maximize #:minimize #:maximizing #:minimizing #:counting
           #:always #:never #:thereis #:finding #:collect #:collecting
           #:with #:while #:until #:adjoining #:nconcing #:appending
           #:nunioning #:unioning #:reducing #:accumulate #:accumulating

           #:defmacro-clause
           #:defmacro-driver
           #:defclause-sequence
           #:declare-variables
           #:defsynonym
           #:display-iterate-clauses
           #:dsetq)
  (:shadowing-import-from :hu.dwim.common
                          #:in-package)
  (:readtable-setup (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.reiterate)))

(hu.dwim.common:import-all-owned-symbols :hu.dwim.reiterate :hu.dwim.reiterate/iterate-compat)
