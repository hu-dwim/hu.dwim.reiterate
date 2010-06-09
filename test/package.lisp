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
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.reiterate
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.walker
        :hu.dwim.util
        :metabang-bind)
  (:shadow #:test)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.reiterate)))

(in-package :hu.dwim.reiterate.test)

(bind ((mohter-package (find-package :hu.dwim.reiterate))
       (test-package (find-package :hu.dwim.reiterate.test)))
  (do-symbols (symbol mohter-package)
    (when (and (eq (symbol-package symbol) mohter-package)
               (not (find-symbol (symbol-name symbol) test-package)))
      (import symbol))))
