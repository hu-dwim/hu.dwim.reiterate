;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.reiterate
  (:use :alexandria
        :anaphora
        :contextl
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.syntax-sugar
        :hu.dwim.walker
        :hu.dwim.util
        :metabang-bind)
  (:export #:for
           #:repeat
           #:collect
           #:initially
           #:finally
           #:in-list)
  (:shadowing-import-from :closer-mop
                          #:defgeneric
                          #:defmethod
                          #:ensure-generic-function
                          #:find-method
                          #:remove-method
                          #:standard-class
                          #:standard-method
                          #:standard-generic-function)
  (:shadowing-import-from :hu.dwim.common
                          #:in-package)
  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))
