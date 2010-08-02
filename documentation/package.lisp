;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.reiterate.documentation
  (:use :alexandria
        :anaphora
        :hu.dwim.asdf
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.reiterate
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :metabang-bind
        :hu.dwim.wui)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.wui)))
