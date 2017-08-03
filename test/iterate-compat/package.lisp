;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/test)

(def package :hu.dwim.reiterate/iterate-compat/test
  (:use :alexandria
        :anaphora
        :contextl
        :hu.dwim.common-lisp
        :hu.dwim.debug
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.reiterate/iterate-compat
        :hu.dwim.logger
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.walker
        :hu.dwim.util
        :metabang-bind)
  (:shadowing-import-from :hu.dwim.reiterate/test
                          #:test
                          #:eval
                          #:macroexpand)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.reiterate)))

(hu.dwim.common:import-all-owned-symbols :hu.dwim.reiterate/iterate-compat :hu.dwim.reiterate/iterate-compat/test)
(hu.dwim.common:import-all-owned-symbols :hu.dwim.reiterate :hu.dwim.reiterate/iterate-compat/test)

(use-package :hu.dwim.debug :hu.dwim.reiterate/iterate-compat)

;; set up ITERATE as a nickname for HU.DWIM.REITERATE/ITERATE-COMPAT,
;; and shadowing-import some stuff that the iterate-test is excercising, but we don't want to implement ourselves, like DSETQ.
;; TODO decide... looks like this hack will not be needed after all...
#+nil
(bind ((iter-package (find-package :iterate))
       (iter-compat-package (find-package :hu.dwim.reiterate/iterate-compat)))
  (when (and iter-package
             (not (eq iter-package iter-compat-package)))
    (warn "Renaming the ITERATE package to be able to install a nickname for the HU.DWIM.REITERATE/ITERATE-COMPAT compatibility package, so that we can load and run the original ITERATE tests on it.")
    (rename-package iter-package :iterate.original))
  (unless (member (symbol-name :iterate) (package-nicknames iter-compat-package) :test 'equal)
    (rename-package iter-compat-package (package-name iter-compat-package) '(:iterate)))
  #+(or) ;; this would be too much headache...
  (use-package #+sbcl :sb-rt
               #-sbcl :regression-test
               :hu.dwim.reiterate/test)
  (bind ((symbols (read-from-string "(iterate.original:dsetq)")))
    (shadowing-import symbols iter-compat-package)
    (export symbols iter-compat-package)))

(bind ((iter-compat-package (find-package :hu.dwim.reiterate/iterate-compat))
       (symbols (read-from-string "(iterate:dsetq)")))
  (shadowing-import symbols iter-compat-package)
  (export symbols iter-compat-package))
