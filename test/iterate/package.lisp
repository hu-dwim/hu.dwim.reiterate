;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/test)

;; set up ITERATE as a nickname for HU.DWIM.REITERATE/ITERATE,
;; and shadowing-import some stuff that we don't want to implement ourselves, like DSETQ
(bind ((iter-package (find-package :iterate))
       (iter-compat-package (find-package :hu.dwim.reiterate/iterate)))
  (when (and iter-package
             (not (eq iter-package iter-compat-package)))
    (warn "Renaming the ITERATE package to be able to install a nickname for the HU.DWIM.REITERATE/ITERATE compatibility package, so that we can load and run the original ITERATE tests on it.")
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
