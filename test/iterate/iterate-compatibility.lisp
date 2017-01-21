;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/iterate/test)

(def suite* (test/iterate-compat :in test))

(bind ((test-file (asdf:system-relative-pathname :iterate "iterate-test.lisp")))
  (assert (uiop:file-exists-p test-file))
  (load test-file))

(def test test/iterate-compat/iterate-self-tests ()
  (bind ((rt-package (find-package #+sbcl :sb-rt
                                   #-sbcl :regression-test)))
    (funcall (find-symbol (symbol-name '#:do-tests) rt-package))))
