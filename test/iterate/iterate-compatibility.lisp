;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/iterate/test)

(def suite* (test/iterate-compat :in test))

;; this is the package into which the interate unit tests will be read
(def package :hu.dwim.reiterate/iterate/iterate-tests
  (:use :common-lisp
        :hu.dwim.reiterate/iterate ; use the reiterate/iterate compatibility layer
        #+sbcl #:sb-rt
        #-sbcl #:regression-test))

(def function read-iterate-tests-with-reitarate ()
  (bind ((test-file (asdf:system-relative-pathname :iterate "iterate-test.lisp")))
    (assert (uiop:file-exists-p test-file))
    (with-standard-io-syntax
      (with-input-from-file (input test-file)
        (bind ((*package* (find-package :hu.dwim.reiterate/iterate/iterate-tests))
               (*print-readably* nil)
               (*print-circle* #t)
               (defpackage-form (read input))
               (in-package-form (read input)))
          (assert (eq 'cl:defpackage (first defpackage-form)))
          (assert (string= '#:iterate.test (second defpackage-form)))
          (assert (eq 'cl:in-package (first in-package-form)))
          (assert (string= '#:iterate.test (second in-package-form)))
          (loop
            :for form = (read input nil 'eof)
            :until (eq form 'eof)
            :do (print form)
            :do (eval form)))))))

(def test test/iterate-compat/iterate-self-tests ()
  (bind ((rt-package (find-package #+sbcl :sb-rt
                                   #-sbcl :regression-test)))
    (funcall (find-symbol (symbol-name '#:do-tests) rt-package))))
