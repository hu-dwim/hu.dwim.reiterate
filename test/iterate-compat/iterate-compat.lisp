;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/iterate-compat/test)

(def suite* (test/iterate-compat :in test))

(def function read-iterate-tests-with-reitarate ()
  (bind ((test-file (asdf:system-relative-pathname :iterate "iterate-test.lisp")))
    (assert (uiop:file-exists-p test-file))
    (with-standard-io-syntax
      (with-input-from-file (input test-file)
        (bind ((*package* (find-package :hu.dwim.reiterate/iterate-compat/iterate-tests))
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

(def fixture read-itearate-tests ()
  (rem-all-tests)
  (read-iterate-tests-with-reitarate)
  (-body-))

(def test test/iterate-compat/iterate-self-tests ()
  (with-fixture read-itearate-tests
    (bind ((all-tests (pending-tests))
           (expected-to-fail (symbol-value 'hu.dwim.reiterate/iterate-compat/iterate-tests::*tests-expected-to-fail*))
           ((:values _ _ failed-tests)
            (funcall (find-symbol (symbol-name '#:do-tests) :hu.dwim.reiterate/iterate-compat/test)))
           (unexpected-failures (set-difference failed-tests expected-to-fail)))
      (format *debug-io* "~&~%~%Unexpectedly failed tests:~%~A~%Test count: ~A, failed: ~A, expected failures: ~A, unexpected failures: ~A~%~%"
              (format nil "~{~A~^~%~}" (sort (mapcar 'string-downcase unexpected-failures)
                                             'string<))
              (length all-tests)
              (length failed-tests)
              (length expected-to-fail)
              (length unexpected-failures))
      (values))))
