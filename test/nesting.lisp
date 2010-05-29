;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/nesting :in test))

(def test test/nesting/1 ()
  #+nil ;; TODO expands into infinite loop
  (with-expected-failures
    (is (equal '(a b c)
               (eval '(iter named outer
                       (for i :in-list '(a b c))
                       (macrolet ((wrapper (&body body)
                                    ;; FIXME unfortunately we can't detect this ITER due to ` hiding it from us
                                    `(iter named inner
                                           (repeat 2)
                                           ,@body)))
                         (wrapper
                          ;; this collect should collect into OUTER
                          (collect i))))))))
  (is (equal '(a a b b c c)
             (eval '(macrolet ((wrapper (&body body)
                                `(iter named inner
                                       (repeat 2)
                                       ,@body)))
                     (iter named outer
                           (for i :in-list '(a b c))
                           (wrapper
                            ;; this collect should collect into OUTER
                            (collect i))))))))
