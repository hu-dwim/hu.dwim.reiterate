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
                          (collect i)))))))))

(def test test/nesting/2 ()
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

(def test test/nesting/3 ()
  (is (equal '(a a a b b b c c c)
             (eval '(iter outer
                          (for i :in-list '(a b c))
                          (progn
                            (iter inner
                                  (repeat 3)
                                  (collect i :in outer))))))))

(def test test/nesting/4 ()
  (is (equal '(A A 1 2 3 A 1 2 3 B B 1 2 3 B 1 2 3 C C 1 2 3 C 1 2 3)
             (eval '(iter level1
                          (for i :in-list '(a b c))
                          (progn
                            (collect i)
                            (iter level2
                                  (repeat 2)
                                  (collect i :in level1)
                                  (iter level3
                                        (for i :in-list '(1 2 3))
                                        (collect i :in level1)))))))))
