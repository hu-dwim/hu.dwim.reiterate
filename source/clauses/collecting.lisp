;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def function expand/collector (name value-form kind &key (test #'eql) (at :end))
  (macrolet ((start/end-dispatch (start end)
               `(ecase at
                  ((:start :head) ,start)
                  ((:end :tail) ,end))))
    ;; TODO variable/last-cons is not always necessary (if there are only :at :start stuff)
    (bind (((variable/head variable/last-cons) (ensure-clause-data (list 'collecting name)
                                                 (list (register/variable (or name "COLLECTING/HEAD"))
                                                       (register/variable "COLLECTING/LAST-CONS")))))
      (register/result-form-candidate (list 'collecting name) variable/head)
      (with-unique-names (value)
        `(let ((,value ,value-form))
           ,(ecase kind
                   (collecting
                     (start/end-dispatch
                       `(progn
                          (setq ,variable/head (cons ,value ,variable/head))
                          (unless ,variable/last-cons
                            (setq ,variable/last-cons ,variable/head)))
                       `(setq ,variable/last-cons (if ,variable/head
                                                     (setf (cdr ,variable/last-cons) (cons ,value nil))
                                                     (setq ,variable/head (cons ,value nil))))))
                   (appending
                     (start/end-dispatch
                       (not-yet-implemented)
                       `(when ,value
                          (setq ,value (copy-list ,value))
                          (setq ,variable/last-cons (last (if ,variable/head
                                                              (setf (cdr ,variable/last-cons) ,value)
                                                              (setq ,variable/head ,value)))))))
                   (nconcing
                     (start/end-dispatch
                       (not-yet-implemented)
                       `(when ,value
                          (setq ,variable/last-cons (last (if ,variable/head
                                                              (setf (cdr ,variable/last-cons) ,value)
                                                              (setq ,variable/head ,value)))))))
                   (adjoining
                     (start/end-dispatch
                       (not-yet-implemented)
                       `(unless (member ,value ,variable/head :test ,test)
                          (setq ,variable/last-cons (if ,variable/head
                                                        (setf (cdr ,variable/last-cons) (cons ,value nil))
                                                        (setq ,variable/head (cons ,value nil)))))))
                   (unioning
                     (start/end-dispatch
                       (not-yet-implemented)
                       `(when ,value
                          (setq ,value (copy-list ,value))
                          (setq ,value (delete-if (LAMBDA (el)
                                                    (member el ,variable/head :test ,test))
                                                  ,value))
                          (setq ,variable/last-cons (last (if ,variable/head
                                                              (setf (cdr ,variable/last-cons) ,value)
                                                              (setq ,variable/head ,value)))))))))))))

(macrolet
    ((x (&rest entries)
       `(progn
          ,@(loop
              :for entry :in entries
              :collect (bind (((name &rest keyword-args) (ensure-list entry)))
                         `(def clause ,name
                            (clause-of-kind? ,name)
                            (bind (((value &key in into (at :end) ,@keyword-args) (rest -clause-))
                                   (value (-recurse- value)))
                              (with-possibly-different-iteration-context (in :clause -clause-)
                                (expand/collector into value ',name :at at
                                                  ,@(mappend (lambda (keyword-arg)
                                                               (list (intern (string (first keyword-arg))
                                                                             (load-time-value (find-package "KEYWORD")))
                                                                     (first keyword-arg)))
                                                             keyword-args))))))))))
  (x collecting
     appending
     nconcing
     (adjoining (test #'eql))
     (unioning (test #'eql))))
