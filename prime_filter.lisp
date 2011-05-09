;;;
;;; This file is a part of Hanoi Coding Dojo Kata session, released under
;;; MIT license.
;;;
;;; See the COPYING file for more details.
;;;
;;; Copyright (c) 2011 by Hanoi Coding Dojo
;;;


(require :lisp-unit)
(require :iterate)

(defpackage #:prime-filter
  (:use :common-lisp :lisp-unit :iterate))

(in-package :prime-filter)

(defun prime-filter (a-list)
  (let* ((result '()))
    (loop for number in a-list
       do (if (and (is-prime? number)
                   (not (find number result)))
              (push number result)))
    (sort result #'<)))

(defun is-prime? (number)
  (let* ((number-divisors 0))
    (iter (for i from 1 to number)
          (if (= 0 (mod number i))
              (incf number-divisors)))
    (if (= 2 number-divisors)
        t
        nil)))

(define-test test-with-number-2
  (assert-equalp '(2) (prime-filter '(2))))

(define-test test-with-number-less-than-10
  (assert-equalp '(2 3 5 7) (prime-filter '(0 1 2 3 4 5 6 7 8 9))))

(define-test test-with-number-greater-than-10
  (assert-equalp '(11 23) (prime-filter '(11 12 14 15 23 55))))

(define-test test-with-duplications
  (assert-equalp '(2) (prime-filter '(2 2)))
  (assert-equalp '(2 3) (prime-filter '(2 2 3 3 3))))

(define-test test-with-duplications-and-mixed-order
  (assert-equalp '(2 3) (prime-filter '(2 3 2 3)))
  (assert-equalp '(2 3) (prime-filter '(3 2 3 2)))
  (assert-equalp '(2 11) (prime-filter '(11 12 12 4 4 1 2 11))))

(defun test-prime-filter ()
  (run-all-tests :prime-filter))
