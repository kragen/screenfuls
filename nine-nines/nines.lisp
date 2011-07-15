;;; nines.lisp --- Program to solve the "Nine Nines" problem.
;;; Spec at http://www.itasoftware.com/careers/programmers.php
;;; Written by Luke Gorrie <luke@bluetail.com> on April 26, 2002.
;;; Hacked by Darius Bacon <darius@wry.me> to fit on one screen.
;;; No copyright license information.

;; The possible results of an expression containing one nine are just
;; 9 and -9. Any expression with N (> 1) nines can be seen as a
;; combination of two smaller subexpressions by some operator,
;; resulting in any combination of the subexpressions' possible
;; results.
;;
;; Note: Because unary minus can be applied at any time (including the
;; very end), the internal representation only explicitly deals with
;; absolute values, which are understood to be +/-.

(defvar *solutions* nil
  "Array of sets of possible absolute values for <index>-nine expressions.")

(defun run (n)
  "Return the solution to the N Nines problem."
  (let ((*solutions* (make-array (1+ n) :initial-element nil)))
    (setf (aref *solutions* 1) '(9))
    (loop for k from 2 to n
          do (setf (aref *solutions* k) (solve k)))
    (find-missing (sort (aref *solutions* n) #'<))))

(defun solve (n)
  "Calculate the possible values for an expression of N nines."
  (let ((table (make-hash-table :test 'eql)))
    (do ((x (1- n) (1- x))
         (y 1      (1+ y)))
        ((> y x) (hash-table-keys table))
      (add-combinations table x y))))

(defun add-combinations (table i j)
  "Add to TABLE the absolute values that can result from combining expressions
  of I nines and J nines using any operator, with either on the left hand side."
  (labels ((add-answer (answer) (setf (gethash answer table) t)))
    (dolist (x (aref *solutions* i))
      (dolist (y (aref *solutions* j))
        (add-answer (+ x y))
        (add-answer (abs (- x y)))
        (add-answer (* x y))
        (unless (zerop y) (add-answer (/ x y)))
        (unless (zerop x) (add-answer (/ y x)))))))

(defun find-missing (list)
  "Find the first 'missing' integer in the (sorted) LIST, starting from 0."
  (loop for x in (remove-if-not #'integerp list)
        for n from 0
        when (/= x n) return n))

(defun hash-table-keys (hash-table)
  "Return a list of the keys of HASH-TABLE."
  (let ((keys '()))
    (maphash (lambda (key value) (push key keys))
             hash-table)
    keys))
