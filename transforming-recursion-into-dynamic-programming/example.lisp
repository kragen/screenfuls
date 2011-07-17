;;; These two examples are not from the same version of the code.
;;; I have not tested them.

(defun-recurrence M (n k) (1 1)
  (cond ((= n 1) (A 0))
        ((= k 1) (loop for i from 0 below n 
                    sum (A i)))
        (t (loop for i from 1 to n
              minimize (max (M i (- k 1))
                            (loop for j from i below n
                               sum (A j)))))))

(defun-recurrence fib (n) (0)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
