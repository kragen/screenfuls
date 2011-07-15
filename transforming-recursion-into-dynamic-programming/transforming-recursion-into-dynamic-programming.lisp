;;; Transforming recursion into dynamic programming.
;;; © 1994-2003 by Darius Bacon.  No license provided.

(defmacro defun-recurrence (name params inits &body body)
  "Define a recursive function that's evaluated by dynamic programming,
   bottom-up from the INITS values to the PARAMS values."
  (let ((tabulate-name (concat-symbol "TABULATE-" name))
        (table `(make-array (list ,@(mapcar (lambda (p) `(+ ,p 1))
                                            params))))
        (ranges (mapcar (lambda (p init) `(for ,p from ,init to ,p))
                        params
                        inits)))
    `(progn
       (defun ,tabulate-name ,params
         (tabulate ,name ,table ,ranges ,@body))
       (defun ,name ,params
         (aref (,tabulate-name ,@params) ,@params)))))

(defmacro tabulate (name table-exp ranges &body body)
  "Evaluate a recursive function by dynamic programming, returning
   the memo-table."
  (let ((table (gensym))
        (vars (mapcar #'second ranges)))
    `(let ((,table ,table-exp))
       ,(nest-loops ranges
                    `(setf (aref ,table ,@vars)
                           (flet ((,name ,vars (aref ,table ,@vars)))
                             ,@body)))
       ,table)))

(defun nest-loops (ranges body-exp)
  "Build a nested LOOP form."
  (if (null ranges)
      body-exp
      `(loop ,@(car ranges)
             do ,(nest-loops (cdr ranges) body-exp))))

(defun concat-symbol (&rest parts)
  "Concatenate symbols or strings to form an interned symbol."
  (intern (format nil "~{~a~}" parts)))
