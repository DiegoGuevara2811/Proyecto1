(defun factorial (n)
  (cond
    ((= n 0) 1)
    (t (setq resultado (* n (factorial (- n 1))))
       resultado)))
 (factorial 5)