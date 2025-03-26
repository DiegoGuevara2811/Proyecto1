(defun mcd (a b)
  (cond
    ((= b 0) a)
    (t (mcd b (mod a b)))))
(print (mcd 56 98))
(atom 3)