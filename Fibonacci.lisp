(defun fibonacci (n)
  (let ((start 0)
        (end 1))
    (loop for i from 0 below n do
      (print start)
      (psetq start end
             end (+ start end)))))

(fibonacci 10)
