(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun calcular-factorial ()
  (format t "Introduce un número para calcular su factorial: ")
  (let ((n (read)))
    (if (and (integerp n) (>= n 0))
        (format t "El factorial de ~d es: ~d~%" n (factorial n))
        (format t "Por favor, introduce un número entero no negativo.~%"))))

(calcular-factorial)
