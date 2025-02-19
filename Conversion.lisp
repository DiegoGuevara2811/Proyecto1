(defun fahrenheit-a-celsius (fahrenheit)
  (/ (* (- fahrenheit 32) 5) 9))

(defun convertir-temperatura ()
  (format t "Introduce la temperatura en Fahrenheit: ")
  (let ((fahrenheit (read)))
    (let ((celsius (fahrenheit-a-celsius fahrenheit)))
      (format t "La temperatura en Celsius es: ~2f~%" celsius))))

(convertir-temperatura)
