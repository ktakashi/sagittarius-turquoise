(import (clos user) (turquoise)
	(sagittarius object))

(let ((window (make <frame> :name "One Button Window"))
      (button (make <button> :name "Button")))
  (add! window button)
  (add! button (lambda (button action)
		 (print (~ action 'operation))))
  (window))