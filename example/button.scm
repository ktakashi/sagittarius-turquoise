(import (clos user) (turquoise)
	(sagittarius object))

;; default size for all buttons
(define-method slot-unbound ((c <class>) (b <button>) (s (eqv? 'width))) 100)
(define-method slot-unbound ((c <class>) (b <button>) (s (eqv? 'height))) 27)
  
(let ((window (make <frame> :name "One Button Window"))
      (button (make <button> :name "Button")))
  (add! window button)
  (add! button (lambda (button event)
		 (print (~ event 'action))))
  (window))