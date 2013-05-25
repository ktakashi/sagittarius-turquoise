(import (clos user) (turquoise)
	(sagittarius object))

;; default size for all buttons
(define-method slot-unbound ((c <class>) (b <button>) (s (eqv? 'width))) 100)
(define-method slot-unbound ((c <class>) (b <button>) (s (eqv? 'height))) 27)

;; default size for all text
(define-method slot-unbound ((c <class>) (b <text>) (s (eqv? 'height))) 27)


(let ((window (make <frame>  :name "One Button Window"))
      (text   (make <text>   :value "Initial text" :width 100))
      (button (make <button> :name "Get value" :x-point 100)))
  (add! window button)
  (add! window text)
  (add! button (lambda (button action) 
		 (set! (~ text 'color) (make <rgb> :b 255))
		 (print (~ text 'value))))
  (window))