(import (clos user) (turquoise)
	(sagittarius object))

;; default size for all panels
(define-method slot-unbound ((c <class>) (p <panel>) (s (eqv? 'width)))  200)
(define-method slot-unbound ((c <class>) (p <panel>) (s (eqv? 'height))) 200)

(let ((window (make <frame> :name "Panel example"))
      (panel1 (make <panel> :name "Panel1" :background 'gray))
      (panel2 (make <panel> :name "Panel1" :background 'black
		    :x-point 200))
      (label (make <label> :text "Label1" :width 100 :height 27)))
  (add! window panel1)
  (add! window panel2)
  (add! panel2 label)
  (window))