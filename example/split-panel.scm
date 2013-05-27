(import (clos user) (turquoise)
	(sagittarius object))

(let* ((window (make <frame> :name "Panel example"))
       (panel1 (make <split-panel> :name "Panel1"
		     :panel1 (make <panel> :background 'black)
		     :panel1 (make <panel> :background (make <rgb> :g 255))))
       (panel2 (make <split-panel> :name "Panel1"
		     :virtical #t
		     ;; top panel must have size
		     :adjust-size #t
		     :panel1 panel1
		     :panel2 (make <panel> :background (make <rgb> :b 255)))))
  (add! window panel2)
  (window))