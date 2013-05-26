(import (rnrs) (clos user) (turquoise) (sagittarius object)
	(util file))

(define (file-name path)
  (let-values (((dir base ext) (decompose-path path)))
    (string-append base "." ext)))

(let ((frame (make <frame> :name "Simple Edit"))
      ;; TODO resize automatically
      (text  (make <text-area> :width 500 :height 500))
      (menu  (make <menu>))
      (file  (make <menu> :name "File"))
      (open  (make <menu-item> :name "&Open"))
      (save  (make <menu-item> :name "&Save"))
      (help  (make <menu> :name "Help"))
      (about (make <menu-item> :name "&About"))
      (open-select (make <file-select> :title "Open File" :type 'open))
      (save-select (make <file-select> :title "Save File" :type 'save)))
  (add! frame text)
  (add! frame menu)
  (add! menu file)
  (add! menu help)
  (add! file open)
  (add! file save)
  (add! help about)
  (add! open (lambda (o a)
	       (let ((path (open-select)))
		 (when path
		   (set! (~ frame 'name) (file-name path))
		   (let ((contents (call-with-input-file path get-string-all)))
		     (unless (eof-object? contents)
		       (set! (~ text 'value) contents)))))))
  (add! save (lambda (o a)
	       (let ((path (save-select)))
		 (when path
		   (when (file-exists? path) (delete-file path))
		   (set! (~ frame 'name) (file-name path))
		   (call-with-output-file path
		     (lambda (p)
		       (put-string p (~ text 'value))))))))
  (frame))
		   
  
  