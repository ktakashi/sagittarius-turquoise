;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; turquoise/extensions/split-panel.scm - Split panel components
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(define-library (turquoise extensions split-panel)
  ;; if native GUI support this then we don't have to export anything
  ;; but for now we only support Windows so it's always there
  (export <split-panel>)
  (import (rnrs)
	  (clos user)
	  (sagittarius)
	  (sagittarius object)
	  (sagittarius control)
	  (turquoise interface)
	  (turquoise components))
  (begin
    ;; split-panel can be implemented in Scheme level

    ;; split-panel have 2 empty panel
    ;; for virtical
    ;; +--------------+---------------+
    ;; |    panel1    |    panel2     |
    ;; +--------------+---------------+
    ;;
    ;; for horizontal
    ;; +--------------+
    ;; |    panel1    |
    ;; +--------------+
    ;; |    panel2    |
    ;; +--------------+
    (define-class <split-panel> (<panel>)
      ((panel1 :init-keyword :panel1 :init-form (make <panel>))
       (panel2 :init-keyword :panel2 :init-form (make <panel>))
       (virtical :init-keyword :virtical :init-value #f)
       ;; separator position
       (position :init-keyword :position :init-value 0)
       (moving :init-value #f)
       (cursor)))

    (define-method initialize ((p <split-panel>) initargs)
      (define (get-offsets event virtical?)
	(let* ((data (~ event 'data))
	       (w (assq 'width data))
	       (h (assq 'height data)))
	  (if virtical?
	      (values (cdr h) (cdr w))
	      (values (cdr w) (cdr h)))))
      (define (set-and-move comp x y w h)
	(safe-position-set! comp x y w h)
	(move-component comp x y w h))

      (call-next-method)
      ;; the background color will be border's color
      (set! (~ p 'background) (get-keyword :background initargs 'light-gray))
      (set! (~ p 'cursor) 
	    (create-cursor (if (~ p 'virtical) 'size-we 'size-ns)))
      (add! p 'resize 
	    (lambda (sp a)
	      (let* ((virtical? (~ sp 'virtical))
		     (position (~ sp 'position))
		     (p1 (~ sp 'panel1))
		     (p2 (~ sp 'panel2)))
		(let-values (((width height) (get-offsets a virtical?)))
		  ;; position won't be zero after it's set
		  (when (zero? position)
		    ;; compute it now
		    (set! position (div height 2))
		    (set!(~ sp 'position) position))
		  (when (and (not (zero? height))
			     (< height position)
			     (> height (* 10 2)))
		    (set! position (- height 10))
		    (set!(~ sp 'position) position))
		  (if virtical?
		      (begin
			;; confusing but width and height is
			;; other way around in virtical mode
			(set-and-move p1 0 0 (- position 1) width)
			(set-and-move p2 (+ position 2) 0
				      (- height position 2) width))
		      (begin
			(set-and-move p1 0 0 width (- position 1))
			(set-and-move p2 0 (+ position 2) width 
				      (- height position 2))))))))
      (add! p 'mouse-move 
	    (lambda (sp a)
	      (define (left-button? a)
		(cond ((assq 'button (~ a 'data))
		       => (lambda (s) (assq 'left (cdr s))))
		      (else #f)))
	      (let1 virtical? (~ sp 'virtical)
		(let-values (((width height) (get-offsets a virtical?)))
		  (when (> height 10)
		    (set-cursor! sp (~ sp 'cursor))
		    (when (and (left-button? a) (~ sp 'moving))
		      (unless (> height 
				 (if virtical? (~ sp 'width) (~ sp 'height)))
			(set! (~ sp 'position) height)
			(resize-component sp (~ sp 'width) (~ sp 'height)))))))))
      (add! p 'mouse-down (lambda (sp a)
			    (set-cursor! sp (~ sp 'cursor))
			    (set! (~ sp 'moving) #t)
			    (capture-component sp)))
      (add! p 'mouse-up (lambda (sp a)
			  (set! (~ sp 'moving) #f)
			  (uncapture-component sp))))

    (define-method add! ((p <split-panel>) o)
      (error 'split-panel
	     "attempt to add component to split panel itself." o))

    (define-method add! ((p <split-panel>) (n (eqv? 1)) (c <component>))
      (add! (~ p 'panel1) c))
    (define-method add! ((p <split-panel>) (n (eqv? 2)) (c <component>))
      (add! (~ p 'panel2) c))

    (define-method show ((sp <split-panel>))
      (call-next-method)
      (let* ((panel (~ sp 'panel1))
	     (context (~ panel 'context)))
	(set! (~ panel 'owner) sp))
      (let* ((panel (~ sp 'panel2))
	     (context (~ panel 'context)))
	(set! (~ panel 'owner) sp))
      (show (~ sp 'panel1))
      (show (~ sp 'panel2))
      (resize-component sp (~ sp 'width) (~ sp 'height)))

    (define-method update-component ((sp <split-panel>))
      (update-component (~ sp 'panel1))
      (update-component (~ sp 'panel2))
      (resize-component sp (~ sp 'width) (~ sp 'height)))

    ))