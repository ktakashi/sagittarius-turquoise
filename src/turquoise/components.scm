;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; turquoise/components - GUI components
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

(library (turquoise components)
    (export
     ;; abstract classes
     <component> <container>
     ;; widgets (non-container)
     <button> <radio>
     <check-box> <tri-state-check-box>
     <combo-box> <text>
     <list-box> <scroll> <label> <text-area>
     ;; container widgets
     <window> <frame> <dialog>
     ;; mixins
     <menu-bar-container> <content-panel-container> <performable>
     ;; action
     <action>
     ;; misc
     <rgb> <rgb-color>
     ;; component synchroniser
     update-component
     )
    (import (rnrs) 
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius mop validator))

  ;; TODO what should component have?
  (define-class <component> () 
    ((context)
     ;; owner component, #f means root component
     (owner   :init-keyword :owner :init-value #f)
     ;; position and style
     (x-point       :init-keyword :x-point)
     (y-point       :init-keyword :y-point)
     (width         :init-keyword :width)
     (height        :init-keyword :height)
     (style         :init-keyword :style   :init-value '())
     (visible       :init-keyword :visible :init-value #t)
     (background    :init-keyword :background :init-value 'white)
     ;; if this component is busy or not (doing something)
     (busy    :init-value #f)))

  (define-generic update-component)
  ;; default do nothing
  (define-method update-component (c) #t)
  (define (component-observer component value)
    ;; component has value already
    (update-component component))

  (define-class <container> (<component>)
    ;; container can contain component
    ((components :init-keyword :components :init-value '())))

  ;; what should this class have?
  (define-class <window> (<container>) ())

  ;; mixins
  (define-class <content-panel-container> ()
    ((root-panel :init-keyword :root-panel)))

  (define-class <menu-bar-container> ()
    ((menu-bar :init-keyword :menu-bar)))

  (define-class <frame> 
    (<window> <content-panel-container> <menu-bar-container>)
    ())

  (define-class <dialog> (<window> <content-panel-container>)
    ())

  ;; performable have actions (procedure list)
  (define-class <performable> ()
    ((actions :init-value '())))

  (define-class <button> (<component> <performable>) ())
  (define-class <radio> (<button>) ())
  (define-class <check-box> (<button> <validator-mixin>) 
    ((checked :init-keyword :checked :init-value #f
	      :observer component-observer)))
  (define-class <tri-state-check-box> (<check-box>) ())
  (define-method initialize ((tc <tri-state-check-box>) initargs)
    (call-next-method)
    (set! (~ tc 'checked) (get-keyword :checked initargs '())))

  (define-class <combo-box> (<component> <performable>) ())
  (define-class <list-box>  (<component> <performable>) ())
  (define-class <scroll>    (<component>) ())

  (define-class <rgb> ()
    ;; TODO should we add validator to check range?
    ((r :init-keyword :r :init-value 0)
     (g :init-keyword :g :init-value 0)
     (b :init-keyword :b :init-value 0)))

  ;; this class is mixin
  (define-class <rgb-color> (<validator-mixin>)
    ((color :init-keyword :color :init-form (make <rgb>)
	    :validator (lambda (o v) 
			 (unless (is-a? v <rgb>)
			   (assertion-violation 'label-color
						"rgb object required"))
			 v)
	    ;; derived class must be a component
	    :observer component-observer)))

  (define-class <text>      (<component> <performable> <rgb-color>)
    ((value :init-keyword :value :init-value "" :observer component-observer)))
  (define-class <text-area> (<text>) ())

  (define-class <label>     (<component> <rgb-color>) 
    ((text  :init-keyword :text :init-value "" :observer component-observer)))

  (define-class <action> ()
    ((control   :init-keyword :control)
     (operation :init-keyword :operation)))
)