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
     <text> <text-area> <label>
     <combo-box> <list-box>
     <item> <list-item>
     <scroll>
     ;; container widgets
     <window> <frame> <dialog> <panel> <split-panel>
     ;; menu
     <menu-component> <menu> <menu-item>
     ;; mixins
     <menu-bar-container> <content-panel-container> <performable>
     ;; event
     <event>
     ;; misc
     <rgb> <rgb-color> <file-select>
     ;; component synchroniser
     update-component
     )
    (import (rnrs) 
	    (clos user)
	    (srfi :26 cut)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius mop validator))


  (define-generic update-component)
  ;; default do nothing
  (define-method update-component (c) #t)
  (define (component-observer component value)
    ;; component has value already
    (update-component component))

  ;; TODO what should component have?
  (define-class <component> (<validator-mixin>)
    ((context)
     ;; owner component, #f means root component
     (owner      :init-keyword :owner :init-value #f)
     ;; position and style
     (x-point    :init-keyword :x-point :observer component-observer)
     (y-point    :init-keyword :y-point :observer component-observer)
     (width      :init-keyword :width :observer component-observer)
     (height     :init-keyword :height :observer component-observer)
     ;; bad naming, this actually adjust component size to
     ;; parent component size!
     (adjust-size :init-keyword :adjust-size :init-value #f
		  :observer component-observer)
     (style      :init-keyword :style   :init-value '())
     (visible    :init-keyword :visible :init-value #t)
     (background :init-keyword :background :init-value 'white
		 :observer component-observer)
     ;; component name
     (name  :init-keyword :name :init-value "undefined"
	    :validator (lambda (o v)
			 (unless (string? v)
			   (error 'window-ctx "name must be string" v))
			 v)
	    :observer component-observer)
     ;; if this component is busy or not (doing something)
     (busy    :init-value #f)))

  (define-class <container> (<component>)
    ;; container can contain component
    ((components :init-keyword :components :init-value '())))

  ;; what should this class have?
  (define-class <window> (<container>)
    ((handlers :init-value '())))

  ;; mixins
  (define-class <content-panel-container> ()
    ((root-panel :init-keyword :root-panel)))

  (define-class <menu-bar-container> ()
    ((menu-bar :init-keyword :menu-bar :init-value #f)))

  (define-class <frame> 
    (<menu-bar-container> <content-panel-container> <window>)
    ())

  (define-class <dialog> (<content-panel-container> <window>)
    ())

  ;; panel is not a window but container
  (define-class <panel> (<container>) ())
  ;; split-panel have 2 empty panel
  (define-class <split-panel> (<panel>)
    ((panel1 :init-keyword :panel1 :init-form (make <panel>))
     (panel2 :init-keyword :panel2 :init-form (make <panel>))
     (virtical :init-keyword :virtical :init-value #f)))

  ;; performable have actions (procedure list)
  (define-class <performable> ()
    ((actions :init-value '())))

  (define-class <menu-component> (<component>) ())

  (define-class <menu> (<menu-component>)
    ;; TODO what's the standard menu for platform?
    ((root :init-value #f)
     (items :init-value '()
	    :validator (lambda (o v)
			 (unless (for-all (cut is-a? <> <menu-component>) v)
			   (error 'menu "all elements must be menu-component"
				  v))
			 v))))

  (define-class <menu-item> (<menu-component> <performable>)
    ;; TODO what's the standard menu for platform?
    ((type :init-keyword :type :init-value 'string)))

  (define-class <button> (<component> <performable>) ())
  (define-class <radio> (<button>) ())
  (define-class <check-box> (<button>)
    ((checked :init-keyword :checked :init-value #f
	      :observer component-observer)))
  (define-class <tri-state-check-box> (<check-box>) ())
  (define-method initialize ((tc <tri-state-check-box>) initargs)
    (call-next-method)
    (set! (~ tc 'checked) (get-keyword :checked initargs '())))

  ;; abstract class
  (define-class <item> () ())
  (define-class <list-item> ()
    ;; index is for management
    ((index)
     (label :init-keyword :label :init-value "")))

  (define-class <combo-box> (<component> <performable>) ())
  (define-class <list-box>  (<component> <performable>)
    ((items :init-keyword :items :init-value '())
     (selected :init-value #f
	       :validator (lambda (o v)
			    (unless (memq v (~ o 'items))
			      (error 'list-box "item is not added" v))
			    v)
	       :observer component-observer)))
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

  (define-class <event> ()
    ((control :init-keyword :control)
     (action  :init-keyword :action)))
  
  (define-method write-object ((a <event>) p)
    (format p "#<event ~a>" (~ a 'action)))

  ;; this is not component but for utility
  (define-class <file-select> ()
    ((title   :init-keyword :title :init-value "No title")
     (type    :init-keyword :type) ;; user must set
     ;; list of string
     (filters :init-keyword :filters :init-value '("*.*"))
     (show-readonly :init-keyword :show-read-only :init-value #f)
     (extension :init-keyword :extension :init-value "")))

)