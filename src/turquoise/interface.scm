;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; turquoise/interface - Interface layer.
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

(library (turquoise interface)
    (export show hide add! button-style text-style
	    visible? on-initialize sync-component
	    ;; some others
	    default-action sync-action
	    ;; low level
	    resize-component
	    move-component
	    create-cursor
	    set-cursor!
	    capture-component
	    uncapture-component
	    safe-position-set!

	    ;; hooks
	    window-close)
    (import (rnrs)
	    (clos user)
	    (turquoise components)
	    (turquoise internal)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (srfi :39 parameters))

  ;; TODO how much should i initialise with the method?
  (define-method initialize :before ((w <window>) initargs)
    (let1 ctx (apply make <window-ctx> initargs)
      (set! (~ w 'context) ctx)
      w))

  (define-method initialize ((w <window>) initargs)
    (call-next-method)
    (make-window w))

  ;; For some limitation we create components like button
  ;; later
  (define-method initialize :before ((component <component>) initargs)
    (unless (slot-bound? component 'context)
      (let1 ctx (apply make <component-ctx> initargs)
	(set! (~ component 'context) ctx)
	component)))
  
  (define-method initialize ((component <component>) initargs)
    (call-next-method))

  (define-method object-apply ((window <window>))
    (parameterize ((*current-root-window* window))
      (show window)
      (message-loop window)))

  (define (visible? comp)
    (~ comp 'context 'visible))

  (define-syntax button-style
    (syntax-rules ()
      ((_ style) 'style)))

  (define-syntax text-style
    (syntax-rules ()
      ((_ styles ...) '(styles ...))))

)