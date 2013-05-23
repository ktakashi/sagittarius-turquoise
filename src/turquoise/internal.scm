;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; turquoise/internal - Internal method interface
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

;; exports interface methods to be implemented by underlying
;; platform specific implementations.
(library (turquoise internal)
    (export <component-ctx> <window-ctx>
	    make-window show hide message-loop
	    add! window-close on-initialize
	    *current-root-window*)
    (import (rnrs) (clos user) (sagittarius mop validator)
	    (srfi :39 parameters))

  (define *current-root-window* (make-parameter #f))

  (define-class <component-ctx> (<validator-mixin>)
    ;; platform specific window object.
    ((handle        :init-value #f)
     (id            :init-value #f)
     (x-point       :init-keyword :x-point :init-value 0)
     (y-point       :init-keyword :y-point :init-value 0)
     ;; how much?
     (width         :init-keyword :width   :init-value 100)
     (height        :init-keyword :height  :init-value 20)
     (style         :init-keyword :style   :init-value '())
     (visible       :init-keyword :visible :init-value #t)
     ;; data store for platform specific values
     (platform-data :init-value ())
     (name  :init-keyword :name :init-value "No title"
	    :validator (lambda (o v)
			 (unless (string? v)
			   (assertion-violation 'window-ctx
						"name must be string" v))
			 v))))
  ;; internal window context
  (define-class <window-ctx> (<component-ctx>) 
    ((background :init-keyword :background :init-value 'white)
     (control-map :init-form (make-eqv-hashtable))))

  (define-generic make-window)
  ;; set given window visible
  (define-generic show)
  (define-generic hide)
  ;; loop
  (define-generic message-loop)

  (define-generic add!)

  (define-generic window-close)
  (define-generic on-initialize)
  (define-method on-initialize (c) #t)
)