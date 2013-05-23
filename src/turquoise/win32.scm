;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; turquoise/win32 - Win32 implementation.
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

(library (turquoise win32)
    (export)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius ffi)
	    (sagittarius control)
	    (sagittarius object)
	    (win32 user)
	    (win32 defs)
	    (win32 common-dialog)
	    (win32 kernel)
	    (win32 gdi)
	    (match)
	    (turquoise components)
	    (turquoise internal)
	    (srfi :26 cut)
	    (srfi :39 parameters))

  (define hinstance (get-module-handle null-pointer))

  (define generate-id (let ((id 0)) (lambda () (set! id (+ id 1)) id)))

  (define (%create-window context owner id)
    (create-window-ex (lookup-window-style context)
		      (lookup-class-name context)
		      (~ context 'name)
		      (lookup-style context)
		      (~ context 'x-point)
		      (~ context 'y-point)
		      (~ context 'width)
		      (~ context 'height)
		      owner
		      id
		      hinstance
		      null-pointer))
  ;; FIXME implement this!
  (define (lookup-operation op)
    (define-macro (case/unquote obj . clauses)
      (let1 tmp (gensym)
	(define (expand-clause clause)
	  (match clause
	    (((item) . body)
	     `((eqv? ,tmp ,item) ,@body))
	    (((item ___) . body)
	     (let1 ilist (list 'quasiquote
			       (map (cut list 'unquote <>) item))
	       `((memv ,tmp ,ilist) ,@body)))
	    (((? (lambda (x) (eq? 'else x)) -) . body)
	     `(else ,@body))))
	`(let ((,tmp ,obj))
	   (cond ,@(map expand-clause clauses)))))
    (case/unquote op
      ((BN_CLICKED)
       'click)
      ((BN_DOUBLECLICKED)
       'double-click)
      ((BN_SETFOCUS EN_SETFOCUS)
       'focus)
      ((BN_KILLFOCUS EN_KILLFOCUS)
       'blur)
      ((EN_CHANGE) 'change)
      ((EN_UPDATE) 'update)
      ((EN_HSCROLL) 'horizontal-scroll)
      ((EN_VSCROLL) 'virtical-scroll)
      (else op)))

  (define (lookup-color color)
    (case color
      ((white) WHITE_BRUSH)
      ((black) BLACK_BRUSH)
      ((gray)  GRAY_BRUSH)
      ((light-gray) LTGRAY_BRUSH)
      ((dark-gray) DKGRAY_BRUSH)
      ((empty)     HOLLOW_BRUSH)
      (else WHITE_BRUSH)))

  (define (add-specific! context name class-name)
    (push! (~ context 'platform-data) (cons name class-name)))
  (define (lookup-specific! context name)
    (and-let* ((s (assq  name (~ context 'platform-data))))
      (cdr s)))

  (define-syntax define-platform-data
    (lambda (x)
      (define (genname prefix name :optional (setter? #f))
	(string->symbol (format "~a-~a~a" prefix (syntax->datum name)
				(if setter? "!" ""))))
      (syntax-case x ()
	((k name)
	 (with-syntax ((append (datum->syntax #'k (genname "add" #'name #t)))
		       (lookup (datum->syntax #'k (genname "lookup" #'name))))
	   #'(begin
	       (define (append ctx value)
		 (add-specific! ctx 'name value))
	       (define (lookup ctx)
		 (lookup-specific! ctx 'name))))))))

  (define-platform-data class-name)
  (define-platform-data style)
  (define-platform-data window-style)

  (define (visible-flag context)
    (if (~ context 'visible) WS_VISIBLE 0))

  (define-method window-close ((w <window>))
    (destroy-window (~ w 'context 'handle))
    (set! (~ w 'context 'handle) #f)
    (set! (~ w 'context 'visible) #f)
    0)
  
  (define-syntax ash (identifier-syntax bitwise-arithmetic-shift))
  (define-method make-window ((w <window>))
    (define win-proc
      (c-callback void* (HWND unsigned-int WPARAM LPARAM)
		  (lambda (hwnd imsg wparam lparam)
		    ;; TODO make handle
		    (cond ((= imsg WM_CLOSE) (window-close w))
			  ((= imsg WM_DESTROY)
			   (cond ((and (not (~ w 'owner))
				       (eq? w (*current-root-window*)))
				  (post-quit-message 0) 0)
				 (else 1)))
			  ((= imsg WM_COMMAND) ; button or so?
			   ;; get loword (lower 16 bits)
			   (let* ((id (bitwise-and wparam #xFFFF))
				  (op (bitwise-and (ash wparam -16) #xFFFF))
				  (wd (~ w 'context 'control-map id))
				  ;; todo make action
				  (action (make <action>
					    :control wd
					    :operation (lookup-operation op))))
			     (when (and wd (is-a? wd <performable>))
			       (for-each (^p (p wd action)) (~ wd 'actions))))
			   )
			  (else
			   (def-window-proc hwnd imsg wparam lparam))))))

    (let ((wnd (allocate-c-struct WNDCLASSEX))
	  (context (~ w 'context))
	  ;; TODO should we support user defined class name?
	  (class-name (format "~a" (gensym "window-class"))))
      (add-class-name! context class-name)
      ;; TODO probably we need a slot for this in context so that
      ;; user can select the window style
      ;;(add-window-style! context WS_EX_PALETTEWINDOW)
      (add-window-style! context WS_EX_APPWINDOW)
      (let-syntax ((wnd-set! (syntax-rules ()
			       ((_ p v) (c-struct-set! wnd WNDCLASSEX 'p v)))))
	(wnd-set! cbSize (size-of-c-struct WNDCLASSEX))
	(wnd-set! lpfnWndProc win-proc)
	;; TODO style
	(wnd-set! style (bitwise-ior CS_HREDRAW CS_VREDRAW))
	;; TODO check parent
	(wnd-set! hInstance hinstance)
	;; TODO get icon from context
	(wnd-set! hIcon (load-icon null-pointer IDI_APPLICATION))
	(wnd-set! hCursor (load-cursor null-pointer IDC_ARROW))
	(wnd-set! hbrBackground 
		  (get-stock-object (lookup-color (~ context 'background))))
	(wnd-set! lpszClassName class-name)
	(wnd-set! hIconSm null-pointer)
	(if (zero? (register-class-ex wnd))
	    (error 'make-window "Failed to register WNDCLASS")
	    (let1 style (bitwise-ior WS_OVERLAPPEDWINDOW
				     (visible-flag context))
	      (add-style! context style)))
	w)))
  (define (default-retriver comp action))
  (define-method control-value-retriever ((comp <component>)) default-retriver)
  (define-method control-value-retriever ((comp <text>))
    ;; set default text here
    (let1 hwnd (~ comp 'context 'handle)
      (send-message hwnd WM_SETTEXT 0 (~ comp 'value))
      (update-window hwnd))
    (lambda (component action)
      (when (eq? (~ action 'operation) 'update)
	;; whatever action will update the value
	(let* ((hwnd (~ comp 'context 'handle))
	       (len  (pointer->integer
		      (send-message hwnd WM_GETTEXTLENGTH 0 null-pointer)))
	       (buf  (make-bytevector len)))
	  (send-message hwnd WM_GETTEXT (+ len 1) buf)
	  ;; UTF8?
	  (set! (~ comp 'value) (utf8->string buf))))))

  (define-method control-value-retriever ((comp <check-box>))
    ;; set default text here
    (let1 hwnd (~ comp 'context 'handle)
      (send-message hwnd BM_SETCHECK
		    (let1 v (~ comp 'checked)
		      (if (boolean? v)
			  (if v BST_CHECKED BST_UNCHECKED)
			  BST_INDETERMINATE))
		    null-pointer)
      (update-window hwnd))
    (lambda (component action)
      (when (eq? (~ action 'operation) 'click)
	;; whatever action will update the value
	(let* ((hwnd (~ comp 'context 'handle))
	       (ret  (pointer->integer
		      (send-message hwnd BM_GETCHECK 0 null-pointer))))
	  (set! (~ comp 'checked)
		(cond ((= ret BST_CHECKED) #t)
		      ((= ret BST_UNCHECKED) #f)
		      ((= ret BST_INDETERMINATE) '())))))))

  (define (%show comp show-child?)
    (let1 context (~ comp 'context)
      (unless (~ context 'handle)
	(let* ((owner (~ comp 'owner))
	       (id    (~ context 'id))
	       (hwnd (%create-window
		      context 
		      (if owner 
			  (~ owner 'context 'handle)
			  null-pointer)
		      (if id (integer->pointer id) null-pointer))))
	  (set! (~ context 'handle) hwnd)
	  ;; bit awkward solution
	  (when (is-a? comp <performable>)
	    ;; put the value retriever the first
	    (set! (~ comp 'actions) 
		  (cons (control-value-retriever comp)
			(~ comp 'actions))))))
      (when show-child?
	(let1 hwnd (~ context 'handle)
	  (show-window hwnd SW_SHOW)
	  (update-window hwnd)
	  (set! (~ context 'visible) #t)))))

  (define-method show ((comp <component>))
    (%show comp #t))

  (define-method show ((container <container>))
    (call-next-method)
    (for-each (cut %show <> #f) (~ container 'components)))

  (define-method hide ((comp <component>))
    (let1 hwnd (~ comp 'context 'handle)
      (when hwnd
	(show-window hwnd SW_HIDE)
	(update-window hwnd)
	(set! (~ comp 'context 'visible) #f))))

  (define-method message-loop ((w <window>))
    (let ((msg (allocate-c-struct MSG)))
      (let loop ((m (get-message msg null-pointer 0 0)))
	(when m
	  (translate-message msg)
	  (dispatch-message msg)
	  (loop (get-message msg null-pointer 0 0))))
      (c-struct-ref msg MSG 'wParam)))

  (define-method add! ((container <container>) (comp <component>))
    (set! (~ comp 'owner) container)
    (push! (~ container 'components) comp))

  (define-method add! ((w <window>) (comp <component>))
    ;; do super method first otherwise id is not set yet.
    (call-next-method)
    (let ((id (~ comp 'context 'id))
	  (context (~ w 'context)))
      (set! (~ context 'control-map id) comp)))

  (define (lookup-button-style style)
    (case style
      ((push)  	   BS_PUSHBUTTON)
      ((check) 	   BS_AUTOCHECKBOX)
      ((radio) 	   BS_AUTORADIOBUTTON)
      ((tri-state) BS_AUTO3STATE)
      ((push-box)  BS_PUSHBOX)
      (else        BS_PUSHBUTTON)))

  (define (context-style context)
    (or (lookup-style context) 0))

  (define-method add! ((container <container>) (comp <button>))
    ;; creates button handle
    (let* ((context (~ comp 'context))
	   (style (bitwise-ior (context-style context)
			       WS_CHILD (visible-flag context)
			       (lookup-button-style (~ context 'style)))))
      (add-class-name! context "BUTTON")
      (add-style! context style)
      (add-window-style! context WS_EX_STATICEDGE)
      (set! (~ context 'id) (generate-id))
      (call-next-method)))

  (define-method add! ((container <container>) (comp <radio>))
    (let1 context (~ comp 'context)
      (set! (~ context 'style) 'radio))
    (call-next-method))

  (define-method add! ((container <container>) (comp <check-box>))
    (let1 context (~ comp 'context)
      ;; FIXME shouldn't be like this ...
      (if (is-a? comp <tri-state-check-box>)
	  (set! (~ context 'style) 'tri-state)
	  (set! (~ context 'style) 'check)))
    (call-next-method))

  (define-method add! ((p <performable>) (proc <procedure>))
    (push! (~ p 'actions) proc))

  (define (lookup-edit-style styles)
    (fold-left bitwise-ior 0 (map (^(style)
			       (case style
				 ((left)      ES_LEFT)
				 ((center)    ES_CENTER)
				 ((right)     ES_RIGHT)
				 ((multiline) ES_MULTILINE)
				 ((uppercase) ES_UPPERCASE)
				 ((lowercase) ES_LOWERCASE)
				 ((password)  ES_PASSWORD)
				 ((virtical-scroll)   ES_AUTOVSCROLL)
				 ((horizontal-scroll) ES_AUTOHSCROLL)
				 ((no-hide-selection) ES_NOHIDESEL)
				 ((readonly)  ES_READONLY)
				 ((want-return) ES_WANTRETURN)
				 ((number)    ES_NUMBER)
				 (else 0))) styles)))

  (define-method add! ((container <container>) (text <text>))
    (let* ((context (~ text 'context))
	   (style (bitwise-ior (context-style context)
			       WS_CHILD (visible-flag context)
			       (lookup-edit-style 
				(~ context 'style)))))
      (add-class-name! context "EDIT")
      (add-style! context style)
      (add-window-style! context WS_EX_STATICEDGE)
      (set! (~ context 'id) (generate-id))
      (call-next-method)))

  (define-method add! ((container <container>) (text <text-area>))
    (let1 context (~ text 'context)
      #|
      (push! (~ context 'style) 'want-return)
      (push! (~ context 'style) 'multiline)
      (push! (~ context 'style) 'horizontal-scroll)
      (push! (~ context 'style) 'virtical-scroll)
      |#
      (add-style! context (bitwise-ior ES_WANTRETURN ES_MULTILINE
				       ES_AUTOHSCROLL ES_AUTOVSCROLL
				       WS_VSCROLL WS_HSCROLL))
      (call-next-method)))

  ;; misc
  (define-method initialize ((ctx <window-ctx>) initargs)
    (call-next-method)
    (set! (~ ctx 'x-point) (get-keyword :x-point initargs CW_USEDEFAULT))
    (set! (~ ctx 'y-point) (get-keyword :y-point initargs CW_USEDEFAULT))
    (set! (~ ctx 'width)   (get-keyword :width initargs CW_USEDEFAULT))
    (set! (~ ctx 'height)  (get-keyword :height initargs CW_USEDEFAULT))
    )

)