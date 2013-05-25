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
	    (rnrs mutable-pairs)
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

  (define-method slot-unbound ((c <class>) (o <component>) (s (eql 'x-point)))
    0)
  (define-method slot-unbound ((c <class>) (o <component>) (s (eql 'y-point)))
    0)
  (define-method slot-unbound ((c <class>) (o <component>) (s (eql 'width)))
    CW_USEDEFAULT)
  (define-method slot-unbound ((c <class>) (o <component>) (s (eql 'height)))
    CW_USEDEFAULT)

  (define-method slot-unbound ((c <class>) (o <window>) (s (eql 'x-point)))
    CW_USEDEFAULT)
  (define-method slot-unbound ((c <class>) (o <window>) (s (eql 'y-point)))
    CW_USEDEFAULT)

  (define (%create-window comp owner id)
    (let1 context (~ comp 'context)
      (create-window-ex (or (lookup-window-style context) 0)
			(lookup-class-name context)
			(~ context 'name)
			(bitwise-ior (context-style context) 
				     (visible-flag comp))
			(~ comp 'x-point)
			(~ comp 'y-point)
			(~ comp 'width)
			(~ comp 'height)
			owner
			id
			hinstance
			(object->pointer comp))))

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
	  (((? (lambda (x) (eq? 'else (identifier->symbol x))) -) . body)
	   `(else ,@body))))
      `(let ((,tmp ,obj))
	 (cond ,@(map expand-clause clauses)))))
  (define-method lookup-operation ((b <button>) op)
    (case/unquote op
      ((BN_CLICKED) 'click)
      ((BN_DOUBLECLICKED) 'double-click)
      ((BN_SETFOCUS) 'focus)
      ((BN_KILLFOCUS) 'blur)
      (else op)))
  (define-method lookup-operation ((t <text>) op)
    (case/unquote op
      ((EN_SETFOCUS) 'focus)
      ((EN_KILLFOCUS) 'blur)
      ((EN_CHANGE) 'change)
      ((EN_UPDATE) 'update)
      ((EN_HSCROLL) 'horizontal-scroll)
      ((EN_VSCROLL) 'virtical-scroll)
      (else op)))
  (define-method lookup-operation ((t <list-box>) op)
    (case/unquote op
      ((LBN_SELCHANGE) 'selection-change)
      ((LBN_DBLCLK)    'double-click)
      ((LBN_SELCANCEL) 'selection-cancel)
      ((LBN_SETFOCUS)  'focus)
      ((LBN_KILLFOCUS) 'blur)
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

  (define (add-specific! context name value)
    (push! (~ context 'platform-data) (cons name value)))
  (define (update-specific! context name new-value)
    (or (and-let* ((s (assq  name (~ context 'platform-data))))
	  (set-cdr! s new-value))
	(add-specific! context name new-value)))
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
		       (update (datum->syntax #'k (genname "update" #'name #t)))
		       (lookup (datum->syntax #'k (genname "lookup" #'name))))
	   #'(begin
	       (define (append ctx value)
		 (add-specific! ctx 'name value))
	       (define (update ctx value)
		 (update-specific! ctx 'name value))
	       (define (lookup ctx)
		 (lookup-specific! ctx 'name))))))))

  (define-platform-data class-name)
  (define-platform-data style)
  (define-platform-data window-style)

  (define (visible-flag comp)
    (if (~ comp 'visible) WS_VISIBLE 0))

  (define-method window-close ((w <window>))
    (destroy-window (~ w 'context 'handle))
    (set! (~ w 'context 'handle) #f)
    (set! (~ w 'visible) #f)
    0)
  
  (define-syntax ash (identifier-syntax bitwise-arithmetic-shift))

  (define window-proc
    (c-callback 
     void* (HWND unsigned-int WPARAM LPARAM)
     (lambda (hwnd imsg wparam lparam)
       (define (get-window hwnd)
	 (let1 p (get-window-long-ptr hwnd GWLP_USERDATA)
	   (if (null-pointer? p)
	       #f
	       (pointer->object p))))
       (define (lookup-control w id)
	 (let loop ((components (~ w 'components)))
	   (cond ((null? components) #f)
		 ((= (~ (car components) 'context 'id) id) (car components))
		 (else (loop (cdr components))))))
       (cond ((= imsg WM_CREATE)
	      ;; save the lpCreateParams of CREATESTRUCT
	      (set-window-long-ptr 
	       hwnd GWLP_USERDATA 
	       (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	     ((= imsg WM_ERASEBKGND)
	      (let* ((rect (allocate-c-struct RECT))
		     (hdc (get-dc hwnd))
		     (hbrush 
		      (get-stock-object
		       (lookup-color (~ (get-window hwnd) 'background)))))
		(get-update-rect hwnd rect #f)
		(fill-rect hdc rect hbrush)
		1))
	     ((= imsg WM_CLOSE) (window-close (get-window hwnd)))
	     ((= imsg WM_DESTROY)
	      (let1 w (get-window hwnd)
		(cond ((and (not (~ w 'owner))
			    (eq? w (*current-root-window*)))
		       (post-quit-message 0) 0)
		      (else 1))))
	     ((= imsg WM_COMMAND) ; button or so?
	      ;; get loword (lower 16 bits)
	      (let* ((id (bitwise-and wparam #xFFFF))
		     (op (bitwise-and (ash wparam -16) #xFFFF))
		     (wd (~ (get-window hwnd) 'context 'control-map id))
		     ;; todo make action
		     (action (make <action>
			       :control wd
			       :operation (lookup-operation wd op))))
		(when (and wd (is-a? wd <performable>))
		  (for-each (^p (p wd action)) (~ wd 'actions)))))
	     ((or (= imsg WM_CTLCOLORSTATIC)
		  (= imsg WM_CTLCOLOREDIT))
	      (or (and-let* ((control (lookup-control 
				       (get-window hwnd)
				       (pointer->integer (get-window-long-ptr
							  lparam GWLP_ID))))
			     (hdc (integer->pointer wparam))
			     (c (~ control 'color)))
		    (set-text-color hdc (rgb (~ c 'r) (~ c 'g) (~ c 'b)))
		    (set-bk-mode hdc TRANSPARENT)
		    (pointer->integer
		     (get-stock-object (lookup-color (~ control 'background)))))
		  (def-window-proc hwnd imsg wparam lparam)))
	     ((= imsg WM_CTLCOLORBTN)
	      (def-window-proc hwnd imsg wparam lparam)
	      ;;(pointer->integer (get-stock-object BLACK_BRUSH))
	      )
	     (else
	      (def-window-proc hwnd imsg wparam lparam))))))
  ;; register window-class
  (define *window-class-name* "turquoise-window")
  (let ((wnd (allocate-c-struct WNDCLASSEX)))
    (let-syntax ((wnd-set! (syntax-rules ()
			     ((_ p v) (c-struct-set! wnd WNDCLASSEX 'p v)))))
      (wnd-set! cbSize (size-of-c-struct WNDCLASSEX))
      (wnd-set! lpfnWndProc window-proc)
      (wnd-set! style (bitwise-ior CS_HREDRAW CS_VREDRAW))
      (wnd-set! hInstance hinstance)
      (wnd-set! hIcon (load-icon null-pointer IDI_APPLICATION))
      (wnd-set! hCursor (load-cursor null-pointer IDC_ARROW))
      (wnd-set! hbrBackground (get-stock-object WHITE_BRUSH))
      (wnd-set! lpszClassName *window-class-name*)
      (wnd-set! hIconSm null-pointer)
      (when (zero? (register-class-ex wnd))
	(error 'make-window "Failed to register WNDCLASS"))))

  (define-method make-window ((w <window>))
    (let1 context (~ w 'context)
      (add-class-name! context *window-class-name*)
      ;; TODO probably we need a slot for this in context so that
      ;; user can select the window style
      ;;(add-window-style! context WS_EX_PALETTEWINDOW)
      (add-window-style! context WS_EX_APPWINDOW)
      (let1 style (bitwise-ior WS_OVERLAPPEDWINDOW WS_CLIPCHILDREN)
	(add-style! context style))
      w))

  (define-method sync-component ((comp <text>))
    (lambda (component action)
      (when (eq? (~ action 'operation) 'update)
	;; whatever action will update the value
	(with-busy-component comp
	  (let* ((hwnd (~ comp 'context 'handle))
		 (len  (pointer->integer
			(send-message hwnd WM_GETTEXTLENGTH 0 null-pointer)))
		 (buf  (make-bytevector len)))
	    (send-message hwnd WM_GETTEXT (+ len 1) buf)
	    ;; UTF8?
	    (set! (~ comp 'value) (utf8->string buf)))))))

  (define-method sync-component ((comp <check-box>))
    (lambda (component action)
      (when (eq? (~ action 'operation) 'click)
	;; whatever action will update the value
	(with-busy-component comp
	  (let* ((hwnd (~ comp 'context 'handle))
		 (ret  (pointer->integer
			(send-message hwnd BM_GETCHECK 0 null-pointer))))
	    (set! (~ comp 'checked)
		  (cond ((= ret BST_CHECKED) #t)
			((= ret BST_UNCHECKED) #f)
			((= ret BST_INDETERMINATE) '()))))))))

  (define-method sync-component ((comp <list-box>))
    (lambda (component action)
      (when (eq? (~ action 'operation) 'selection-change)
	;; whatever action will update the value
	(with-busy-component comp
	  (let* ((hwnd (~ comp 'context 'handle))
		 (index (send-message hwnd LB_GETCURSEL 0 null-pointer))
		 (item  (send-message hwnd LB_GETITEMDATA 
				      (pointer->integer index) null-pointer)))
	    (set! (~ component 'selected) (pointer->object item)))))))

  ;; initialise
  (define-method on-initialize ((comp <component>))
    (update-component comp))
  ;; updater around
  (define-method update-component :around ((comp <component>))
    (when (and (slot-bound? comp 'context) 
	       (~ comp 'context 'handle)
	       (not (~ comp 'busy)))
      (call-next-method)))

  (define-method show ((comp <component>))
    (let1 context (~ comp 'context)
      (unless (~ context 'handle)
	(let* ((owner (~ comp 'owner))
	       (id    (~ context 'id))
	       (hwnd (%create-window
		      comp 
		      (if owner 
			  (~ owner 'context 'handle)
			  null-pointer)
		      (if id (integer->pointer id) null-pointer))))
	  (set! (~ context 'handle) hwnd)
	  (on-initialize comp)
	  ;; bit awkward solution
	  (when (is-a? comp <performable>)
	    ;; put the value retriever the first
	    (set! (~ comp 'actions) 
		  (cons (sync-component comp) (~ comp 'actions))))))
      (when (~ comp 'visible)
	(let1 hwnd (~ context 'handle)
	  (show-window hwnd SW_SHOW)
	  (update-window hwnd)))))

  (define-method show ((container <container>))
    (call-next-method)
    (for-each show (~ container 'components)))

  (define-method hide ((comp <component>))
    (let1 hwnd (~ comp 'context 'handle)
      (when hwnd
	(show-window hwnd SW_HIDE)
	(update-window hwnd)
	(set! (~ comp 'visible) #f))))

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
    (let1 context (~ comp 'context)
      (update-style! context (bitwise-ior WS_CHILD (context-style context)))
      (unless (~ context 'id) (set! (~ context 'id) (generate-id))))
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

  (define-method initialize ((comp <button>) initargs)
    (call-next-method)
    (let* ((context (~ comp 'context))
	   (style (bitwise-ior (context-style context)
			       BS_NOTIFY
			       (lookup-button-style (~ comp 'style)))))
      (add-class-name! context "BUTTON")
      (add-style! context style)
      (add-window-style! context WS_EX_STATICEDGE)))

  (define-method initialize ((comp <radio>) initargs)
    (set! (~ comp 'style) 'radio)
    (call-next-method))

  (define-method initialize ((comp <check-box>) initargs)
    ;; FIXME shouldn't be like this ...
    (if (is-a? comp <tri-state-check-box>)
	(set! (~ comp 'style) 'tri-state)
	(set! (~ comp 'style) 'check))
    (call-next-method))

  (define-method update-component ((comp <check-box>))
    ;; set default text here
    (let1 hwnd (~ comp 'context 'handle)
      (send-message hwnd BM_SETCHECK
		    (let1 v (~ comp 'checked)
		      (if (boolean? v)
			  (if v BST_CHECKED BST_UNCHECKED)
			  BST_INDETERMINATE))
		    null-pointer)
      (update-window hwnd)))

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

  (define-method initialize ((text <text>) initargs)
    (call-next-method)
    (let* ((context (~ text 'context))
	   (style (bitwise-ior (context-style context)
			       (lookup-edit-style (~ text 'style)))))
      (add-class-name! context "EDIT")
      (add-style! context style)
      (add-window-style! context WS_EX_STATICEDGE)))

  (define-method update-component ((comp <text>))
    ;; set default text here
    (let1 hwnd (~ comp 'context 'handle)
      (send-message hwnd WM_SETTEXT 0 (~ comp 'value))
      (update-window hwnd)))

  (define-method initialize ((text <text-area>) initargs)
    (let1 context (~ text 'context)
      (add-style! context (bitwise-ior ES_WANTRETURN ES_MULTILINE
				       ES_AUTOHSCROLL ES_AUTOVSCROLL
				       WS_VSCROLL WS_HSCROLL))
      (call-next-method)))

  (define-method initialize ((text <list-box>) initargs)
    (let1 context (~ text 'context)
      (add-class-name! context "LISTBOX")
      (add-style! context (bitwise-ior LBS_NOTIFY WS_VSCROLL WS_VSCROLL
				       WS_BORDER))
      (add-window-style! context WS_EX_STATICEDGE)
      (call-next-method)))
  (define-method update-component ((lst <list-box>))
    (define (add-item hwnd item)
      (unless (slot-bound? item 'index)
	(let1 index (send-message hwnd LB_ADDSTRING 0 (~ item 'label))
	  (set! (~ item 'index) (pointer->integer index))
	  (send-message hwnd LB_SETITEMDATA
			(pointer->integer index) (object->pointer item)))))
    (let1 hwnd (~ lst 'context 'handle)
      (for-each (cut add-item hwnd <>) (reverse (~ lst 'items)))
      (when (~ lst 'selected)
	(send-message hwnd LB_SETSEL 1 
		      (integer->pointer (~ lst 'selected 'index)))))
    (call-next-method))

  (define-method add! ((ls <list-box>) (item <list-item>))
    (push! (~ ls 'items) item))

  (define-method initialize ((text <label>) initargs)
    (let1 context (~ text 'context)
      (add-class-name! context "STATIC")
      (add-window-style! context WS_EX_STATICEDGE)
      ;;(set! (~ text 'context 'id) 0)
      (call-next-method)))

  (define-method update-component ((text <label>))
    (set-window-text (~ text 'context 'handle) (~ text 'text))
    (update-window (~ text 'context 'handle))
    (call-next-method))
)