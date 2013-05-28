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

  (define (%create-window comp owner)
    (let* ((context (~ comp 'context))
	   (id      (~ context 'id)))
      (create-window-ex (or (lookup-window-style context) 0)
			(lookup-class-name context)
			(~ comp 'name)
			(bitwise-ior (if (null-pointer? owner)
					 0
					 WS_CHILD)
				     (context-style context) 
				     (visible-flag comp))
			(~ comp 'x-point)
			(~ comp 'y-point)
			(~ comp 'width)
			(~ comp 'height)
			owner
			(if id (integer->pointer id) null-pointer)
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
  (define-method lookup-action ((b <button>) op)
    (case/unquote op
      ((BN_CLICKED) 'click)
      ((BN_DOUBLECLICKED) 'double-click)
      ((BN_SETFOCUS) 'focus)
      ((BN_KILLFOCUS) 'blur)
      (else op)))
  (define-method lookup-action ((t <text>) op)
    (case/unquote op
      ((EN_SETFOCUS) 'focus)
      ((EN_KILLFOCUS) 'blur)
      ((EN_CHANGE) 'change)
      ((EN_UPDATE) 'update)
      ((EN_HSCROLL) 'horizontal-scroll)
      ((EN_VSCROLL) 'virtical-scroll)
      (else op)))
  (define-method lookup-action ((t <list-box>) op)
    (case/unquote op
      ((LBN_SELCHANGE) 'selection-change)
      ((LBN_DBLCLK)    'double-click)
      ((LBN_SELCANCEL) 'selection-cancel)
      ((LBN_SETFOCUS)  'focus)
      ((LBN_KILLFOCUS) 'blur)
      (else op)))

  ;; I have no idea whether or not menu has any other action
  (define-method lookup-action ((t <menu-item>) op) 'click)

  (define *color-table* (make-eqv-hashtable))
  (define (lookup-color color)
    (cond ((symbol? color)
	   ;; lookup stock object
	   (get-stock-object (case color
				((white) WHITE_BRUSH)
				((black) BLACK_BRUSH)
				((gray)  GRAY_BRUSH)
				((light-gray) LTGRAY_BRUSH)
				((dark-gray) DKGRAY_BRUSH)
				((empty)     HOLLOW_BRUSH)
				(else WHITE_BRUSH))))
	  ((is-a? color <rgb>)
	   (let1 c (rgb (~ color 'r) (~ color 'g) (~ color 'b))
	     (cond ((~ *color-table* c))
		   (else
		    (let1 brush (create-solid-brush c)
		      (set! (~ *color-table* c) brush)
		      brush)))))
	  (else
	   (error 'color "symbol or <rgb> object required" color))))

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

  (define (search-menu w menu id)
    (let loop ((item (~ menu 'items)))
      (cond ((null? item) #f)
	    ((is-a? (car item) <menu>)
	     (or (search-menu w (car item) id)
		 (loop (cdr item))))
	    ((is-a? (car item) <menu-item>)
	     (if (eqv? id (~ (car item) 'context 'id))
		 (begin
		   ;; set id to control-map so that next time
		   ;; it'll be O(1)
		   (set! (~ w 'context 'control-map id) (car item))
		   (car item))
		 (loop (cdr item))))
	    (else (error 'menu-bar "menu bar contains non menu" (car item))))))

  ;; will use
  (define (get-window hwnd)
    (let1 p (get-window-long-ptr hwnd GWLP_USERDATA)
      (if (null-pointer? p)
	  #f
	  (pointer->object p))))

  (define-syntax loword
    (syntax-rules ()
      ((_ word) (bitwise-and word #xFFFF))))
  (define-syntax hiword
    (syntax-rules ()
      ((_ word) (bitwise-and (ash word -16) #xFFFF))))

  (define (safe-move-window comp x y w h)
    (when (~ comp' context 'handle)
      (unless (or (= x CW_USEDEFAULT)
		  (= y CW_USEDEFAULT)
		  (= w CW_USEDEFAULT)
		  (= h CW_USEDEFAULT))
	(move-window (~ comp 'context 'handle) x y w h #t))))

  (define (default-window-proc hwnd imsg wparam lparam)
    (define (lookup-control w id)
      (let loop ((components (~ w 'components)))
	(cond ((null? components) #f)
	      ((= (~ (car components) 'context 'id) id) (car components))
	      (else (loop (cdr components))))))
    (cond ((= imsg WM_CREATE)
	   ;; save the lpCreateParams of CREATESTRUCT
	   (let1 w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)
	     (set-window-long-ptr hwnd GWLP_USERDATA w)
	     1))
	  ((= imsg WM_ERASEBKGND)
	   (let* ((rect (allocate-c-struct RECT))
		  (hdc (get-dc hwnd))
		  (hbrush (lookup-color (~ (get-window hwnd) 'background))))
	     (get-update-rect hwnd rect #f)
	     (fill-rect hdc rect hbrush)
	     1))
	  ((= imsg WM_CLOSE)
	   (let1 w (get-window hwnd)
	     (when (w (make <event> :control w :action 'close))
	       (window-close w))))
	  ((= imsg WM_DESTROY)
	   (let1 w (get-window hwnd)
	     (cond ((and (not (~ w 'owner))
			 (eq? w (*current-root-window*)))
		    (post-quit-message 0) 0)
		   (else 1))))
	  ((= imsg WM_SIZE)
	   (let ((rc (allocate-c-struct RECT)))
	     ;; make components which have adjust-size #t to the size of
	     ;; this component
	     (get-client-rect hwnd rc)
	     ;; get size
	     (let1 window (get-window hwnd)
	       (with-busy-component window
		 (let ((x (c-struct-ref rc RECT 'left))
		       (y (c-struct-ref rc RECT 'top))
		       (w (c-struct-ref rc RECT 'right))
		       (h (c-struct-ref rc RECT 'bottom)))
		   (set! (~ window 'width)   w)
		   (set! (~ window 'height)  h)
		   (for-each (lambda (component)
			       (let1 hwnd (~ component 'context 'handle)
				 (when hwnd
				   (when (~ component 'adjust-size)
				     (safe-position-set! component x y w h))
				   (safe-move-window component
						     (~ component 'x-point)
						     (~ component 'y-point)
						     (~ component 'width)
						     (~ component 'height))
				   ;; do we need this?
				   #;
				   (update-component component))))
			     (~ window 'components)))))
	     0))
	  ((= imsg WM_COMMAND) ; button or so?
	   ;; get loword (lower 16 bits)
	   (let* ((id (loword wparam))
		  (op (hiword wparam))
		  (w  (get-window hwnd))
		  (wd (or (~ w 'context 'control-map id)
			  (and (is-a? w <menu-bar-container>)
			       (~ w 'menu-bar)
			       (search-menu w (~ w 'menu-bar) id)))))
	     (when (is-a? wd <performable>)
	       (let1 action (lookup-action wd op)
		 (with-busy-component wd
		   (wd (make <event> :control wd :action action)))))))
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
		 (pointer->integer (lookup-color (~ control 'background))))
	       (def-window-proc hwnd imsg wparam lparam)))
	  ((= imsg WM_CTLCOLORBTN)
	   (def-window-proc hwnd imsg wparam lparam)
	   ;;(pointer->integer (get-stock-object BLACK_BRUSH))
	   )
	  (else
	   (def-window-proc hwnd imsg wparam lparam))))

  (define (compose-lparam low high) 
    (integer->pointer (bitwise-ior low (ash high 16))))

  (define-method resize-component ((c <component>) width height)
    (send-message (~ c 'context 'handle) WM_SIZE 0
		  (compose-lparam width height)))
  (define-method move-component ((c <component>) x y w h)
    (safe-move-window c x y w h))

  (define-method create-cursor ((type <symbol>))
    (define (lookup-cursor type)
      (case type
	;; todo add
	((size-ns) IDC_SIZENS)
	((size-we) IDC_SIZEWE)
	(else IDC_ARROW)))
    (make <cursor> :handle (load-cursor null-pointer (lookup-cursor type))))
  (define-method set-cursor! ((c <component>) (cursor <cursor>))
    (set-cursor (~ cursor 'handle)))
  (define-method capture-component ((c <component>))
    (set-capture (~ c 'context 'handle)))
  (define-method uncapture-component ((c <component>)) (release-capture))

  (define (panel-proc hwnd imsg wparam lparam)
    (let ((r (default-window-proc hwnd imsg wparam lparam))
	  (w (get-window hwnd)))
      (define (lookup-event imsg)
	(case/unquote imsg
	  ((WM_SIZE) 'resize)
	  ((WM_MOUSEMOVE)   'mouse-move)
	  ((WM_LBUTTONDOWN) 'mouse-down)
	  ((WM_LBUTTONUP)   'mouse-up)
	  ;; TODO
	  (else #f)))
      (define (create-data event wparam lparam)
	(define mk-control (list MK_CONTROL MK_LBUTTON MK_MBUTTON
				 MK_RBUTTON MK_SHIFT MK_XBUTTON1
				 MK_XBUTTON2))
	(define (create-mouse-states wparam)
	  (let loop ((controls mk-control) (r '()))
	    (if (null? controls)
		r
		(loop (cdr controls)
		      (case/unquote (bitwise-and wparam (car controls))
			((MK_CONTROL)  (cons 'control r))
			((MK_LBUTTON)  (cons 'left r))
			((MK_MBUTTON)  (cons 'middle r))
			((MK_RBUTTON)  (cons 'right r))
			((MK_SHIFT)    (cons 'shift r))
			;; ???
			((MK_XBUTTON1) (cons 'x-button1 r))
			((MK_XBUTTON2) (cons 'x-button2 r))
			(else r))))))
	(case event
	  ((resize mouse-move)
	   (let* ((word (pointer->integer lparam))
		  (w (loword word))
		  (h (hiword word)))
	     `((width . ,w) (height . ,h)
	       . ,(if (eq? event 'mouse-move)
		      `((button ,(create-mouse-states wparam)))
		      '()))))
	  (else '())))
      (define (create-event sp event wparam lparam)
	(make <event> :control sp :action event
	      :data (create-data event wparam lparam)))
      (when (and w (~ w 'context 'handle))
	(w (create-event w (lookup-event imsg) wparam lparam)))
      r))
  (define *window-proc*
    (c-callback void* (HWND unsigned-int WPARAM LPARAM) default-window-proc))
  (define *panel-proc*
    (c-callback void* (HWND unsigned-int WPARAM LPARAM) panel-proc))

  ;; register window-class
  (define *window-class-name* "turquoise-window")
  (define *panel-class-name*  "turquoise-panel")
  
  (define-syntax register-window-class
    (syntax-rules ()
      ((_ class-name proc)
       (let ((wnd (allocate-c-struct WNDCLASSEX)))
	 (let-syntax ((wnd-set! 
		       (syntax-rules ()
			 ((_ p v) (c-struct-set! wnd WNDCLASSEX 'p v)))))
	   (wnd-set! cbSize (size-of-c-struct WNDCLASSEX))
	   (wnd-set! lpfnWndProc proc)
	   (wnd-set! style (bitwise-ior CS_HREDRAW CS_VREDRAW))
	   (wnd-set! hInstance hinstance)
	   (wnd-set! hIcon (load-icon null-pointer IDI_APPLICATION))
	   (wnd-set! hCursor (load-cursor null-pointer IDC_ARROW))
	   (wnd-set! hbrBackground (get-stock-object WHITE_BRUSH))
	   (wnd-set! lpszClassName class-name)
	   (wnd-set! hIconSm null-pointer)
	   (when (zero? (register-class-ex wnd))
	     (error 'make-window "Failed to register WNDCLASS")))))))

  (register-window-class *window-class-name* *window-proc*)
  (register-window-class *panel-class-name*  *panel-proc*)
  
  (define-method make-window ((w <window>))
    (let1 context (~ w 'context)
      (add-class-name! context *window-class-name*)
      ;; TODO probably we need a slot for this in context so that
      ;; user can select the window style
      ;;(add-window-style! context WS_EX_PALETTEWINDOW)
      (add-window-style! context WS_EX_APPWINDOW)
      (let1 style (bitwise-ior WS_OVERLAPPEDWINDOW WS_CLIPCHILDREN
			       WS_CLIPSIBLINGS)
	(add-style! context style))
      w))

  (define-method sync-action ((t <text>)) 'update)
  (define-method sync-component ((comp <text>))
    (lambda (component action)
      (with-busy-component comp
	(let* ((hwnd (~ comp 'context 'handle))
	       (len  (pointer->integer
		      (send-message hwnd WM_GETTEXTLENGTH 0 null-pointer)))
	       (buf  (make-bytevector len)))
	  (send-message hwnd WM_GETTEXT (+ len 1) buf)
	  ;; UTF8?
	  (set! (~ comp 'value) (utf8->string buf))))))

  (define-method sync-action ((cb <check-box>)) 'click)
  (define-method sync-component ((comp <check-box>))
    (lambda (component action)
      (with-busy-component comp
	(let* ((hwnd (~ comp 'context 'handle))
	       (ret  (pointer->integer
		      (send-message hwnd BM_GETCHECK 0 null-pointer))))
	  (set! (~ comp 'checked)
		(cond ((= ret BST_CHECKED) #t)
		      ((= ret BST_UNCHECKED) #f)
		      ((= ret BST_INDETERMINATE) '())))))))

  (define-method sync-action ((lb <list-box>)) 'selection-change)
  (define-method sync-component ((comp <list-box>))
    (lambda (component action)
      (with-busy-component comp
	(let* ((hwnd (~ comp 'context 'handle))
	       (index (send-message hwnd LB_GETCURSEL 0 null-pointer))
	       (item  (send-message hwnd LB_GETITEMDATA 
				    (pointer->integer index) null-pointer)))
	  (set! (~ component 'selected) (pointer->object item))))))
  
  ;; initialise
  (define-method on-initialize ((comp <component>))
    (update-component comp))
  ;; updater around
  (define-method update-component :around ((comp <component>))
    (when (and (slot-bound? comp 'context) 
	       (~ comp 'context 'handle)
	       (not (~ comp 'busy)))
      (call-next-method)))

  ;; should we put this in :after or :around?
  ;; to make :after work
  (define-method update-component ((comp <component>)) #f)
  ;; updator after
  (define-method update-component :after ((comp <component>))
    ;; for all component properties such as point or size
    (if (~ comp 'adjust-size)
	;; if adjust-size is #t then the size is depending on the
	;; owner component
	(let1 owner (~ comp 'owner)
	  (when owner
	    (send-message (~ owner 'context 'handle) WM_SIZE 0
			  (compose-lparam (~ owner 'width) (~ owner 'height)))))
	(safe-move-window comp (~ comp 'x-point) (~ comp 'y-point)
			  (~ comp 'width) (~ comp 'height))))

  (define (%show comp)
    (let1 hwnd (~ comp 'context 'handle)
      (set! (~ comp 'visible) #t)
      (show-window hwnd SW_SHOW)
      (update-window hwnd)))

  (define (%init comp)
    (on-initialize comp)
    (when (is-a? comp <performable>)
      (let1 action (sync-action comp)
	(when action
	  (add! comp action (sync-component comp))))))

  (define-method show ((comp <component>))
    (let1 context (~ comp 'context)
      (if (~ context 'handle)
	  (%show comp)
	  (let* ((owner (~ comp 'owner))
		 (hwnd (%create-window
			comp 
			(if owner 
			    (~ owner 'context 'handle)
			    null-pointer))))
	    (set! (~ context 'handle) hwnd)
	    (%init comp)
	    (when (~ comp 'visible) (%show comp))))))

  ;; menu
  (define-method show ((item <menu-item>))
    (let ((context (~ item 'context))
	  (style   (~ item 'style))
	  (hmenu   (~ item 'owner 'context 'handle)))
      ;; TODO menu style
      (append-menu hmenu MF_STRING (~ item 'context 'id) (~ item 'name))
      (%init item)))

  (define-method show ((menu <menu>))
    (let ((context (~ menu 'context))
	  (items   (~ menu 'items))
	  (style   (~ menu 'style))
	  (root?   (~ menu 'root)))
      (let1 hmenu (if root? (create-menu) (create-popup-menu))
	;; first set the handle
	(set! (~ menu 'context 'handle) hmenu)
	;; show all items
	;; append-menu appends infront of the previous one
	;; to show the order of append, we need to reverse it
	(for-each show (reverse items))
	(if root?
	    (set-menu (~ menu 'owner 'context 'handle) hmenu)
	    ;; TODO menu style
	    (append-menu (~ menu 'owner 'context 'handle)
			 (bitwise-ior MF_POPUP MF_STRING)
			 (pointer->integer hmenu)
			 (~ menu 'name))))))
  
  (define-method show ((menu-holfer <menu-bar-container>))
    (call-next-method)
    (when (~ menu-holfer 'menu-bar)
      (show (~ menu-holfer 'menu-bar))
      (update-window (~ menu-holfer 'menu-bar 'context 'handle))))

  (define-method show ((container <container>))
    (call-next-method)
    (for-each show (~ container 'components))
    ;; for panel ...
    (update-component container))

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
      (unless (~ context 'id) (set! (~ context 'id) (generate-id)))
      (let1 id (~ comp 'context 'id)
	(set! (~ container 'context 'control-map id) comp)))
    (push! (~ container 'components) comp))

  (define-method update-component ((w <window>))
    (set-window-text (~ w 'context 'handle) (~ w 'name)))

  (define-method add! ((w <menu-bar-container>) (menu <menu>))
    (call-next-method)
    (set! (~ menu 'owner) w)
    (set! (~ menu 'root) #t)
    (set! (~ w 'menu-bar) menu)
    ;; remove menu from components ...
    (let1 components (~ w 'components)
      (set! (~ w 'components) (remove menu components))))

  (define-method initialize ((p <panel>) initargs)
    (call-next-method)
    (let1 context (~ p 'context)
      (add-class-name! context *panel-class-name*)
      (let1 style (bitwise-ior WS_CLIPCHILDREN WS_CLIPSIBLINGS)
	(add-style! context style))))

  (define-method update-component ((p <panel>))
    ;; for adjust size, we need to send WM_SIZE to owner window
    (let1 owner (~ p 'owner)
      (when (and owner (~ owner 'context 'handle)
		 (not (~ owner 'busy)))
	(let ((width (~ owner 'width))
	      (height (~ owner 'height)))
	  (send-message (~ owner 'context 'handle) WM_SIZE 0
			(compose-lparam width height)))))
    ;; send WM_SIZE message to this panel for the compoenents
    (send-message (~ p 'context 'handle) WM_SIZE 0
		  (compose-lparam (~ p 'width) (~ p 'height))))

  ;;(define-platform-data splitter-moving?)
  ;;(define-platform-data splitter-position)
  ;;(define-platform-data splitter-cursor)
  #;
  (define-method initialize ((p <split-panel>) initargs)
    (call-next-method)
    ;; the background color will be border's color
    (set! (~ p 'background) (get-keyword :background initargs 'light-gray))
    (let1 context (~ p 'context)
      (update-splitter-moving?! context #f)
      ;; later
      (add-splitter-position! context 0)
      (add-splitter-cursor! context #f)))

  ;; FIXME
  #;
  (define-method show ((sp <split-panel>))
    (call-next-method)
    (let* ((panel (~ sp 'panel1))
	   (context (~ panel 'context)))
      (update-style! context (bitwise-ior WS_CHILD (context-style context)))
      (set! (~ panel 'owner) sp))
    (let* ((panel (~ sp 'panel2))
	   (context (~ panel 'context)))
      (update-style! context (bitwise-ior WS_CHILD (context-style context)))
      (set! (~ panel 'owner) sp))
    (show (~ sp 'panel1))
    (show (~ sp 'panel2))
    (let1 rc (allocate-c-struct RECT)
      (get-client-rect (~ sp 'context 'handle) rc)
      (send-message (~ sp 'context 'handle) WM_SIZE 0
		    (compose-lparam (c-struct-ref rc RECT 'right)
				    (c-struct-ref rc RECT 'bottom)))))

  #;
  (define-method update-component ((sp <split-panel>))
    (update-component (~ sp 'panel1))
    (update-component (~ sp 'panel2))
    (send-message (~ sp 'context 'handle) WM_SIZE 0
		  (compose-lparam (~ sp 'width) (~ sp 'height))))
  
  (define (lookup-button-style style)
    (case style
      ((push)  	   BS_PUSHBUTTON)
      ((check) 	   BS_AUTOCHECKBOX)
      ((radio) 	   BS_AUTORADIOBUTTON)
      ((tri-state) BS_AUTO3STATE)
      ((push-box)  BS_PUSHBOX)
      (else        BS_PUSHBUTTON)))

  (define (context-style context) (or (lookup-style context) 0))

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

  ;; menu
  (define-method initialize ((menu <menu>) initargs)
    (call-next-method))
  
  (define-method add! ((menu <menu>) (item <menu-component>))
    (set! (~ item 'owner) menu)
    (let1 context (~ item 'context)
      (unless (~ context 'id) (set! (~ context 'id) (generate-id))))
    (push! (~ menu 'items) item))

  ;; misc
  (define-method open-file-select ((select <file-select>))
    (open-file-select select null-pointer))
  (define-method open-file-select ((select <file-select>) (w <window>))
    (unless (~ w 'context 'handle)
      (error 'file-select "owner window is not shown yet" w))
    (open-file-select select  (~ w 'context 'handle)))
  (define-method open-file-select ((select <file-select>) hwnd)
    (define (create-filter args)
      (call-with-bytevector-output-port
       (lambda (p)
	 (for-each (lambda (str)
		     (put-bytevector p (string->utf8 str))
		     (put-u8 p 0))
		   args)
	 (put-u8 p 0))))
    (define (get-flags select)
      (bitwise-ior (if (eq? (~ select 'type) 'open)
		       OFN_FILEMUSTEXIST
		       OFN_OVERWRITEPROMPT)
		   (if (~ select 'show-readonly)
		       0
		       OFN_HIDEREADONLY)))
    (let ((ofn (allocate-c-struct OPENFILENAME))
	  (name (make-bytevector 256)))
      (c-struct-set! ofn OPENFILENAME 
		     'lStructSize OPENFILENAME_SIZE_VERSION_400)
      (c-struct-set! ofn OPENFILENAME 'hwndOwner hwnd)
      (c-struct-set! ofn OPENFILENAME 
		     'lpstrFilter (create-filter (~ select 'filters)))
      (c-struct-set! ofn OPENFILENAME 'lpstrFile name)
      (c-struct-set! ofn OPENFILENAME 'lpstrFileTitle null-pointer)
      (c-struct-set! ofn OPENFILENAME 'nMaxFile 256)
      (c-struct-set! ofn OPENFILENAME 'nFilterIndex 1)
      (c-struct-set! ofn OPENFILENAME 'Flags (get-flags select))
      (c-struct-set! ofn OPENFILENAME 'lpstrDefExt(~ select 'extension))
      (c-struct-set! ofn OPENFILENAME 'lpstrTitle (~ select 'title))
      (if (eq? (~ select 'type) 'open)
	  (if (get-open-file-name ofn)
	      (utf8->string name)
	      #f)
	  (if (get-save-file-name ofn)
	      (utf8->string name)
	      #f))))

)