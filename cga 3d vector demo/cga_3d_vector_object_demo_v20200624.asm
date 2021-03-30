; nasm cga3d_v4v16.asm -o vt.com -f bin

VIDEO_SEGMENT	equ	0b800h 	; display memory segment for true CGA graphics modes

INPUT_STATUS_1	equ	03dah	; VGA status register
VSYNC_MASK	equ	08h	; vertical sync bit in status register 1
DE_MASK		equ	01h	; display enable bit in status register 1

MAP_MASK	equ	2		; SC map mask register
SC_INDEX		equ	3c4h	; SC index register
 
TOP_CC		equ	1
BOTTOM_CC	equ	2
LEFT_CC		equ	4
RIGHT_CC		equ	8
BEHIND_CC	equ	16
MIDDLE_X		equ	160
MIDDLE_Y		equ	100
TOP_EDGE	equ	MIDDLE_Y-100
BOTTOM_EDGE	equ	MIDDLE_Y+99
LEFT_EDGE	equ	MIDDLE_X-160
RIGHT_EDGE	equ	MIDDLE_X+159

KEY_1	EQU	0231h
KEY_2	EQU	0332h
KEY_3	EQU	0433h
KEY_4 	EQU	0534h
KEY_5 	EQU	0635h
KEY_6 	EQU	0736h
KEY_ESC	EQU 27

CPU 8086
bits 16
org 100h

section .text

start:
	call		BuildScreenTable

	mov	ax,04h 	; CGA 320 x 200 4 colors
	int		10h

;	mov	ax,0Bh 	; Pallette
;	mov	bh,1
;	mov	bl,0
;	mov	bx,00000h ; pallette 0 high
;	int	10h 
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front
	
		mov	al,[XAngle]
		add	al,1
		mov	[XAngle],al
		mov	al,[YAngle]
		sub	al,1
		mov	[YAngle],al
		mov	al,[ZAngle]
		add	al,1
		mov	[ZAngle],al
		call	BuildRotateMatrix
	
;		mov word	[XOffset],0
;		mov word	[YOffset],0
;		mov word	[ZOffset],250
		mov	si,ObjectPoints00 ; points
		mov	di,ObjectLines00 ; lines
		mov	cl,8 		; points
		mov	ch,12	; lines
		call 		DrawObject

;		mov word	[XOffset],100
;		mov word	[YOffset],100
;		mov word	[ZOffset],350
;		mov	si,ObjectPoints00 ; points
;		mov	di,ObjectLines00 ; lines
;		mov	cl,8 		; points
;		mov	ch,12	; lines
;		call 		DrawObject
	
		call		GetKey
	jnc		MainLoop

Done: ; Finished, clear key, reset screen mode and exit.
	mov	ax,3		; reset to text mode
	int		10h
	mov	ah,4ch	; exit to DOS
	int		21h

GetKey:
	mov	ah,11h
	int	16h         	; has a key been pressed
	jz	.key_none ; no
	mov	ah,10h      ; yes 
	int	16h         	; get it in AX (al= ascii, ah=scan code)
	cmp	al,KEY_ESC
	je	.key_esc
	cmp 	ax,KEY_1
	je 	.key_1
	cmp 	ax,KEY_2
	je 	.key_2
	cmp 	ax,KEY_3
	je 	.key_3
	cmp 	ax,KEY_4
	je 	.key_4
	cmp 	ax,KEY_5
	je 	.key_5
	cmp 	ax,KEY_6
	je 	.key_6
.key_none:
	clc
	ret
.key_esc:
	stc
	ret
.key_1:
	mov ax,[XOffset]
	sub ax,1
	mov [XOffset],ax
	clc
	ret
.key_2:
	mov ax,[XOffset]
	add ax,1
	mov [XOffset],ax
	clc
	ret
.key_3:
	mov ax,[YOffset]
	sub ax,1
	mov [YOffset],ax
	clc
	ret
.key_4:
	mov ax,[YOffset]
	add ax,1
	mov [YOffset],ax
	clc
	ret
.key_5:
	mov ax,[ZOffset]
	sub ax,1
	mov [ZOffset],ax
	clc
	ret
.key_6:
	mov ax,[ZOffset]
	add ax,1
	mov [ZOffset],ax
	clc
	ret
	
BuildScreenTable:
	lea	si,[BackBuffer]
	mov	bp,si ; even lines
	add	bp,8192 ; odd lines	
	lea	di,[BackBufferTable]
	mov	cl,100
.loop:
		mov	[di],si
		mov	[di+2],bp
		add	si,80
		add	bp,80
		add	di,4
		dec	cl
		jnz	.loop
	ret
	
CopyClearBackBuffer:
	mov	ax,VIDEO_SEGMENT ; ES points to CGA memory.
	mov	es,ax

	lea	si,[BackBuffer]	; point ds:si to backbuffer
	xor	di,di 		; point es:di to video segement
	mov	cx,4000
	rep	movsw		; copy first plane of words from ds:si to es:di

	lea	si,[BackBuffer+8192]	; point ds:si to backbuffer
	mov	di,8192
	mov	cx,4000
	rep	movsw		; copy second plane of words from ds:si to es:di
	
	mov	ax,ds		; es points to ds
	mov	es,ax

	mov	ax,00101101001011010b ;
	lea	di,[BackBuffer]		; point es:di at backbuffer
	mov	cx,4000
	rep	stosw			; clear first plane of words

	lea	di,[BackBuffer+8192]	; point es:di at backbuffer
	mov	cx,4000
	rep	stosw			; clear first plane of words

	ret

Plot00:
	add	bx,bx 			; start ver *2
	mov	di,[BackBufferTable+bx]	; lookup start ver
	mov	bx,cx			; start hor
	add	bx,bx			; start hor *2
	add	di,[HorPosTable+bx]	; add start hor
	mov	cl,[di]
	and	cl,[AndTable+bx]
	mov	[di],cl
	ret

Plot11:
	add	bx,bx 			; start ver *2
	mov	di,[BackBufferTable+bx]	; lookup start ver
	mov	bx,cx			; start hor
	add	bx,bx			; start hor *2
	add	di,[HorPosTable+bx]	; add start hor
	mov	cl,[di]
	or	cl,[OrTable+bx] 
	mov	[di],cl
	ret

Line11:
	cmp 	bp,ax 		; is vs (bx) le ve (ax) ?
	jnc	Line11vp 	; yes
Line11vn: 			; vs is gt ve
	cmp 	bx,dx		; is hs (dx) le he (cx) ?
	jnc 	LineTRBL11 	; yes
	jmp	LineTLBR11	; hs is gt he
Line11vp:			; vs is le ve
	cmp 	bx,dx 		; is hs (dx) le he (cx)?
	jnc 	LineBRTL11	; yes
	jmp LineBLTR11		; hs is gt heo

LineBRTL11:
	xchg	ax,bp
	xchg	dx,bx

LineTLBR11:
	sub	ax,bp 	; ax - dx = ex - sx 						#1
	sub	dx,bx	; dx - dy = ey - sy 						#2

	mov	di,dx
	add	di,di	; di - 2*dy							#5

	mov	si,ax
	add	si,si	; si - 2*dx							#6

	add	bx,bx
	add	bp,bp

	cmp	ax,dx
	jl	.hline 	; 								#3
	
.vline:
	mov	cx,di
	sub	cx,ax	; delta=2*dy -dx 					#4

.vloop:
		mov	es,di	;								#11
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	dl,[di]
		or	dl,[OrTable+bp] 
		mov	[di],dl
		mov	di,es

		add	bp,2		; ++sx							#12
		cmp	cx,0
		jl	.vp		; if p>0							#10
.vn:
		add	bx,2		; ++sy 							#9
		add	cx,di		;  p=p+2*dy-2*dx 					#7
		sub	cx,si
		dec	ax		; dy--
		jns	.vloop		; exit
		ret
.vp:					;								#13
	add	cx,di 		; p=p+2*dy 						#8
	dec	ax		; dy--
	jns	.vloop		; exit
	ret
	
.hline:
	mov	cx,si
	sub	cx,dx	; p=2*dx -dy						#14

.hloop:
		mov	es,di
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	al,[di]
		or	al,[OrTable+bp] 
		mov	[di],al
		mov	di,es

		add	bx,2		; ++sx							#17
		cmp	cx,0
		jl	.hp		; if p>0							#18
		jmp .hn
.hn:
		add	bp,2		; ++sx							#19
		add	cx,si	;  p=p+2*dx-2*dy;  					#15
		sub	cx,di
		dec	dx		; dy--
		jns	.hloop	; exit
		ret
.hp:					;								#20
		add	cx,si 	; p=p+2*dx						#16
		dec	dx		; dy--
		jns	.hloop	; exit
		ret

	
LineTRBL11:
	xchg	ax,bp
	xchg	dx,bx

LineBLTR11:
	mov	es,bp	; backup ex
	xchg	ax,bp	; swap x - ax=ex - bp=sx
	
	sub	ax,bp 	; ax - dx = ex - sx 						#1
	sub	dx,bx	; dx - dy = ey - sy 						#2

	mov	di,dx
	add	di,di	; di - 2*dy							#5

	mov	si,ax
	add	si,si	; si - 2*dx							#6

	add	bx,bx
	mov	bp,es	; restore ex
	add	bp,bp

	cmp	ax,dx
	jl	.hline 	; 								#3
	
.vline:
	mov	cx,di
	sub	cx,ax	; delta=2*dy -dx 					#4

.vloop:
		mov	es,di	;								#11
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	dl,[di]
		or	dl,[OrTable+bp] 
		mov	[di],dl
		mov	di,es

		sub	bp,2	; --sx							#12
		cmp	cx,0
		jl	.vp	; if p>0							#10
.vn:
		add	bx,2	; ++sy 							#9
		add	cx,di	;  p=p+2*dy-2*dx 					#7
		sub	cx,si
		dec	ax	; dy--
		jns	.vloop	; exit
		ret
.vp:					;								#13
		add	cx,di 	; p=p+2*dy 						#8
		dec	ax	; dy--
		jns	.vloop	; exit
		ret
	
.hline:
	mov	cx,si
	sub	cx,dx	; p=2*dx -dy						#14

.hloop:
		mov	es,di
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	al,[di]
		or	al,[OrTable+bp] 
		mov	[di],al
		mov	di,es

		add	bx,2		; ++sx							#17
		cmp	cx,0
		jl	.hp		; if p>0							#18
		jmp .hn
.hn:
		sub		bp,2		; --sx							#19
		add		cx,si	;  p=p+2*dx-2*dy;  					#15
		sub		cx,di
		dec		dx		; dy--
		jns		.hloop	; exit
		ret
.hp:					;								#20
		add		cx,si 	; p=p+2*dx						#16
		dec		dx		; dy--
		jns		.hloop	; exit
		ret
	
Line00:
	cmp	bp,ax 		; is vs (bx) le ve (ax) ?
	jnc	Line00vp 		; yes
Line00vn: 					; vs is gt ve
	cmp 	bx,dx		; is hs (dx) le he (cx) ?
	jnc 	LineTRBL00 	; yes
	jmp	LineTLBR00	; hs is gt he
Line00vp:					; vs is le ve
	cmp 	bx,dx 		; is hs (dx) le he (cx)?
	jnc 	LineBRTL00	; yes
	jmp 	LineBLTR00	; hs is gt heo
	
LineBRTL00:
	xchg	ax,bp
	xchg	dx,bx

LineTLBR00:
	sub	ax,bp 	; ax - dx = ex - sx 						#1
	sub	dx,bx	; dx - dy = ey - sy 						#2

	mov	di,dx
	add	di,di		; di - 2*dy							#5

	mov	si,ax
	add	si,si		; si - 2*dx							#6

	add	bx,bx
	add	bp,bp

	cmp	ax,dx
	jl	.hline 	; 								#3
	
.vline:
	mov	cx,di
	sub	cx,ax	; delta=2*dy -dx 					#4

.vloop:
		mov	es,di	;								#11
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	dl,[di]
		and	dl,[AndTable+bp] 
		mov	[di],dl
		mov	di,es

		add	bp,2	; ++sx							#12
		cmp	cx,0
		jl	.vp	; if p>0							#10
.vn:
		add	bx,2	; ++sy 							#9
		add	cx,di	;  p=p+2*dy-2*dx 					#7
		sub	cx,si
		dec	ax	; dy--
		jns	.vloop	; exit
		ret
.vp:				;								#13
		add	cx,di 	; p=p+2*dy 						#8
		dec	ax	; dy--
		jns	.vloop	; exit
		ret
	
.hline:
	mov	cx,si
	sub	cx,dx	; p=2*dx -dy						#14

.hloop:
		mov	es,di
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	al,[di]
		and	al,[AndTable+bp] 
		mov	[di],al
		mov	di,es

		add	bx,2	; ++sx							#17
		cmp	cx,0
		jl	.hp	; if p>0							#18
		jmp 	.hn
.hn:
		add	bp,2	; ++sx							#19
		add	cx,si	;  p=p+2*dx-2*dy;  					#15
		sub	cx,di
		dec	dx	; dy--
		jns	.hloop	; exit
		ret
.hp:					;								#20
		add	cx,si 	; p=p+2*dx						#16
		dec	dx	; dy--
		jns	.hloop	; exit
		ret

	
LineTRBL00:
	xchg	ax,bp
	xchg	dx,bx

LineBLTR00:
	mov	es,bp	; backup ex
	xchg	ax,bp	; swap x - ax=ex - bp=sx
	
	sub	ax,bp 	; ax - dx = ex - sx 						#1
	sub	dx,bx	; dx - dy = ey - sy 						#2

	mov	di,dx
	add	di,di	; di - 2*dy							#5

	mov	si,ax
	add	si,si	; si - 2*dx							#6

	add	bx,bx
	mov	bp,es	; restore ex
	add	bp,bp

	cmp	ax,dx
	jl	.hline 	; 								#3
	
.vline:
	mov	cx,di
	sub	cx,ax	; delta=2*dy -dx 					#4

.vloop:
		mov	es,di	;								#11
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	dl,[di]
		and	dl,[AndTable+bp] 
		mov	[di],dl
		mov	di,es

		sub	bp,2	; --sx							#12
		cmp	cx,0
		jl	.vp	; if p>0							#10
.vn:
		add	bx,2	; ++sy 							#9
		add	cx,di	;  p=p+2*dy-2*dx 					#7
		sub	cx,si
		dec	ax	; dy--
		jns	.vloop	; exit
		ret
.vp:				;								#13
		add	cx,di 	; p=p+2*dy 						#8
		dec	ax	; dy--
		jns	.vloop	; exit
		ret
	
.hline:
	mov	cx,si
	sub	cx,dx	; p=2*dx -dy						#14

.hloop:
		mov	es,di
		mov	di,[BackBufferTable+bx]	; lookup start ver
		add	di,[HorPosTable+bp]	; add start hor
		mov	al,[di]
		and	al,[AndTable+bp] 
		mov	[di],al
		mov	di,es

		add	bx,2	; ++sx							#17
		cmp	cx,0
		jl	.hp	; if p>0							#18
		jmp .hn
.hn:
		sub	bp,2	; --sx							#19
		add	cx,si	;  p=p+2*dx-2*dy;  					#15
		sub	cx,di
		dec	dx	; dy--
		jns	.hloop	; exit
		ret
.hp:						;								#20
		add	cx,si 	; p=p+2*dx						#16
		dec	dx	; dy--
		jns	.hloop	; exit
		ret

; classify3d
;  get z
;  if minus
;   classify eq 16
;  else
;   do perspective
;    classify eq 0
;    if x lt 32767-100
;     classify eq 1
;    if x gt 32767+100
;    classify eq 2
;   if y lt 32767-160
;    classify +=4
;   if y gt 32767+160
;    classify +=8
	
DrawObject:
	push	di ; object lines
	push	cx ; number of lines

	mov	di,ObjectPointsResults

	xor	ch,ch
	dec	cl
	add	cl,cl
	mov	di,cx	; point count
		
	mov	bx,255 ; high = or clip codes / low = and clip codes

; ax= spare
; dx= spare
; bp= x
; cx= y
; es= z
; bx = used
; si = used
; di = used
	
.rotate_point_loop:
		push	bx
		mov	cx,[ds:si]	; xpos
		mov	ax,[ds:si+2]	; ypos
		mov	bp,[ds:si+4]	; zpos
		
		call	Rotate

		mov	ax,cx
		mov	cx,bp
		mov	bp,bx
		
		add	ax,[YOffset]
		mov	[ObjectYRot+di],ax 		; yrot
		
		add	cx,[ZOffset]
		mov	[ObjectZRot+di],cx 	; zrot

		add	bp,[XOffset]
		mov	[ObjectXRot+di],bp 			; xrot

		test	cx,cx 		; test if zpos is -ve
		jns	.point_in_front
			mov	dl,BEHIND_CC
			jmp	.point_behind	
.point_in_front:
		call	Perspective
		mov	[ObjectYPos+di],ax 		; xpos
		mov	[ObjectXPos+di],bp 		; ypos
		call	ClassifyPoint
.point_behind:
		mov	[ObjectClipCode+di],dl 	; clip code

		pop	bx
		and	bl,dl	; object and clip code
		or	bh,dl	; object or clip code
		
		add	si,6				
		sub	di,2
		jns	.rotate_point_loop

	pop	cx 	; number of lines
	pop 	si 	; object lines

	cmp	bl,0				; if and_object_clip_code ne 0 all off one side so exit
	jnz	.off_screen
 
	cmp	bh,0				; if or_object_clip_code eq 0 all on screen so no clip needed
	jnz	.draw_clipped_object

.draw_whole_object:
.draw_whole_object_line_loop: ; todo speedup - decrement si instead of increment - then check for 0 - means that cx can be removed - cant do as si is object pointer
		push	cx
		push	si

		xor	bh,bh

		mov	bl,[ds:si]
		add	bl,bl	
		mov	ax,[ObjectYPos+bx] ; starty
		mov	dx,[ObjectXPos+bx] ; startx

		mov	bl,[ds:si+1]
		add	bl,bl
		mov	bp,[ObjectYPos+bx] ; endy
		mov	bx,[ObjectXPos+bx] ; endx
		
		call	Line11

		pop	si
		pop	cx
		add	si,2

		dec	ch
		jnz	.draw_whole_object_line_loop

.off_screen:		
	ret
	
.draw_clipped_object:	
.draw_clipped_object_line_loop:
		xor	bh,bh
		push	cx
		
		mov	bl,[ds:si]
		add	bl,bl
		mov	al,[ObjectClipCode+bx] 	; start clip code
		mov	bl,[ds:si+1]
		add	bl,bl
		mov	ah,[ObjectClipCode+bx] 	; end clip code

		mov	bl,al		; backup start clip code
		and	bl,ah	; both points are off 
		jnz	.line_off_screen	; then line is off screen

		or	al,ah 	; if any point is off
		jnz	.line_clip	; then line needs clipped
		
		mov	bl,[ds:si]
		add	bl,bl	
		mov	ax,[ObjectYPos+bx] ; starty
		mov	dx,[ObjectXPos+bx] ; startx

		mov	bl,[ds:si+1]
		add	bl,bl
		mov	bp,[ObjectYPos+bx] ; endy 
		mov	bx,[ObjectXPos+bx] ; endx

.draw_clipped_line:
		push	si
		call	Line00 ; Line11
		pop	si

.line_off_screen:
		pop	cx
		add	si,2
		
		dec	ch
		jnz	.draw_clipped_object_line_loop
	ret

.line_clip:	; al = point 1 clip ; ah = point 2 clip

; copy point details to 2 sets of coords temp coords zyx and temp perspective xy
; if either point is behind
;  do z clip on required point
;  do perspective
;  do rest of tests

	xor	bh,bh
	mov	bl,[ds:si]
	add	bl,bl	
	mov	dx,[ObjectXRot+bx]
	mov	[STX],dx
	mov	dx,[ObjectYRot+bx]
	mov	[STY],dx
	mov	dx,[ObjectZRot+bx]
	mov	[STZ],dx
	mov	dx,[ObjectXPos+bx]
	mov	[XST],dx
	mov	dx,[ObjectYPos+bx]	
	mov	[YST],dx	
	mov	al,[ObjectClipCode+bx]
	
	mov	bl,[ds:si+1]
	add	bl,bl
	mov	dx,[ObjectXRot+bx]
	mov	[ENX],dx
	mov	dx,[ObjectYRot+bx]
	mov	[ENY],dx
	mov	dx,[ObjectZRot+bx]
	mov	[ENZ],dx	
	mov	dx,[ObjectXPos+bx]
	mov	[XEN],dx
	mov	dx,[ObjectYPos+bx]
	mov	[YEN],dx
	mov	ah,[ObjectClipCode+bx]

	mov	[CCST],ax
	
.line_clip_loop:
	mov	ax,[CCST]
	mov	bl,al			; start cc
	and	al,ah			; and with end cc
	jnz	.line_off_screen		; both off same side

	or	bl,ah			; or with end cc
	jz	.clip_draw_test		; all on so draw line

; test if either point is off (al=al or ah)
.clip_behind_test:
	test	bl,BEHIND_CC
	jz	.clip_left_test
		call	LineClipBehind

		mov	ax,[CCST]
		mov	bl,al			; start cc
		and	al,ah			; and with end cc
		jnz	.line_off_screen		; both off same side

		or	bl,ah			; or with end cc
		jz	.clip_draw_test		; all on so draw line
;;		jmp	.line_clip_loop
	
.clip_left_test:
	test	bl,LEFT_CC
	jz	.clip_right_test
		call	LineClipLeft
		mov	ax,[CCST]
		mov	bl,al			; start cc
		and	al,ah			; and with end cc
		jnz	.line_off_screen		; both off same side

		or	bl,ah			; or with end cc
		jz	.clip_draw_test		; all on so draw line
;;		jmp	.line_clip_loop
		
.clip_right_test:
	test	bl,RIGHT_CC
	jz	.clip_top_test
		call	LineClipRight
		mov	ax,[CCST]
		mov	bl,al			; start cc
		and	al,ah			; and with end cc
		jnz	.line_off_screen		; both off same side

		or	bl,ah			; or with end cc
		jz	.clip_draw_test		; all on so draw line
;;		jmp	.line_clip_loop
		

.clip_top_test:
	test	bl,TOP_CC
	jz	.clip_bottom_test
		call	LineClipTop
		mov	ax,[CCST]
		mov	bl,al			; start cc
		and	al,ah			; and with end cc
		jnz	.line_off_screen		; both off same side

		or	bl,ah			; or with end cc
		jz	.clip_draw_test		; all on so draw line
;;		jmp	.line_clip_loop

.clip_bottom_test:
	test	bl,BOTTOM_CC
	jz	.line_clip_loop
		call	LineClipBottom
		mov	ax,[CCST]
		mov	bl,al			; start cc
		and	al,ah			; and with end cc
		jnz	.line_off_screen		; both off same side

;;		or	bl,ah			; or with end cc
;;		jz	.clip_draw_test		; all on so draw line

;;	jmp		.line_clip_loop

.clip_draw_test:
	mov	ax,[YST]
	mov	dx,[XST]
	mov	bp,[YEN]
	mov	bx,[XEN]
	jmp	.draw_clipped_line
	
LineClipBehind:
	test	ah,BEHIND_CC
	jz	.behind_sback_efront
.behind_sfront_eback:
	mov	ax,[STX]
	mov	cx,[STZ]
	mov	di,[ENZ]
	mov	bx,[ENX]
	call	ClipBehind
	push	di
	
	mov	ax,[STY]
	mov	cx,[STZ]
	mov	di,[ENZ]
	mov	bx,[ENY]
	call	ClipBehind

	mov	ax,di
	pop	bp

	xor	cx,cx
	call	Perspective
	mov	[YEN],ax 
	mov	[XEN],bp
	call	ClassifyPoint
	mov	[CCEN],dl
	ret

.behind_sback_efront:
	mov	ax,[ENX]
	mov	cx,[ENZ]
	mov	di,[STZ]
	mov	bx,[STX]
	call	ClipBehind
	push	di
	
	mov	ax,[ENY]
	mov	cx,[ENZ]
	mov	di,[STZ]
	mov	bx,[STY]
	call	ClipBehind

	mov	ax,di
	pop	bp

	xor	cx,cx
	call	Perspective
	mov	[YST],ax 
	mov	[XST],bp 
	call	ClassifyPoint
	mov	[CCST],dl
	ret
	
; get start xyz
; get end xyz
; if endcc eq behind_cc
;  zon eq startz  
;  zdiff eq startz+endz
; else
;  zon eq endz
;  zdiff eq endz+startz
; calc xdiff
; calc ydiff
; x = xdiff *zon
; x = x / zdiff
; x = x + startx
; y = ydiff *zon
; y = y / zdiff
; y = y + starty
; perspective
; classifypoint

LineClipTop:
	test	ah,TOP_CC
	jz	.top_p1on_p2off
.top_p1off_p2on:
	mov	ax,[YST]
	mov	cx,[XST]
	mov	di,[XEN]
	mov	bx,[YEN]
	call	ClipTop

	mov	[YEN],di
	mov	bx,TOP_EDGE
	mov	[XEN],bx

	mov	ax,[YEN]
	mov	bp,[XEN]
	call	ClassifyPoint
	mov	[CCEN],dl
	ret

.top_p1on_p2off:
	mov	ax,[YEN]
	mov	cx,[XEN]
	mov	di,[XST]
	mov	bx,[YST]
	call	ClipTop

	mov	[YST],di
	mov	bx,TOP_EDGE
	mov	[XST],bx

	mov	ax,[YST]
	mov	bp,[XST]
	call	ClassifyPoint
	mov	[CCST],dl
	ret
	
LineClipLeft:
	test	ah,LEFT_CC	; end clip code is off left?
	jz	.left_p1on_p2off
	
.left_p1off_p2on:
	mov	cx,[YST]
	mov	ax,[XST]
	mov	di,[XEN]
	mov	bx,[YEN]	
	call	ClipLeft 	; ax returned 

	mov	[XEN],bx
	mov	bx,LEFT_EDGE
	mov	[YEN],bx

	mov	ax,[YEN]
	mov	bp,[XEN]
	call	ClassifyPoint
	mov	[CCEN],dl
	ret
	
.left_p1on_p2off:
	mov	cx,[YEN]
	mov	ax,[XEN]
	mov	di,[XST]
	mov	bx,[YST]
	call	ClipLeft

	mov	[XST],bx
	mov	bx,LEFT_EDGE
	mov	[YST],bx

	mov	ax,[YST]
	mov	bp,[XST]
	call	ClassifyPoint
	mov	[CCST],dl
	ret

LineClipRight:
	test	ah,RIGHT_CC
	jz	.right_p1on_p2off
	
.right_p1off_p2on:
	mov	bp,[YST]
	mov	dx,[XST]
	mov	di,[XEN]
	mov	bx,[YEN]	
	call	ClipRight

	mov	[XEN],bp
	mov	bx,RIGHT_EDGE
	mov	[YEN],bx

	mov	ax,[YEN]
	mov	bp,[XEN]
	call	ClassifyPoint
	mov	[CCEN],dl		
	ret

.right_p1on_p2off:
	mov	bp,[YEN]
	mov	dx,[XEN]
	mov	di,[XST]
	mov	bx,[YST]	
	call	ClipRight

	mov	[XST],bp
	mov	bx,RIGHT_EDGE
	mov	[YST],bx

	mov	ax,[YST]
	mov	bp,[XST]
	call	ClassifyPoint
	mov	[CCST],dl

	ret
	
LineClipBottom:
	test	ah,BOTTOM_CC
	jz	.bottom_p1on_p2off
	
.bottom_p1off_p2on:
	mov	ax,[YST]
	mov	dx,[XST]
	mov	di,[XEN]
	mov	bx,[YEN]	
	call	ClipBottom

	mov	[YEN],cx
	mov	bx,BOTTOM_EDGE
	mov	[XEN],bx

	mov	ax,[YEN]
	mov	bp,[XEN]
	call	ClassifyPoint
	mov	[CCEN],dl		
	ret

.bottom_p1on_p2off:
	mov	ax,[YEN]
	mov	dx,[XEN]
	mov	di,[XST]
	mov	bx,[YST]	
	call	ClipBottom

	mov	[YST],cx
	mov	bx,BOTTOM_EDGE
	mov	[XST],bx

	mov	ax,[YST]
	mov	bp,[XST]
	call	ClassifyPoint
	mov	[CCST],dl
	ret
	
WaitVSync: ; Wait for the leading edge of vertical sync pulse.
	mov	dx,INPUT_STATUS_1
WaitNotVSyncLoop:
		in	al,dx
		and	al,VSYNC_MASK
		jnz	WaitNotVSyncLoop
WaitVSyncLoop:
		in	al,dx
		and	al,VSYNC_MASK
		jz	WaitVSyncLoop
	ret

ClipBehind:
	mov	bp,cx	; backup pon.z (onh)
	sub	cx,di 	; pon.z-poff.z (dh)

	mov	di,ax	; backup pon.x (onv)
	sub	ax,bx	; pon.x-poff.x (dv)

	imul	bp	; pon.x-poff.x (dx) * pon.z (onh) 
	idiv	cx	; 	/ pon.z-poff.z (dh) (pon.z+poff.z)

	sub	di,ax
	
	ret

	
; input
; ax = x on screen ; pon.x = +ve
; bx = x off screen ; poff.x = -ve
; dx = y on screen ; pon.y
; di = y off screen ; poff.y
; output
; ax = clippedy

ClipTop:
	mov	bp,cx	; backup pon.x (onh)
	sub	cx,di 	; pon.x-poff.x (dh)

	mov	di,ax	; backup pon.y (onv)
	sub	ax,bx	; pon.y-poff.y (dv)

	imul	bp	; pon.y-poff.y (dv) * pon.x (onh) 
	idiv	cx	; 	/ pon.x-poff.x (dh) (pon.x+poff.x)

	sub	di,ax
	
	ret

ClipLeft:
	mov	bp,cx	; backup pon.x (onh)
	sub	cx,bx 	; pon.x-poff.x (dh)

	mov	bx,ax	; backup pon.y (onv)
	sub	ax,di	; pon.y-poff.y (dv)

	imul	bp	; pon.y-poff.y (dv) * pon.x (onh) 
	idiv	cx	; 	/ pon.x-poff.x (dh) (pon.x+poff.x)

	sub	bx,ax
	
	ret

ClipBottom:
 	mov	bp,BOTTOM_EDGE
	sub	bp,dx	; rightedge - pon.x (onh)
	sub	di,dx	; poff.x - pon.x (dh)

	mov	cx,ax
	sub	ax,bx	; pon.y-poff.y (dv)

	imul	bp
	idiv	di
	
	sub	cx,ax
	ret

ClipRight:
 	mov	ax,RIGHT_EDGE
	sub	ax,bp	; rightedge - pon.x (onh)
	sub	bx,bp	; poff.x - pon.x (dh)
	
	mov	bp,dx
	sub	dx,di	; pon.y-poff.y (dv)

	imul	dx
	idiv	bx

	sub	bp,ax
	ret

; input
; ax = x on screen ; pon.x = +ve
; bx = x off screen ; poff.x = -ve
; dx = y on screen ; pon.y
; di = y off screen ; poff.y
; output
; ax = clippedy
	
ClipZ:
; do z clip
; do perspective
; do clip

; z on
; z off
; x on
; x off
; y on
; y off

BuildRotateMatrix:
	xor	bh,bh

	mov	bl,[XAngle]
	mov	al,[sintab+bx]
	cbw
	add	ax,ax
	mov	[SinX],ax
	mov	al,[costab+bx]
	cbw
	add	ax,ax
	mov	[CosX],ax
	
	mov	bl,[YAngle]
	mov	al,[sintab+bx]
	cbw
	add	ax,ax
	mov	[SinY],ax
	mov	al,[costab+bx]
	cbw
	add	ax,ax
	mov	[CosY],ax

	mov	bl,[ZAngle]
	mov	al,[sintab+bx]
	cbw
	add	ax,ax
	mov	[SinZ],ax
	mov	al,[costab+bx]
	cbw
	add	ax,ax
	mov	[CosZ],ax
	ret
	
Rotate:

;rotate around x-axis:
;	y'' = y'*cos(C) + z'*sin(C)
;	z'' = y'*sin(C) - z'*cos(C)
	
RotateX:
	mov	es,ax	; y
	imul	word [CosX] ; y*cos(x)
	mov	bl,ah
	mov	bh,dl

	mov	ax,bp	; z
	imul	word [SinX]	; z*sin(x)
	mov	al,ah
	mov	ah,dl
	add		bx,ax

	mov	ax,es	; y
	mov	es,bx
	imul	word [SinX]	 ; y*sin(x)
	mov	bl,ah
	mov	bh,dl

	mov	ax,bp	; z
	imul	word [CosX]	; z*cos(x)
	mov	al,ah
	mov	ah,dl

	sub	bx,ax
	mov	bp,bx

; rotate around y-axis:
;	x'' = x'*cos(B) + z*sin(B)
;	z'  = x'*sin(B) - z*cos(B)

RotateY:
	mov	ax,cx	; x
	imul	word [CosY]	 ; x*cos(y)
	mov	bl,ah
	mov	bh,dl

	mov	ax,bp	; z
	imul	word [SinY]	; z*sin(y)
	mov	al,ah
	mov	ah,dl
	add	bx,ax

	mov	ax,cx	; x
	mov	cx,bx
	imul	word [SinY]	; x*sin(y)
	mov	bl,ah
	mov	bh,dl

	mov	ax,bp	; z
	imul	word [CosY]	; z*cos(y)
	mov	al,ah
	mov	ah,dl

	sub	bx,ax
	mov	bp,bx

;rotate around z-axis:
;	x'  = x*cos(A) + y*sin(A)
;	y'  = x*sin(A) - y*cos(A)

RotateZ:
	mov	ax,cx	; x
	imul	word [CosZ] 	; x*cos(z)
	mov	bl,ah
	mov	bh,dl

	mov	ax,es	; y
	imul	word [SinZ]; y*sin(z)
	mov	al,ah
	mov	ah,dl
	add		bx,ax

	mov	ax,cx 	; x
	imul 	word [SinZ]	; x*sin(z)
	mov	cl,ah
	mov	ch,dl

	mov	ax,es	; y
	imul	word [CosZ]	; y*cos(z)
	mov	al,ah
	mov	ah,dl

	sub	cx,ax
	ret

ClassifyPoint:	; in = ax/bp	; out = dl
	xor	dl,dl	; clear clipcodes

	cmp	bp,TOP_EDGE
	jge	.right_test
		mov	dl,TOP_CC
		jmp	.hor_test_end
.right_test:
	cmp	bp,BOTTOM_EDGE
	jle	.hor_test_end
		mov	dl,BOTTOM_CC

.hor_test_end:
	cmp	ax,LEFT_EDGE
	jge	.bottom_test
		or	dl,LEFT_CC
		ret
.bottom_test:
	cmp	ax,RIGHT_EDGE
	jle	.ver_test_end
		or	dl,RIGHT_CC 			
.ver_test_end:
	ret

Perspective:	; in - bp = z / ax = x  dx = y
	inc	ch	 	; z +256
		
	cwd
	mov	dl,ah
	mov	ah,al		; dx:ax = ypos *256 = ypos *256	
	idiv	cx 		; ax = dx:ax = ypos*256 / zpos+256
	add	ax,MIDDLE_Y	; ypos
	
	xchg	ax,bp

	cwd
	mov	dl,ah
	mov	ah,al		; dx:ax = xpos *256 = xpos *256
	idiv	cx 		; ax = dx:ax = xpos*256 / zpos+256
	add	ax,MIDDLE_X	; xpos
	
	ret			; output bl=clip codes / ax=xpos / bp=ypos
	
; get sinx
; get cosx
; get siny
; get cosy
; get sinz
; get cosz

;	const float rs = sin(dir->x); // Set up the world-to-view rotation. Note: much of the work done in concatenating these matrices can be factored out, since it contributes nothing to the final result; multiply the three matrices together on paper to generate a minimum equation for each of the 9 final elements
;	const float rc = cos(dir->x);
;	const float ps = sin(dir->y);
;	const float pc = cos(dir->y);
;	const float ys = sin(dir->z);
;	const float yc = cos(dir->z);
;	const float mtemp2[3][3] ={ 
; { rc * yc, rs, rc * -ys }, 
; { (pc * -rs * yc) + (ps * ys), pc * rc, (pc * -rs * -ys) + (ps * yc) }, 
; { (-ps * -rs * yc) + (pc * ys), -ps * rc, (-ps * -rs * -ys) + (pc * yc) } };
;	*right = PointSet( mtemp2[0][0], mtemp2[0][1], mtemp2[0][2] ); // Break out the rotation matrix into vright, vup, and vpn. We could work directly with the matrix; breaking it out into three vectors is just to make things clearer
;	*up = PointSet( mtemp2[1][0],mtemp2[1][1],mtemp2[1][2] );
;	*in = PointSet( mtemp2[2][0], mtemp2[2][1], mtemp2[2][2] );

;  //  mx1= sin(gam)*sin(bet)*sin(alp) + cos(gam)*cos(alp)
;    //  my1= -cos(bet)*sin(alp)
;    //  mz1= sin(gam)*cos(alp) - cos(gam)*sin(bet)*sin(alp)

;    //  mx2= cos(gam)*sin(alp) - sin(gam)*sin(bet)*cos(alp)
;    //  my2= cos(bet)*cos(alp)
;    //  mz2= -cos(gam)*sin(bet)*cos(alp) - sin(gam)*sin(alp)

;    //  mx3 = cos(bet)*sin(gam)
;    //  my3 = sin(bet)
;    //  mz3 = cos(gam)*cos(bet)

; x"'=x*mx1 + y*my1 + z*mz1,  y"'=x*mx2 + y*my2 + z*mz2 and  z"'=x*mx3 + y*my3 + z*mz3

; clipping
; left 
; pon = point onscreen
; poff = point offscreen

; pon.x = +ve
; poff.x = -ve
; clipline = 0
; mulx = pon.x
; divx = pon.x-poff.x
; dy = pon.y - poff.y
; cy = poff.y + (dy * mulx / divx)
; cx = 0

	
;WaitDisplayEnable: ; Wait for display enable to happen (pixels to be scanned to the screen, indicating we're in the middle of displaying a frame).
;	mov	dx,INPUT_STATUS_1
;WaitDELoop:
;	in		al,dx
;	and		al,DE_MASK
;	jnz		WaitDELoop
;	ret
	
section .data align=8 ; 16 ; 8 ; 16

; BackBuffer: 		resw 16384	; 2 screen buffers
; BackBufferTable: 	resw 202 	; 200 screen lines as words
OrTable:	
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3
	dw	192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3,192,48,12,3

AndTable:	
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	dw	255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3,255-192,255-48,255-12,255-3
	
HorPosTable:
	dw	0,0,0,0
	dw	1,1,1,1
	dw	2,2,2,2
	dw	3,3,3,3
	dw	4,4,4,4
	dw	5,5,5,5
	dw	6,6,6,6
	dw	7,7,7,7
	dw	8,8,8,8
	dw	9,9,9,9
	
	dw	10,10,10,10
	dw	11,11,11,11
	dw	12,12,12,12
	dw	13,13,13,13
	dw	14,14,14,14
	dw	15,15,15,15
	dw	16,16,16,16
	dw	17,17,17,17
	dw	18,18,18,18
	dw	19,19,19,19
	
	dw	20,20,20,20
	dw	21,21,21,21
	dw	22,22,22,22
	dw	23,23,23,23
	dw	24,24,24,24
	dw	25,25,25,25
	dw	26,26,26,26
	dw	27,27,27,27
	dw	28,28,28,28
	dw	29,29,29,29
	
	dw	30,30,30,30
	dw	31,31,31,31
	dw	32,32,32,32
	dw	33,33,33,33
	dw	34,34,34,34
	dw	35,35,35,35
	dw	36,36,36,36
	dw	37,37,37,37
	dw	38,38,38,38
	dw	39,39,39,39

	dw	40,40,40,40
	dw	41,41,41,41
	dw	42,42,42,42
	dw	43,43,43,43
	dw	44,44,44,44
	dw	45,45,45,45
	dw	46,46,46,46
	dw	47,47,47,47
	dw	48,48,48,48
	dw	49,49,49,49

	dw	50,50,50,50
	dw	51,51,51,51
	dw	52,52,52,52
	dw	53,53,53,53
	dw	54,54,54,54
	dw	55,55,55,55
	dw	56,56,56,56
	dw	57,57,57,57
	dw	58,58,58,58
	dw	59,59,59,59

	dw	60,60,60,60
	dw	61,61,61,61
	dw	62,62,62,62
	dw	63,63,63,63
	dw	64,64,64,64
	dw	65,65,65,65
	dw	66,66,66,66
	dw	67,67,67,67
	dw	68,68,68,68
	dw	69,69,69,69

	dw	70,70,70,70
	dw	71,71,71,71
	dw	72,72,72,72
	dw	73,73,73,73
	dw	74,74,74,74
	dw	75,75,75,75
	dw	76,76,76,76
	dw	77,77,77,77
	dw	78,78,78,78
	dw	79,79,79,79

ObjectPoints00:
;	dw	-10,-10,-10 		;+150
;	dw	-10,-10,+10		;+150
;	dw	-10,+10,-10		;+150
;	dw	-10,+10,+10		;+150
;	dw	+10,-10,-10 		;+150
;	dw	+10,-10,+10 		;+150
;	dw	+10,+10,-10		;+150
;	dw	+10,+10,+10		;+150

	dw	-300,-300,-300 		;+150
	dw	-300,-300,+300		;+150
	dw	-300,+300,-300		;+150
	dw	-300,+300,+300		;+150
	dw	+300,-300,-300 		;+150
	dw	+300,-300,+300 		;+150
	dw	+300,+300,-300		;+150
	dw	+300,+300,+300		;+150

	dw	-3000,-4000,-5000 ;+150
	dw	-3000,-4000,+5000 ;+150
	dw	-3000,+4000,-5000 ;+150
	dw	-3000,+4000,+5000 ;+150
	dw	+3000,-4000,-5000 ;+150
	dw	+3000,-4000,+5000 ;+150
	dw	+3000,+4000,-5000;+150
	dw	+3000,+4000,+5000 ;+150
	
ObjectLines00:
	db	0,1
	db	1,3
	db	3,2
	db	2,0
	db	4,5
	db	5,7
	db	7,6
	db	6,4
	db	0,4
	db	1,5
	db	2,6
	db	3,7

XOffset:	dw	0
YOffset:	dw	0
ZOffset:	dw	25 ; 2500	
	
%include  'sincos.inc'

section .bss 	; put uninitialized data here
BackBuffer: 		resw 16384	; 2 screen buffers
BackBufferTable: 	resw 202 	; 200 screen lines as words

ObjectPointsResults:
ObjectYPos:	resw	128 ; 128 words - xpos
ObjectXPos:	resw	128 ; 128 words - ypos
ObjectClipCode:	resw	128 ; 128 words - clip codes
ObjectXRot:	resw	128 ; 128 words - x rotated points
ObjectYRot:	resw	128 ; 128 words - y rotated points
ObjectZRot:	resw	128 ; 128 words - z rotated points

SinX:		resw	1
CosX:	resw	1
SinY:		resw	1
CosY:	resw	1
SinZ:		resw	1
CosZ:	resw	1

XST		resw	1
YST		resw	1
XEN		resw	1
YEN		resw	1
CCST	resb	1
CCEN	resb	1

XAngle:	resb	1
YAngle:	resb	1
ZAngle:	resb	1

STX:		resw	1
STY:		resw	1
STZ:		resw	1
ENX:		resw	1
ENY:		resw	1
ENZ:		resw	1



; pon.x = +ve
; poff.x = -ve
; clipline = 0
; mulx = pon.x
; divx = pon.x-poff.x
; dy = pon.y - poff.y
; cy = poff.y + (dy * mulx / divx)
; cx = 0

;CONSIDERATION #1:  Division

;Divisions are very slow.  But why divide when you could multiply?
;You know that the maximum height of a plottable line is the height of the
;screen.  As you are dividing 'dx' by 'dy' and getting a fixed-point result,
;the divisor is only ever within the range 1 to (screen_height - 1) inclusive
;(note that if the line is Y-major then this will be dy/dx hence the range
;will be 1 to (screen_width - 1) inclusive).  So we can create a lookup-table
;of size (MAX(screen_width, screen_height)) of fixed-point reciprocals where:

;    div_table[index] = (1 << 16) / index;

;This step is performed at initialization-time.  Then to perform, say, dx/dy in
;fixed-point we would later use:

;    m = dx * div_table[dy];

;This can typically be performed up to 50 times as fast as the previous division
;method on some processors.

;One other important point to note is that 16 bits may no longer be accurate
;enough for large screensizes as 1/table_size can lose accuracy due to underflow
;very quickly.  More bits can then be used to represent the reciprocals, and
;these shifted back to the 16.16-format fixed-point after the multiply as
;follows:

;    div_table[index] = (1 << 23) / index;

;(at initialization-time), then

;    m = (dx * div_table[dy]) >> (22 - 16)

;(when we require a division).  This forms the table in .22-format fixed-point,
;leaving the top 10 bits clear for the sign-bit plus the multiplication by dx
;(which may now be represented in 9 bits max, or has a maximum magnitude of 511).


; Calculates a matrix. Tha calculated matrix can then be used to rotate
; verteces very fast, since all the verteces can be rotated with the
; same matrix (as long as the rotation-angles stay the same).

; This routine requires the rotation angles to be given as memory-
; variables rotX/Y/Z. The angles are WORDs and between 0 and 65535.


; PROC calculateMatrix

;        push    di                      ; Save destination ptr

;        mov     ax,[rotX]

;        call    getSinCos               ; Get sin(rotX) & cos(rotX)
;        mov     esi,ecx
;        mov     [cos_x],edx

;        mov     ax,[rotY]

;        call    getSinCos               ; Get sin(rotY) & cos(rotY)
;        push    ecx                     ; Save sin(rotY)
;        mov     ebp,edx

;        mov     ax,[rotZ]

;        call    getSinCos               ; Get sin(rotZ) & cos(rotZ)
;        mov     [sin_z],ecx

;        pop     ebx                     ; Restore sin(rotY)
;        mov     cl,14                   ; Fixed point decimals
;        mov     di,OFFSET sinz_cosx     ; Destination offset

;        mov     eax,[sin_z]             ; sin_z * cos_x
;        imul    eax,[cos_x]
;        sar     eax,cl
;        stosd
	
;        mov     eax,edx                 ; cos_z * sin_x
;        imul    eax,esi
;        sar     eax,cl
;        stosd
	
;        mov     eax,edx                 ; cos_z * cos_x
;        imul    eax,[cos_x]
;        sar     eax,cl
;        stosd
	
;        mov     eax,[sin_z]             ; sin_z * sin_x
;        imul    eax,esi
;        sar     eax,cl
;        stosd

;        pop     di                      ; Restore destination ptr

;        mov     eax,edx                 ; m0 = cos_z * cos_y
;        imul    eax,ebp
;        sar     eax,cl
;        stosd

;        mov     eax,[sin_z]             ; m1 = -sin_z * cos_y
;        imul    eax,ebp
;        sar     eax,cl
;        neg     eax
;        stosd

;        mov     eax,ebx                 ; m2 = sin(rotY)
;        stosd

;        mov     eax,[cosz_sinx]         ; m3 = sin_z * cos_x +
;        imul    eax,ebx                 ;      cos_z * sin_x * sin_y
;        sar     eax,cl
;        add     eax,[sinz_cosx]
;        stosd

;        mov     eax,[sinz_sinx]         ; m4 = cos_z * cos_x -
;        imul    eax,ebx                 ;      sin_z * sin_x * sin_y
;        sar     eax,cl
;        neg     eax
;        add     eax,[cosz_cosx]
;        stosd
	
;        mov     eax,ebp                 ; m5 = -cos_y * sin_x
;        imul    eax,esi
;        sar     eax,cl
;        neg     eax
;        stosd

;        mov     eax,[cosz_cosx]         ; m6 = sin_z * sin_x -
;        imul    eax,ebx                 ;      cos_z * cos_x * sin_y
;        sar     eax,cl
;        neg     eax
;        add     eax,[sinz_sinx]
;        stosd

;        mov     eax,[sinz_cosx]         ; m7 = cos_z * sin_x +
;        imul    eax,ebx                 ;      sin_z * cos_x * sin_y
;        sar     eax,cl
;        add     eax,[cosz_sinx]
;        stosd

;        mov     eax,ebp                 ; m8 = cos_y * cos_x
;        imul    eax,[cos_x]
;        sar     eax,cl
;        stosd

;        ret

;ENDP



; 北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北?
;
;  Rotate one vertex (no projection)
;
;  Input:
;      AX - Pointer to the matrix
;      SI - Pointer to 3 WORDs (x, y, z)
;      DI - Pointer to 3 WORDs (destination x, y, z)
;
; 北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北?

; This subroutine rotates one vertex. The matrix must be calculated first
; using the correct rotX/Y/Z -values. Before calling this, SI must
; point to three words (x/y/z) that will be used as the source coordinates.
; DI must point to destination.

; This routine will NOT project the coordinates to the screen.


; PROC rotateVertex

;        push    di                      ; Save destination ptr

;        movsx   ebx,[WORD si]           ; Move x to ebx
;        movsx   ecx,[WORD si+2]         ; Move y to ecx
;        movsx   edx,[WORD si+4]         ; Move z to edx

;        mov     si,ax                   ; Source offset of the matrix
;        mov     di,OFFSET newX          ; Temporary destination ptr
;        mov     [BYTE count],3          ; Loopcounter


; Rotate all three coordinates (x/y/z) in a loop ----------------------------

; x' = m0 * x + m1 * y + m2 * z
; y' = m3 * x + m4 * y + m5 * z
; z' = m6 * x + m7 * y + m8 * z

;@@rotate_loop:
;        lodsd                           ; Load next value from the matrix
;        imul    eax,ebx                 ; Multiply with x
;        mov     ebp,eax

;        lodsd                           ; Load next value from the matrix
;        imul    eax,ecx                 ; Multiply with y
;        add     ebp,eax

;        lodsd                           ; Load next value from the matrix
;        imul    eax,edx                 ; Multiply with z
;        add     eax,ebp

;        stosd                           ; Store rotated coordinate

;        dec     [BYTE count]            ; Loop
;        jnz     @@rotate_loop


; Store rotated coordinates -------------------------------------------------

;        pop     di                      ; Restore destination ptr
;        mov     si,OFFSET newX          ; Source offset
;        mov     cx,3                    ; Load & store 3 coords (x/y/z)

;@@loop:
;        lodsd                           ; Load rotated coordinate
;        sar     eax,14                  ; Loose the decimals
;        stosw                           ; Store it

;        loop    @@loop                  ; Loop

;        ret

;ENDP
