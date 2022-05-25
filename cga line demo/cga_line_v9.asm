; nasm cga_line_v7.asm -o cgaline.com -f bin


; list of next line tables - even values = +8192 odd values = -8192 - put into 2 registers - swap registers after each next line

VIDEO_SEGMENT	equ	0b800h 	; display memory segment for true CGA graphics modes
SW				equ 80 		; screen width
SE				equ	0		; screen even start
SO				equ	8192	; screen odd start


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
	
	mov	ah, 4ah
	mov	bx, 1000h
	int	21h
	mov	ah, 48h
	mov	bx, 1000h
	int	21h
	mov	[BackBufferSeg], ax
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		mov		bx,[PointList]
		mov		bp,[PointList+2]
		call	Plot00
		mov		bx,[PointList+4]
		mov		bp,[PointList+6]
		call	Plot11

;	mov		cx,0 ; si,0
;	mov		dx,0 ; bp,0
;	mov		si,[PointList+6] ; cx,[PointList+6]
;	mov		bp,[PointList+4] ; dx,[PointList+4]
	mov		si,100				; vs
	mov		dx,[PointList+4]	; ve
	mov		bp,160				; hs
	mov		cx,[PointList+6]	; he
		call	Line8 	; LineHtest8
	
;;		mov	al,[XAngle]
;;		add	al,1
;;		mov	[XAngle],al
;;		mov	al,[YAngle]
;;		sub	al,1
;;		mov	[YAngle],al
;;		mov	al,[ZAngle]
;;		add	al,1
;;		mov	[ZAngle],al
;;		call	BuildRotateMatrix
	
;		mov word	[XOffset],0
;		mov word	[YOffset],0
;		mov word	[ZOffset],250
;;		mov	si,ObjectPoints00 ; points
;;		mov	di,ObjectLines00 ; lines
;;		mov	cl,8 		; points
;;		mov	ch,12	; lines
;;		call 		DrawObject

;		mov word	[XOffset],100
;		mov word	[YOffset],100
;		mov word	[ZOffset],350
;		mov	si,ObjectPoints00 ; points
;		mov	di,ObjectLines00 ; lines
;		mov	cl,8 		; points
;		mov	ch,12	; lines
;		call 		DrawObject
	
		mov		cx,2
		call	Move
	
		call	GetKey
	jnc		MainLoop

Done: ; Finished, clear key, reset screen mode and exit.
	mov	ax,3		; reset to text mode
	int		10h
	mov	ah,4ch	; exit to DOS
	int		21h

;	cmp.w d2,d7 	; is vs le ve?
;	bcc.s .vp 	; yes
;.vn: 			; vs is gt ve
;	cmp.w d1,d0	; is hs le he?
;	bcc line_np 	; yes
;	bra.s line_nn	; hs is gt he
;.vp:			; vs is le ve
;	bne.s	.vp2		; 
;		cmp.w 	d1,d0	; 
;		bne.s	.vp3	; 
;		rts		; 
;		
;.vp2
;	cmp.w d1,d0 	; is hs le he?
;.vp3
;	bcc.s line_pp 	; yes
;	bra line_pn	; hs is gt heo

;line_nn:

Line8:
	cmp		dx,si 	; vs le ve?
	jnc		.noswap	; yes
			xchg	si,dx	; swap vs,ve
			xchg	bp,cx	; swap hs,he
.noswap:
	jne		.vdiff			; v not same
		cmp		cx,bp		; v is same 
		je 		.quit		; h is same

.vdiff:
	sub		dx,si	; ver diff = ve-vs

	sub		cx,bp	; hs le he? - replace with sub below? todo
	jnc		.hforward	; yes

.hbackward: ; decrement line across everytime and line down every few times
 	neg 	cx
 
	cmp		cx,dx
	jc		.vbackward

	div 	cx		; dx:ax *bx = ax*65536 *bx
	add		si,si	; start ver *2
	add		bp,bp	; start hor *2
	mov		di,[BackBufferTable+si]	; lookup start ver
	
.hbloop:
	add 	al,ah	; fraction to current
	jnc		.hbplot	;
		add		si,2
		mov		di,[BackBufferTable+si]	; lookup start ver		
.hbplot:
		mov		bx,[HorPosTable+bp]
		mov		dl,[es:di+bx]
		mov		dh,[AndTable+bp]
		and		dl,dh
		mov		[es:di+bx],dl

		sub		bp,2		; next hor
		dec		cx			; for hor slope goes from 000 to 319
		jnz		.hbloop
.quit:	ret

.vbackward: ; decrement line down everytime and line across every few times
	xchg 	dx,cx
	div 	cx		; dx:ax *bx = ax*65536 *bx

	add		si,si	; start ver *2
	add		bp,bp	; start hor *2
	
	mov		dh,[AndTable+bp]
	mov		bx,[HorPosTable+bp]
	
.vbloop:
	add 	al,ah	; fraction to current
	jnc		.vbplot	;
		sub		bp,2
		mov		dh,[AndTable+bp]
		mov		bx,[HorPosTable+bp]
		
.vbplot:
		mov		di,[BackBufferTable+si]	; lookup start ver
		mov		dl,[es:di+bx]
		and		dl,dh
		mov		[es:di+bx],dl

		add		si,2		; next ver
		dec		cl			; for ver slope goes from 000 to 199
		jnz		.vbloop
	ret

.hforward: ; increment line across everytime and line down every few times

	cmp		cx,dx
	jc		.vforward

	div 	cx		; dx:ax *bx = ax*65536 *bx
	add		si,si	; start ver *2
	add		bp,bp	; start hor *2
	mov		di,[BackBufferTable+si]	; lookup start ver
	
.hfloop:
	add 	al,ah	; fraction to current
	jnc		.hfplot	;
		add		si,2
		mov		di,[BackBufferTable+si]	; lookup start ver		
.hfplot:
		mov		bx,[HorPosTable+bp]
		mov		dl,[es:di+bx]
		mov		dh,[AndTable+bp]
		and		dl,dh
		mov		[es:di+bx],dl

		add		bp,2		; next hor
		dec		cx			; for hor slope goes from 000 to 319
		jnz		.hfloop
	ret

.vforward: ; increment line down everytime and line across every few times
	xchg 	dx,cx
	div 	cx		; dx:ax *bx = ax*65536 *bx

	add		si,si	; start ver *2
	add		bp,bp	; start hor *2
	
	mov		dh,[AndTable+bp]
	mov		bx,[HorPosTable+bp]
	
.vfloop:
	add 	al,ah	; fraction to current
	jnc		.vfplot	;
		add		bp,2
		mov		dh,[AndTable+bp]
		mov		bx,[HorPosTable+bp]
		
.vfplot:
		mov		di,[BackBufferTable+si]	; lookup start ver
		mov		dl,[es:di+bx]
		and		dl,dh
		mov		[es:di+bx],dl

		add		si,2		; next ver
		dec		cl			; for ver slope goes from 000 to 199
		jnz		.vfloop
	ret

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
	xor		si,si	; even lines
	mov		bp,8192 ; odd lines
	lea		di,[BackBufferTable]
	mov		cl,100
.loop:
		mov		[di],si
		mov		[di+2],bp
		add		si,80
		add		bp,80
		add		di,4
		dec		cl
		jnz		.loop
	ret
	
CopyClearBackBuffer:
	cld

	mov		bp,ds			; backup ds

	mov		dx,[BackBufferSeg]
	mov		ds,dx

	mov		ax,VIDEO_SEGMENT
	mov		es,ax

	xor		si,si		; DS:SI points to even back buffer
	mov		di,si		; ES:DI points to CGA memory.

	mov		cx,4000
	rep	movsw			; copy from ds:si to es:di

	mov		si,8192		; DS:SI points to odd back buffer
	mov		di,si		; ES:DI points to CGA memory.

	mov		cx,4000
	rep	movsw			; copy from ds:si to es:di

	mov		ax,ds		; es points to ds
	mov		es,ax

	mov		ax,00101101001011010b ;
	
	xor		di,di		; DS:SI points to even back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		di,8192		; DS:SI points to odd back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		ds,bp			; restore ds

	ret

Plot00:
	add		bx,bx 			; start ver *2
	mov		di,[BackBufferTable+bx]	; lookup start ver
	add		bp,bp			; start hor *2
	add		di,[HorPosTable+bp]	; add start hor
	mov		cl,[es:di]
	and		cl,[AndTable+bp]
	mov		[es:di],cl
	ret

Plot11:
	add		bx,bx 			; start ver *2
	mov		di,[BackBufferTable+bx]	; lookup start ver
	add		bp,bp			; start hor *2
	add		di,[HorPosTable+bp]	; add start hor
	mov		cl,[es:di]
	or		cl,[OrTable+bp] 
	mov		[es:di],cl
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
	
Move:	lea 	si,[PointList]
		lea		di,[DirList]
 
.loop	mov		ax,[si]
		mov		bx,[si+2]
		mov		dx,[di]
		mov		bp,[di+2]
		add		ax,dx
		add		bx,bp
		mov		[si],ax
		mov		[si+2],bx

		cmp 	ax,0
		jne 	.testxl
			neg 	dx
.testxl		cmp 	bx,0
		jne 	.testyl
			neg 	bp
.testyl		cmp 	ax,199
		jne 	.testxr
			neg 	dx
.testxr		cmp 	bx,319
		jne 	.testyr
			neg 	bp
.testyr		mov 	[di],dx
		mov		[di+2],bp
 
		add		si,4
		add		di,4
		dec		cx
		jne		.loop
	ret
	
section .data align=8 ; 16 ; 8 ; 16

BackBufferTable		dw	SE+(SW*000),SO+(SW*001),SE+(SW*002),SO+(SW*003),SE+(SW*004),SO+(SW*005),SE+(SW*006),SO+(SW*007),SE+(SW*008),SO+(SW*009)
					dw	SE+(SW*010),SO+(SW*011),SE+(SW*012),SO+(SW*013),SE+(SW*014),SO+(SW*015),SE+(SW*016),SO+(SW*017),SE+(SW*018),SO+(SW*019)
					dw	SE+(SW*020),SO+(SW*021),SE+(SW*022),SO+(SW*023),SE+(SW*024),SO+(SW*025),SE+(SW*026),SO+(SW*027),SE+(SW*028),SO+(SW*029)
					dw	SE+(SW*030),SO+(SW*031),SE+(SW*032),SO+(SW*033),SE+(SW*034),SO+(SW*035),SE+(SW*036),SO+(SW*037),SE+(SW*038),SO+(SW*039)
					dw	SE+(SW*040),SO+(SW*041),SE+(SW*042),SO+(SW*043),SE+(SW*044),SO+(SW*045),SE+(SW*046),SO+(SW*047),SE+(SW*048),SO+(SW*049)
					dw	SE+(SW*050),SO+(SW*051),SE+(SW*052),SO+(SW*053),SE+(SW*054),SO+(SW*055),SE+(SW*056),SO+(SW*057),SE+(SW*058),SO+(SW*059)
					dw	SE+(SW*060),SO+(SW*061),SE+(SW*062),SO+(SW*063),SE+(SW*064),SO+(SW*065),SE+(SW*066),SO+(SW*067),SE+(SW*068),SO+(SW*069)
					dw	SE+(SW*070),SO+(SW*071),SE+(SW*072),SO+(SW*073),SE+(SW*074),SO+(SW*075),SE+(SW*076),SO+(SW*077),SE+(SW*078),SO+(SW*079)
					dw	SE+(SW*080),SO+(SW*081),SE+(SW*082),SO+(SW*083),SE+(SW*084),SO+(SW*085),SE+(SW*086),SO+(SW*087),SE+(SW*088),SO+(SW*089)
					dw	SE+(SW*090),SO+(SW*091),SE+(SW*092),SO+(SW*093),SE+(SW*094),SO+(SW*095),SE+(SW*096),SO+(SW*097),SE+(SW*098),SO+(SW*099)
               		dw	SE+(SW*100),SO+(SW*101),SE+(SW*102),SO+(SW*103),SE+(SW*104),SO+(SW*105),SE+(SW*106),SO+(SW*107),SE+(SW*108),SO+(SW*109)
					dw	SE+(SW*110),SO+(SW*111),SE+(SW*112),SO+(SW*113),SE+(SW*114),SO+(SW*115),SE+(SW*116),SO+(SW*117),SE+(SW*118),SO+(SW*119)
					dw	SE+(SW*120),SO+(SW*121),SE+(SW*122),SO+(SW*123),SE+(SW*124),SO+(SW*125),SE+(SW*126),SO+(SW*127),SE+(SW*128),SO+(SW*129)
					dw	SE+(SW*130),SO+(SW*131),SE+(SW*132),SO+(SW*133),SE+(SW*134),SO+(SW*135),SE+(SW*136),SO+(SW*137),SE+(SW*138),SO+(SW*139)
					dw	SE+(SW*140),SO+(SW*141),SE+(SW*142),SO+(SW*143),SE+(SW*144),SO+(SW*145),SE+(SW*146),SO+(SW*147),SE+(SW*148),SO+(SW*149)
					dw	SE+(SW*150),SO+(SW*151),SE+(SW*152),SO+(SW*153),SE+(SW*154),SO+(SW*155),SE+(SW*156),SO+(SW*157),SE+(SW*158),SO+(SW*159)
					dw	SE+(SW*160),SO+(SW*161),SE+(SW*162),SO+(SW*163),SE+(SW*164),SO+(SW*165),SE+(SW*166),SO+(SW*167),SE+(SW*168),SO+(SW*169)
					dw	SE+(SW*170),SO+(SW*171),SE+(SW*172),SO+(SW*173),SE+(SW*174),SO+(SW*175),SE+(SW*176),SO+(SW*177),SE+(SW*178),SO+(SW*179)
					dw	SE+(SW*180),SO+(SW*181),SE+(SW*182),SO+(SW*183),SE+(SW*184),SO+(SW*185),SE+(SW*186),SO+(SW*187),SE+(SW*188),SO+(SW*189)
					dw	SE+(SW*190),SO+(SW*191),SE+(SW*192),SO+(SW*193),SE+(SW*194),SO+(SW*195),SE+(SW*196),SO+(SW*197),SE+(SW*198),SO+(SW*199)

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

PointList	dw 0b8h, 13ch, 22h, 0ebh
DirList		dw 1,-1,-1,1
 
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

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer
