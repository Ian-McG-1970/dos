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

KEY_ESC	EQU 1bh
KEY_Q	EQU 'q'
KEY_W	EQU 'w'
KEY_E	EQU 'e'
KEY_R	EQU 'r'

CPU 8086
bits 16
org 100h

section .text

start:
	call	BuildScreenTable
	mov	ax,05h ; 04h	; CGA 320 x 200 4 colors ; more 05 instead of mode 04
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

	xor ax,ax
	mov	[Exit],al
	mov	[XAngle],ax
	mov	[YAngle],ax	
	
;	mov	[Test0],ax
	
MainLoop:
		call	WaitVSync
		call	CopyClearBackBuffer
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		mov		bl,0
		mov		ax,[XAngle]
		call	Hex16
		mov		bl,1
		mov		ax,[YAngle]
		call	Hex16
		mov		bl,3
		mov		ax,[SinX]
		call	Hex16
		mov		bl,4
		mov		ax,[CosX]
		call	Hex16
		mov		bl,6
		mov		ax,[SinY]
		call	Hex16
		mov		bl,7
		mov		ax,[CosY]
		call	Hex16

;		mov		bl,9
;		mov		ax,[Test0]
;		call	Hex16
		mov		bl,10
		mov		ax,[Test1]
		call	Hex16
		mov		bl,11
		mov		ax,[Test2]
		call	Hex16
		mov		bl,12
		mov		ax,[Test3]
		call	Hex16
		mov		bl,13
		mov		ax,[Test4]
		call	Hex16
		mov		bl,15
		mov		ax,[Test5]
		call	Hex16
		mov		bl,16
		mov		ax,[Test6]
		call	Hex16
		mov		bl,17
		mov		ax,[Test7]
		call	Hex16
		mov		bl,18
		mov		ax,[Test8]
		call	Hex16

;		mov		bl,25
;		mov		ax,[Test9]
;		call	Hex16
		
;		mov		bl,27
;		mov		ax,[Test10]
;		call	Hex16

;		mov		bl,29
;		mov		ax,[BackBufferSeg]
;		call	Hex16

;		mov		bl,30
;		mov		ax,[Test11]
;		call	Hex16

;		call	BuildRotateMatrixV2

		call	Horizon
		call	GetKey
		
	mov		al,[Exit]
	or	al,al
	je		MainLoop

Done:	mov		ax,3	; reset to text mode
		int		10h
		mov		ah,4ch	; exit to DOS
		int		21h

BuildScreenTable:	lea		di,[BackBufferTable]
					xor		bp,bp	; even lines
					mov		cx,100
.loop:					mov		[di],bp
						add		bp,8192
						mov		[di+2],bp
						sub		bp,8192-80
						add		di,4
						loop	.loop
					ret

Horizon:
		mov		bp,[XAngle]	; roll
		call	SinCosV2
		mov bx,bp
;		mov		[SinX],bx
;		mov		[CosX],ax

;		xchg ax,bp

; sar ax,1
; sar bx,1
		
		mov		cx,ax	; CosX
		mov		ax,160
		imul	bx			; *SinX - could be replaced with (bp*128) + (bp*32)
		idiv	cx
		mov		[HorEdge],ax

		mov		bx,[YAngle]	; pitch
		and		bx,1022 ; can be optimised to and 1111111110 = 511*2
;	mov		[Test1],bx
		mov		bx,[HorizonTable+bx]

		test	bp,bp
		js		.slpe_n

.slpe_p		mov		bp,bx
			add		bx,ax
			sub		bp,ax

			xor		dx,dx
			mov		cx,RIGHT_EDGE

			jmp		.slpe_c

.slpe_n		mov		bp,bx
			add		bp,ax
			sub		bx,ax

			xor		cx,cx
			mov		dx,RIGHT_EDGE
		
.slpe_c	mov		[XST],dx
		mov		[YST],bp
		mov		[XEN],cx
		mov		[YEN],bx

	mov		[Test1],dx
	mov		[Test2],bp
	mov		[Test3],cx
	mov		[Test4],bx

		test	bx,bx	; bottom if off top
		js		.exit

		cmp		bp,BOTTOM_EDGE	; top if off bottom
		jg		.exit

		test	bp,bp		; is top off top
		jns		.clpt_c		; no
			mov		ax,cx
			sub		ax,dx	;	ax=(endx-sttx) = (cx-dx)
			mov		si,bx
			sub		si,bp	;	si=(endy-stty) = (bx-bp)

			mov		di,dx	; backup sttx

			imul	bp		; 	ax = ( (ex-sx) *sy)
			idiv	si		; 	ax = ( ( (ex-sx) *sy ) / (ey-sy) )
			sub		di,ax	;	sx = sx - ( ( (ex-sx) *sy ) / (ey-sy) );
			mov		[XST],di
			xor		bp,bp
			mov		[YST],bp

.clpt_c	cmp		bx,BOTTOM_EDGE
		jle		.clpb_c

			mov		ax,cx
			sub		ax,dx	;	ax=(endx-sttx) = (cx-dx)
			mov		si,bx
			sub		si,bp	;	si=(endy-stty) = (bx-bp)

			mov		di,bx	
			sub		di,BOTTOM_EDGE	; di = (endy-DIBHeight) = (bx-BOTTOM_EDGE)

			imul	di		; 	ax = ( (ex-sx) * (ey-DIBHeight) )
			idiv	si		; 	ax = ( ( (ex-sx) * (ey-DIBHeight) ) / (ey-sy) )
			sub		cx,ax	;	ex = ex - ( ( (ex-sx) * (ey-DIBHeight) ) / (ey-sy) );
			mov		[XEN],cx
			mov		bx,BOTTOM_EDGE
			mov		[YEN],bx

.clpb_c:
		
;	if (ey >(DIBHeight-1))
;	{
;		ex = ex - ( (ex-sx) *(ey-DIBHeight) / (ey-sy) );
;		ey = DIBHeight-1;
;	}

;	if (ey <0)
;	{
;		return;
;	}
;		cmp		bx,BOTTOM_EDGE
;		cmp		dx,BOTTOM_EDGE
;		jg		.exit
;	if (sy >(DIBHeight-1))
;	{	
;		return;
;	}

;		cmp		bp,BOTTOM_EDGE
;		jge		.exit
;	if (ey >(DIBHeight-1))
;	{
;		ex = ex - ( (ex-sx) *(ey-DIBHeight) / (ey-sy) );
;		ey = DIBHeight-1;
;	}
	
		mov		cx,[XST]
		mov		dx,[YST]
		mov		bp,[XEN]
		mov		si,[YEN]
		call	Line11

.exit	ret
		
Line11:		cmp		dx,si 	; vs le ve?
			jnc		.noswap	; yes
				xchg	si,dx	; swap vs,ve
				xchg	bp,cx	; swap hs,he
				jmp	.vdiff

.noswap:	jne		.vdiff			; v not same
			cmp		cx,bp		; v is same 
			je 		.quit		; h is same

.vdiff:		sub		dx,si	; ver diff = ve-vs
			sub		cx,bp	; hs le he? - replace with sub below? todo
			jnc		.hforward	; yes

.hbackward:	neg 	cx	 		; decrement line across everytime and line down every few times
 			cmp		cx,dx
			jne		.hbnodiag ; .quit ; .vbackward ; .quit ;	.vbackward
					inc		cx	; diagonal
.hbnodiag:	jc		.vbackward

			div 	cx		; dx:ax *bx = ax*65536 *bx
			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
			mov		di,[ds:si]	; lookup start ver		
	
.hbloop:		add 	al,ah	; fraction to current
				jnc		.hbplot	;
					add		si,2
					mov		di,[ds:si]	; lookup start ver

.hbplot:		mov		bx,[HorPosTable+bp]
				mov		dh,[OrTable+bp]
				or byte [es:di+bx],dh

				sub		bp,2		; next hor
				loop	.hbloop		; dec cx jnz
.quit:		ret

.vbackward: xchg 	dx,cx			; decrement line down everytime and line across every few times
			div 	cx		; dx:ax *bx = ax*65536 *bx

			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
	
			mov		dh,[OrTable+bp]
			mov		bx,[HorPosTable+bp]
	
.vbloop:		add 	al,ah	; fraction to current
				jnc		.vbplot	;
					sub		bp,2
					mov		dh,[OrTable+bp]
					mov		bx,[HorPosTable+bp]
		
.vbplot:		mov		di,[ds:si]	; lookup start ver
				or byte [es:di+bx],dh

				add		si,2		; next ver
				loop	.vbloop		; dec cl jnz
			ret

.hforward: 	cmp		cx,dx			; increment line across everytime and line down every few times
			jne		.hfnodiag
				inc 	cx	; diagonal
.hfnodiag:	jc		.vforward

			div 	cx		; dx:ax *bx = ax*65536 *bx
			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
			mov		di,[ds:si]	; lookup start ver

.hfloop:		add 	al,ah	; fraction to current
				jnc		.hfplot	;
					add		si,2
					mov		di,[ds:si]	; lookup start ver		

.hfplot:		mov		bx,[HorPosTable+bp]
				mov		dh,[OrTable+bp]
				or byte [es:di+bx],dh

				add		bp,2		; next hor
				loop	.hfloop		; dec cx jnz
			ret

;	mov		di,[ds:si]

.vforward:	xchg 	dx,cx			; increment line down everytime and line across every few times
			div 	cx		; dx:ax *bx = ax*65536 *bx

			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable

			mov		dh,[OrTable+bp]
			mov		bx,[HorPosTable+bp]
	
.vfloop:		add 	al,ah	; fraction to current
				jnc		.vfplot	;
					add		bp,2
					mov		dh,[OrTable+bp]
					mov		bx,[HorPosTable+bp]
		
.vfplot:		mov		di,[ds:si]	; lookup start ver
				or byte [es:di+bx],dh

				add		si,2		; next ver
				loop	.vfloop		; dec cl jnz
			ret

Line00:		cmp		dx,si 	; vs le ve?
			jnc		.noswap	; yes
				xchg	si,dx	; swap vs,ve
				xchg	bp,cx	; swap hs,he
				jmp	.vdiff

.noswap:	jne		.vdiff			; v not same
				cmp		cx,bp		; v is same 
				je 		.quit		; h is same

.vdiff:		sub		dx,si	; ver diff = ve-vs
			sub		cx,bp	; hs le he? - replace with sub below? todo
			jnc		.hforward	; yes

.hbackward:	neg 	cx		; decrement line across everytime and line down every few times
 
			cmp		cx,dx
			jne		.hbnodiag ; .quit ; .vbackward ; .quit ;	.vbackward
				inc	cx	; diagonal
.hbnodiag:	jc		.vbackward

			div 	cx		; dx:ax *bx = ax*65536 *bx
			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
			mov		di,[ds:si]	; lookup start ver
	
.hbloop:		add 	al,ah	; fraction to current
				jnc		.hbplot	;
					add		si,2
					mov		di,[ds:si]	; lookup start ver

.hbplot:		mov		bx,[HorPosTable+bp]
				mov		dh,[AndTable+bp]
				and byte [es:di+bx],dh

				sub		bp,2		; next hor
				loop	.hbloop		; dec cx jnz
.quit:		ret

.vbackward:	xchg 	dx,cx		; decrement line down everytime and line across every few times
			div 	cx		; dx:ax *bx = ax*65536 *bx

			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
			mov		dh,[AndTable+bp]
			mov		bx,[HorPosTable+bp]
	
.vbloop:		add 	al,ah	; fraction to current
				jnc		.vbplot	;
					sub		bp,2
					mov		dh,[AndTable+bp]
					mov		bx,[HorPosTable+bp]
		
.vbplot:		mov		di,[ds:si]	; lookup start ver
				and byte [es:di+bx],dh

				add		si,2		; next ver
				loop	.vbloop		; dec cl jnz
			ret

.hforward: 	cmp		cx,dx		; increment line across everytime and line down every few times
			jne		.hfnodiag
				inc 	cx	; diagonal
.hfnodiag:	jc		.vforward

			div 	cx		; dx:ax *bx = ax*65536 *bx
			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
			mov		di,[ds:si]	; lookup start ver
	
.hfloop:		add 	al,ah	; fraction to current
				jnc		.hfplot	;
					add		si,2
					mov		di,[ds:si]	; lookup start ver		

.hfplot:		mov		bx,[HorPosTable+bp]
				mov		dh,[AndTable+bp]
				and byte [es:di+bx],dh
	
				add		bp,2		; next hor
				loop	.hfloop		; dec cx jnz
			ret

.vforward: 	xchg 	dx,cx			; increment line down everytime and line across every few times
			div 	cx		; dx:ax *bx = ax*65536 *bx

			add		si,si	; start ver *2
			add		bp,bp	; start hor *2
			add		si,BackBufferTable
			mov		dh,[AndTable+bp]
			mov		bx,[HorPosTable+bp]
	
.vfloop:		add 	al,ah	; fraction to current
				jnc		.vfplot	;
					add		bp,2
					mov		dh,[AndTable+bp]
					mov		bx,[HorPosTable+bp]
		
.vfplot:		mov		di,[ds:si]	; lookup start ver
				and byte [es:di+bx],dh

				add		si,2		; next ver
				loop	.vfloop		; dec cl jnz
			ret

GetKey:
	mov	ah,11h
	int	16h         	; has a key been pressed
	jz	.key_none ; no
	mov	ah,10h      ; yes 
	int	16h         	; get it in AX (al= ascii, ah=scan code)
	cmp	al,KEY_ESC
	je	.key_esc
	cmp 	al,KEY_Q
	je 	.key_q
	cmp 	al,KEY_W
	je 	.key_w
	cmp 	al,KEY_E
	je 	.key_e
	cmp 	al,KEY_R
	je 	.key_r
.key_none:
	ret
.key_esc:	mov	al,1
		mov	[Exit],al
		ret
.key_q:	mov	ax,[XAngle]
		inc	ax
		and ah,03
		mov	[XAngle],ax
		ret
.key_w:	mov	ax,[XAngle]
		dec	ax
		and ah,3
		mov	[XAngle],ax
		ret
.key_e:	mov	ax,[YAngle]
		inc	ax
		and ah,3
		mov	[YAngle],ax
		ret
.key_r:	mov	ax,[YAngle]
		dec	ax
		and ah,3
		mov	[YAngle],ax
		ret
		ret

WaitVSync: 			mov		dx,INPUT_STATUS_1	; Wait for the leading edge of vertical sync pulse.
WaitNotVSyncLoop:		in		al,dx
						and		al,VSYNC_MASK
						jnz		WaitNotVSyncLoop
WaitVSyncLoop:			in		al,dx
						and		al,VSYNC_MASK
						jz		WaitVSyncLoop
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

;	mov		ax,0 ; 00101101001011010b ;
	xor		ax,ax
	mov		di,ax

;	xor		di,di		; DS:SI points to even back buffer	
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

;								mov		cx,[ObjectYPos+bx] ; starty
;								mov		dx,[ObjectXPos+bx] ; startx

;								mov		bp,[ObjectYPos+bx] ; endy 
;								mov		si,[ObjectXPos+bx] ; endx - todo - si is the problem?

;.draw_clipped_line:				call	Line11

.top_p1off_p2on:	mov		ax,[YST]
					mov		cx,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]
					call	ClipTop

					mov		[YEN],di
					mov		bx,TOP_EDGE
					mov		[XEN],bx
	
.left_p1off_p2on:	mov		cx,[YST]
					mov		ax,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]	
					call	ClipLeft 	; ax returned 
	
					mov		[XEN],bx
					mov		bx,LEFT_EDGE
					mov		[YEN],bx
		
LineClip1off_p2on:	mov		bp,[YST]
					mov		dx,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]	
					call	ClipRight

					mov		[XEN],bp
					mov		bx,RIGHT_EDGE
					mov		[YEN],bx
	
.bottom_p1off_p2on:	mov		ax,[YST]
					mov		dx,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]	
					call	ClipBottom

					mov		[YEN],cx
					mov		bx,BOTTOM_EDGE
					mov		[XEN],bx

	
ClipBehind:	mov		bp,cx	; backup pon.z (onh)
			sub		cx,di 	; pon.z-poff.z (dh)

			mov		di,ax	; backup pon.x (onv)
			sub		ax,bx	; pon.x-poff.x (dv)

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

ClipTop:	mov		bp,cx	; backup pon.x (onh)
			sub		cx,di 	; pon.x-poff.x (dh)

			mov		di,ax	; backup pon.y (onv)
			sub		ax,bx	; pon.y-poff.y (dv)

			imul	bp	; pon.y-poff.y (dv) * pon.x (onh) 
			idiv	cx	; 	/ pon.x-poff.x (dh) (pon.x+poff.x)

			sub		di,ax
			ret

ClipLeft:	mov		bp,cx	; backup pon.x (onh)
			sub		cx,bx 	; pon.x-poff.x (dh)

			mov		bx,ax	; backup pon.y (onv)
			sub		ax,di	; pon.y-poff.y (dv)

			imul	bp	; pon.y-poff.y (dv) * pon.x (onh) 
			idiv	cx	; 	/ pon.x-poff.x (dh) (pon.x+poff.x)

			sub		bx,ax
			ret

ClipBottom:	mov		bp,BOTTOM_EDGE
			sub		bp,dx	; rightedge - pon.x (onh)
			sub		di,dx	; poff.x - pon.x (dh)

			mov		cx,ax
			sub		ax,bx	; pon.y-poff.y (dv)

			imul	bp
			idiv	di
	
			sub		cx,ax
			ret

ClipRight:	mov		ax,RIGHT_EDGE
			sub		ax,bp	; rightedge - pon.x (onh)
			sub		bx,bp	; poff.x - pon.x (dh)
	
			mov		bp,dx
			sub		dx,di	; pon.y-poff.y (dv)

			imul	dx
			idiv	bx

			sub		bp,ax
			ret
	
GetSin256:	cmp		bh,0
			je		.s000090
.s090180:	cmp		bh,1
			jne		.s180270
				xor		bh,bh
				xor		bl,255
.s000090		mov		al,[sintab+bx]
				ret
.s180270:	cmp		bh,2
			jne		.s270000
				xor		bh,bh
				mov		al,[sintab+bx]
				neg		al
				ret
.s270000:	xor		bh,bh
			xor		bl,255
			mov		al,[sintab+bx]
			neg		al
			ret

SinCos:		mov		bx,bp
			call	GetSin256
			cbw
			mov		bx,bp
			add		ax,ax
			mov		bp,ax
			inc		bh
			and		bh,3
			call	GetSin256
			cbw
			add		ax,ax
			ret

SinCosV2:	mov		bx,bp
			call	GetSin256
			cbw
			mov		bx,bp
			mov		bp,ax
			inc		bh
			and		bh,3
			call	GetSin256
			cbw
			ret
				

BuildRotateMatrixV2	mov		bp,[XAngle]
					call	SinCosV2
					mov		[SinX],bp 	; rs
					mov		[CosX],ax	; rc
;					mov		[SinX],ax 	; rs
;					mov		[CosX],bp	; rc

					mov		bp,[YAngle]
					call	SinCosV2
					mov		[SinY],bp	; ps
					mov		[CosY],ax	; pc
;					mov		[SinY],ax	; ps
;					mov		[CosY],bp	; pc
					
					ret
					
HexChar:	mov		si,di
			mov		bl,al
			shr		bl,1
			shr		bl,1
			shr		bl,1
			shr		bl,1
			and		bx,15
			mov		ah,[Hex0+bx]
			mov		[es:di],ah
			add		di,8192
			mov		ah,[Hex1+bx]
			mov		[es:di],ah
			sub		di,8192-80
			mov		ah,[Hex2+bx]
			mov		[es:di],ah
			add		di,8192
			mov		ah,[Hex3+bx]
			mov		[es:di],ah
			sub		di,8192-80
			mov		ah,[Hex4+bx]
			mov		[es:di],ah

			mov		di,si
			inc		di

			mov		bl,al
			and		bl,15
			mov		al,[Hex0+bx]
			mov		[es:di],al
			add		di,8192
			mov		al,[Hex1+bx]
			mov		[es:di],al
			sub		di,8192-80
			mov		al,[Hex2+bx]
			mov		[es:di],al
			add		di,8192
			mov		al,[Hex3+bx]
			mov		[es:di],al
			sub		di,8192-80
			mov		al,[Hex4+bx]
			mov		[es:di],al
			ret

Hex32:		push	ax

			push	dx
			push	bx
			mov		al,dh
			call	Hex8
						
			pop		bx
			pop		dx

			push	bx
			mov		al,dl
			mov		di,2
			call	HexByte

			pop		bx
			pop		ax

			push	ax
			push	bx
			mov		al,ah
			mov		di,4
			call	HexByte

			pop		bx
			pop		ax
			mov		di,6
			jmp		HexByte	; does ret

Hex24:		push	ax
			push	bx

			mov		al,bh
			call	Hex8
						
			pop		bx
			pop		ax

			push	ax
			push	bx

			mov		di,2
			call	HexByte

			pop		bx
			pop		ax

			mov		al,ah
			mov		di,4
			jmp		HexByte	; does ret

Hex16:		push	ax
			push	bx

			mov		al,ah
			call	Hex8
						
			pop		bx
			pop		ax

			mov		di,2
			jmp		HexByte ; does ret

Hex8:		xor		di,di
HexByte:	xor		bh,bh
			mov		ah,bl
			add		ah,ah	; *2
			add		bl,ah	; *3
			add		bx,bx	; *6
			add		bx,bx	; *12
			add		di,[BackBufferTable+bx]	; lookup start ver
			jmp		HexChar	 ; does ret
;			ret

%macro  NEGATE 2
			neg		%1
			adc		%2,0
			neg		%2
%endmacro

%macro  MULU3216 0
			mul		bp 		; H1 x L2
			xchg	ax,bx 	; swap ax and H2
			mul 	cx 		; H2 x l1
			add		bx,ax	; add ax to h2
			mov		ax,cx	; mov l1 to ax
			mul 	bp 		; l1 x l2
			add		bx,dx	; add dx to h2
%endmacro

;Neg32V1:	xor		bx,bx	; in=ax,dx out=ax,bx
;			neg		ax
;			sbb		bx,dx
;			ret

;Neg32V2:	neg		ax
;			adc		dx,0
;			neg		dx
;			ret
			
Muli32:		xor		si,si		; clear neg count
			test	ah,ah		; check sign of H1
			jns	.test1
				NEGATE	cx,ax
				inc		si		; inc neg count
.test1:		test	bh,bh		; check sign of H2
			jns	.test2
				NEGATE	bp,bx
				dec		si		; dec neg count
.test2:		MULU3216					;	call	Mulu32	; do mulu32
			test	si,si
			jz		.test3
				NEGATE	ax,bx
.test3:		ret

; ax = H1
; bx = H2
; cx = L1
; bp = L2
; H1 * L2
; H2 * L1
; L1 x L2			
Mulu32:		MULU3216
;			mul		bp 		; H1 x L2
;			xchg	ax,bx 	; swap ax and H2
;			mul 	cx 		; H2 x l1
;			add		bx,ax	; add ax to h2
;			mov		ax,cx	; mov l1 to ax
;			mul 	bp 		; l1 x l2
;			add		bx,dx	; add dx to h2
			ret 			; rc = bx + ax

section .data align=8 ; 16 ; 8 ; 16

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

HorizonTable:
	dw	100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,
	dw	132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,
	dw	164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,
	dw	196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,
	dw	228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,
	dw	260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,
	dw	292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,
	dw	324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,
	dw	-156,-155,-154,-153,-152,-151,-150,-149,-148,-147,-146,-145,-144,-143,-142,-141,-140,-139,-138,-137,-136,-135,-134,-133,-132,-131,-130,-129,-128,-127,-126,-125,
	dw	-124,-123,-122,-121,-120,-119,-118,-117,-116,-115,-114,-113,-112,-111,-110,-109,-108,-107,-106,-105,-104,-103,-102,-101,-100,-99,-98,-97,-96,-95,-94,-93,
	dw	-92,-91,-90,-89,-88,-87,-86,-85,-84,-83,-82,-81,-80,-79,-78,-77,-76,-75,-74,-73,-72,-71,-70,-69,-68,-67,-66,-65,-64,-63,-62,-61,
	dw	-60,-59,-58,-57,-56,-55,-54,-53,-52,-51,-50,-49,-48,-47,-46,-45,-44,-43,-42,-41,-40,-39,-38,-37,-36,-35,-34,-33,-32,-31,-30,-29,
	dw	-28,-27,-26,-25,-24,-23,-22,-21,-20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,
	dw	4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,
	dw	36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
	dw	68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,

Hex0	db 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1	db 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2	db 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3	db 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4	db 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b
	
%include  'sin256.inc'

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words
BackBuffer: 		resw 16384	; 2 screen buffers

XAngle:	resw	1
YAngle:	resw	1

SinX:	resw	1
CosX:	resw	1
SinY:	resw	1
CosY:	resw	1

HorEdge	resw	1

XST		resw	1
YST		resw	1
XEN		resw	1
YEN		resw	1

Exit:	resb	1

Test0: 	resw	1
Test1: 	resw	1
Test2: 	resw	1
Test3: 	resw	1
Test4: 	resw	1
Test5: 	resw	1
Test6: 	resw	1
Test7: 	resw	1
Test8: 	resw	1
Test9: 	resw	1
Test10: resw	1
Test11: resw	1

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer

