; nasm cga_pers45_v09.asm -o cgapers.com -f bin

VIDEO_SEGMENT	equ	0b800h 	; display memory segment for true CGA graphics modes
SW				equ 80 		; screen width
SE				equ	0		; screen even start
SO				equ	8192	; screen odd start


INPUT_STATUS_1	equ	03dah	; VGA status register
VSYNC_MASK	equ	08h	; vertical sync bit in status register 1
DE_MASK		equ	01h	; display enable bit in status register 1

MAP_MASK	equ	2		; SC map mask register
SC_INDEX	equ	3c4h	; SC index register

MAX_HPOS	equ	159
MAX_VPOS	equ	99

MIDDLE_HOR	equ	160
MIDDLE_VER	equ	100

KEY_1	EQU	0231h
KEY_2	EQU	0332h
KEY_3	EQU	0433h
KEY_4 	EQU	0534h
KEY_5 	EQU	0635h
KEY_6 	EQU	0736h
KEY_7 	EQU	0837h
KEY_8 	EQU	0938h
KEY_ESC	EQU 27

OFF_LEFT	EQU		1
OFF_RIGHT	EQU		2
OFF_TOP		EQU		4
OFF_BOTTOM	EQU		8
OFF_BEHIND	EQU		128

CPU 8086
bits 16
org 100h

section .text

start:
	mov		ax,04h 	; CGA 320 x 200 4 colors
	int		10h

	cld
	call	BuildScreenTable
	
	mov		bx,(32768/16) ; 1000h ; (16384/16)
	call	AllocateMemory
	mov		[BackBufferSeg],ax

	mov		bx,(65536/16) ; 3f00h ; 3fffh ; 1000h ; (65535/16)
	call	AllocateMemory
	mov		[PersSeg],ax
	
	mov		bp,MAX_VPOS
	xor		si,si
	call	BuildPersTable
	mov		bp,MAX_HPOS
	mov		si,32766
	call	BuildPersTable

;	mov	ax,0Bh 	; Pallette
;	mov	bh,1
;	mov	bl,0
;	mov	bx,00000h ; pallette 0 high
;	int	10h 
	
;	mov		ah,4ah
;	mov		bx,1000h
;	int		21h
;	mov		ah,48h
;	mov		bx,1000h
;	int		21h
;	mov		[BackBufferSeg],ax
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front

		mov		bx,[XOffset]
		mov		cx,[YOffset]
		mov		si,[ZOffset]
		call	ClassifyPoint
		xor		dh,dh
		mov		[ClipCode],dx
		
		mov		bx,[BackBufferSeg]
		mov		es,bx

	mov		bx,[XOffset]
	mov		cx,[YOffset]
	mov		si,[ZOffset]
	call	Perspective
	mov		bp,dx

	mov		ax,[BackBufferSeg]
	mov		es,ax
	call	Plot11

	mov		bp,00
	mov		di,70
	mov		ax,[XOffset]
	call	Chars

	mov		bp,06
	mov		di,70
	mov		ax,[YOffset]
	call	Chars

	mov		bp,12
	mov		di,70
	mov		ax,[ZOffset]
	call	Chars

	mov		bp,20
	mov		di,70
	mov		ax,[BackBufferSeg]
	call	Chars

	mov		bp,26
	mov		di,70
	mov		ax,[PersSeg]
	call	Chars

	mov		bp,32
	mov		di,70
	mov		ax,[ClipCode]
	call	Chars

	mov		bx,[XOffset]
	mov		cx,[YOffset]
	mov		si,[ZOffset]
	call	Perspective
	mov		ax,[BackBufferSeg]
	mov		es,ax
	mov		bp,40
	mov		di,70
	mov		ax,bx
	call	Chars

	mov		bx,[XOffset]
	mov		cx,[YOffset]
	mov		si,[ZOffset]
	call	Perspective
	mov		ax,[BackBufferSeg]
	mov		es,ax
	mov		bp,46
	mov		di,70
	mov		ax,dx
	call	Chars

;	mov		bx,64
;	mov		cx,64
;	mov		si,1
;	call	Perspective
;	mov		cx,[BackBufferSeg]
;	mov		es,cx
;	mov		bp,52
;	mov		di,70
;	mov		ax,bx
;	call	Chars


;	mov		cx,0 ; si,0
;	mov		dx,0 ; bp,0
;	mov		si,[PointList+6] ; cx,[PointList+6]
;	mov		bp,[PointList+4] ; dx,[PointList+4]
	mov		si,100				; vs
	mov		dx,[PointList+4]	; ve
	mov		bp,160				; hs
	mov		cx,[PointList+6]	; he
;		call	Line8 	; LineHtest8
		
		mov		cx,2
		call	Move
	
		call	GetKey
	jnc		MainLoop

Done: ; Finished, clear key, reset screen mode and exit.
	mov		ax,3		; reset to text mode
	int		10h
	mov		ah,4ch	; exit to DOS
	int		21h

; MAX_EDGE_MULTIPLIER = MAX_EDGE * MULTIPLIER
; VALUE ( DISTANCE ) = MAX_EDGE_MULTIPLIER / ( MAX_EDGE + DISTANCE )

BuildPersTable:	; bp = max_hv_pos	; es = segment to store result
	mov		es,[PersSeg]
	mov		cx,32767					; set count
	mov		ax,bp						; max_hv_pos
	mul		cx							; max_hv_pos *32767
	mov		bx,ax 						; backup max_hvpos_shift_lo
	mov		di,dx 						; backup max_hvpos_shift_hi
	mov		cx,16382					; count
.loop:
		mov		ax,bx					; restore max_hvpos_shift_lo
		mov		dx,di					; restore max_hvpos_shift_hi
		div		bp						; max_hvpos_shift / max_hv_pos+distance
		mov		[es:si],ax
		add		si,2					; inc distance
		add		bp,2					; inc max_hv_pos+distance
		dec		cx						; dec count
		jnz		.loop
	ret

Perspective: ; in = si/bx/cx - out = bx/dx
	mov		es,[PersSeg]
	and		si,0fffeh	; z set to even

	mov		ax,[es:si]
	add		bx,bx
	IMUL	bx
	add		dx,MIDDLE_VER
	mov		bx,dx
	
	mov		ax,[es:si+32766]
	add		cx,cx
	IMUL	cx
	add		dx,MIDDLE_HOR
	
	ret

ClassifyPoint:	; in = si/bx/cx - out = dl
	add		si,MIDDLE_VER

	xor		dl,dl	; clear clipcodes
.hor:
	test	bx,bx 		; test if -ve
	jns		.right	
.left:
		neg		bx
		cmp		bx,si
		jl		.ver
			mov		dl,OFF_LEFT
			jmp		.ver
.right:
		cmp		bx,si
		jl		.ver
			mov		dl,OFF_RIGHT
.ver:
	sub		si,(MIDDLE_VER-MIDDLE_HOR)

	test	cx,cx 		; test if -ve
	jns		.bottom
.top:
		neg		cx
		cmp		cx,si
		jl		.quit
			or		dl,OFF_TOP
			ret
.bottom:
		cmp		cx,si
		jl		.quit
			or		dl,OFF_BOTTOM
.quit:
		ret

;.horpos:
;	cmp	bp,TOP_EDGE
;	jge	.right_test
;		mov	dl,TOP_CC
;		jmp	.hor_test_end
;.right_test:
;	cmp	bp,BOTTOM_EDGE
;	jle	.hor_test_end;
;		mov	dl,BOTTOM_CC
;
;.hor_test_end:
;	cmp	ax,LEFT_EDGE
;	jge	.bottom_test
;		or	dl,LEFT_CC
;		ret
;.bottom_test:
;	cmp	ax,RIGHT_EDGE
;	jle	.ver_test_end;
;		or	dl,RIGHT_CC 			
;.ver_test_end:
;	ret

;ClipCode:	; in = si/bx/cx - out = al
;	xor		al,al

;	cmp		si,bx
;	bcc		
	
;	cmp		si,cx
	
;	for (int distance=0; distance!=32768; ++distance)
;	{
;		const int max_hpos_shift 		= max_hpos*32767;
;		const int max_hpos_distance		= max_hpos+distance;
;		int hpos 						= max_hpos_shift/max_hpos_distance;
;		pers_45_hor[distance] 			= hpos;
;	}

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

AllocateMemory:
	mov		bp,bx	; backup request
	mov		ah,4ah
	int		21h
	mov		ah,48h
	mov		bx,bp	; restore request
	int		21h
	ret

GetKey:
	clc
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
	cmp 	ax,KEY_7
	je 	.key_7
	cmp 	ax,KEY_8
	je 	.key_8
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
.key_7:
	mov ax,[WOffset]
	sub ax,1
	mov [WOffset],ax
	clc
	ret
.key_8:
	mov ax,[WOffset]
	add ax,1
	mov [WOffset],ax
	clc
	ret

BuildScreenTable:
	xor		ax,ax	; even lines
	lea		di,[BackBufferTable]
	mov		cl,100
.loop:
		mov		[di],ax
		add		ax,8192
		mov		[di+2],ax
		sub		ax,8192-80
		add		di,4
		dec		cl
		jnz		.loop
	ret
	
CopyClearBackBuffer:
	mov		bp,ds			; backup ds

	mov		ds,[BackBufferSeg]

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

Chars: ; bp = ver / di = hor / ax = num
	add		bp,bp 			; start ver *2
	add		di,[BackBufferTable+bp]	; lookup start ver
	xor		bh,bh

	mov		bl,10
	test	ax,ax 		; test if -ve
	jns		.pos
			add		bl,1
			neg		ax
.pos:
	call	Char
	add		di,1
	
	xor		dx,dx
  	mov		cx,10000
 	div		cx
	mov		bl,al
	call	Char
	add		di,1

	mov		ax,dx
	xor		dx,dx
  	mov		cx,1000
	div		cx
	mov		bl,al
	call	Char
	add		di,1

	mov		ax,dx
	xor		dx,dx
  	mov		cx,100
	div		cx
	mov		bl,al
	call	Char
	add		di,1

	mov		ax,dx
	xor		dx,dx
  	mov		cx,10
	div		cx
	mov		bl,al
	call	Char
	add		di,1

	mov		bl,dl
	call	Char
	add		di,1

	mov		bl,12
	call	Char

 ret

Char:
	mov		cl,[Char0+bx]
	mov		[es:di+8192],cl
	mov		cl,[Char1+bx]
	mov		[es:di+80],cl
	mov		cl,[Char2+bx]
	mov		[es:di+8192+80],cl
	mov		cl,[Char3+bx]
	mov		[es:di+160],cl
	mov		cl,[Char4+bx]
	mov		[es:di+8192+160],cl

	xor 	cl,cl
	mov		[es:di],cl
	mov		[es:di+240],cl
	
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

Char0:		db	00111111b,00001100b,00111111b,00111111b,00110011b,00111111b,00111111b,00111111b,00111111b,00111111b,00001100b,00000000b,0
Char1:		db	00110011b,00111100b,00000011b,00000011b,00110011b,00110000b,00110000b,00000011b,00110011b,00110011b,00001100b,00000000b,0
Char2:		db	00110011b,00001100b,00111111b,00111111b,00111111b,00111111b,00111111b,00000011b,00111111b,00111111b,00111111b,00111111b,0
Char3:		db	00110011b,00001100b,00110000b,00000011b,00000011b,00000011b,00110011b,00000011b,00110011b,00000011b,00001100b,00000000b,0
Char4:		db	00111111b,00111111b,00111111b,00111111b,00000011b,00111111b,00111111b,00000011b,00111111b,00111111b,00001100b,00000000b,0

PointList	dw 0b8h, 13ch, 22h, 0ebh
DirList		dw 1,-1,-1,1
 	
WOffset:	dw	60
XOffset:	dw	50
YOffset:	dw	50
ZOffset:	dw	70 ; 2500	

section .bss align=8 	; put uninitialized data here

BackBufferTable	resw 	200	; 200 lines of words
BackBufferSeg	resw	1	; pointer to the segment containing the back buffer
;PersHorSeg		resw	1	; pointer to segment containing hor perspective table
;PersVerSeg		resw	1	; pointer to segment containing ver perspective table
PersSeg			resw	1	; pointer to segment containing perspective tables
ClipCode		resw	1
