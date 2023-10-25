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
	add ax,2
	mov	[XAngle],ax
	add ax,2
	mov	[YAngle],ax	
	
;	mov	[Test0],ax
	
MainLoop:
		call	WaitVSync
		call	CopyClearBackBuffer
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		call	Horizon

;		mov		bl,0
;		mov		ax,[XAngle]
;		call	Hex16
;		mov		bl,1
;		mov		ax,[YAngle]
;		call	Hex16
;		mov		bl,3
;		mov		ax,[SinX]
;		call	Hex16
;		mov		bl,4
;		mov		ax,[CosX]
;		call	Hex16
;		mov		bl,6
;		mov		ax,[SinY]
;		call	Hex16
;		mov		bl,7
;		mov		ax,[CosY]
;		call	Hex16

;		mov		bl,9
;		mov		ax,[Test0]
;		call	Hex16
;		mov		bl,10
;		mov		ax,[Test1]
;		call	Hex16
;		mov		bl,11
;		mov		ax,[Test2]
;		call	Hex16
;		mov		bl,12
;		mov		ax,[Test3]
;		call	Hex16
;		mov		bl,13
;		mov		ax,[Test4]
;		call	Hex16
;		mov		bl,15
;		mov		ax,[Test5]
;		call	Hex16

;		mov		bl,22
;		mov		ax,[Test6]
;		call	Hex16
;		mov		bl,23
;		mov		ax,[Test7]
;		call	Hex16
;		mov		bl,24
;		mov		ax,[Test8]
;		call	Hex16
;		mov		bl,25
;		mov		ax,[Test9]
;		call	Hex16
;		mov		bl,26
;		mov		ax,[Test10]
;		call	Hex16

;		mov		bl,29
;		mov		ax,[BackBufferSeg]
;		call	Hex16

;		mov		bl,30
;		mov		ax,[Test11]
;		call	Hex16

;		call	BuildRotateMatrixV2

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
		mov		[SinX],bp
		mov		[CosX],ax
		
		mov		cx,ax		; CosX
		mov		ax,160
		imul	bp			; *SinX - could be replaced with (bp*128) + (bp*32)
		idiv	cx
		mov		[HorEdge],ax

		mov		bx,[YAngle]	; pitch
		and		bx,1022 	; = 511*2
		mov		bx,[HorizonTable+bx]

		mov		[HorDir],bp
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

		test	bx,bx	; bottom if off top
		js		.fill_whole_sky

		cmp		bp,BOTTOM_EDGE	; top if off bottom
		jg		.fill_whole_ground

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
			jmp		.clpt_e

.clpt_c		je	.clpt_e		; is top on the top edge
				mov		si,bp	; backup
				push	cx
				push	bx

				mov		bx,bp
				xor		bp,bp
				mov		ax,1010101010101010b ; 1001110001111000b
				call	FillBlock

				pop		bx		; restore
				pop		cx
				mov		bp,si

.clpt_e cmp		bx,BOTTOM_EDGE
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
			jmp		.clpb_e

.clpb_c:	je	.clpb_e		; is bottom on the bottom edge

				mov		si,bp	; backup
				push	cx
				push	bx

.				mov		bp,BOTTOM_EDGE
				sub		bp,bx
				xchg	bp,bx
;		mov [Test9],bx
;		mov [Test10],bp
				mov		ax,0101010101010101b ; 1001110001111000b ; 0101010101010101b ; 1001110001111000b
				call	FillBlock

				pop		bx		; restore
				pop		cx
				mov		bp,si

; hdiff = xen-xst
; vdiff = yen-yst

; (hdiff *128) / (vdiff/2)

.clpb_e:
		mov		cx,[XST]
		mov		dx,[YST]
		mov		bp,[XEN]
		mov		si,[YEN]
		
;	mov		[Test1],cx
;	mov		[Test2],dx
;	mov		[Test3],bp
;	mov		[Test4],si

;		call	Line11

		mov		cx,[YEN]
		mov		bx,[YST]
		sub		cx,bx
		mov		[VDiff],cx
;	mov		[Test9],cx ; vdiff

;		sar		cx,1
;		mov		[Test10],cx ; vdiff /2
		jne		.lne_c
			xor		ax,ax
			mov		[Slope],ax
			jmp		.cont

.lne_c:	mov		bp,[HorDir]
		test	bp,bp
		jns		.lft_ln
			mov		ax,[XST]
			mov		bx,[XEN]
			sub		ax,bx
			jmp		.cnt1
.lft_ln:
			mov		ax,[XEN]
			mov		bx,[XST]
			sub		ax,bx

.cnt1:	mov		[HDiff],ax
;	mov		[Test8],ax
		mov		cl,7
		sal		ax,cl		; hdiff *128
		xor 	dx,dx
;	mov		[Test7],ax
		mov		cx,[VDiff]
;	mov		cx,[Test10]
		div		cx
;;		add ax,ax
;	mov		[Test6],ax
		mov		[Slope],ax

.cont:
		mov		si,[VDiff]
		test	si,si
		je		.no_slope

		mov		dx,[XST]
		mov		cx,7
		sal		dx,cl

		mov		bp,[YST]
		add		bp,bp
	
		mov		di,[Slope]
	
		mov		bx,[HorDir]
		test	bx,bx
		js		.filln

.fillp:	mov		[.fillpval+2],di

.fillplp:	mov		di,[BackBufferTable+bp]
			mov		bx,dx
			
			xchg	bh,bl	;	v???????vvvvvvvv
			add 	bh,bh	;	???????0vvvvvvvv c
			xor 	bh,bh	;	00000000vvvvvvvv c
			rcl 	bx,1	;	0000000vvvvvvvvc

	mov		cl,[FillTabLeft+bx]
	mov		ax,0101010101010101b
	shr		cl,1	; shift lsb into carry
	rep		stosw
	rcl		cl,1	; rotate lsb from carry
	rep		stosb

			mov 	al,[FillCharLeftTab+bx]
			stosb

	mov		cl,[FillTabRight+bx]
	mov		ax,1010101010101010b
	shr		cl,1	; shift lsb into carry
	rep		stosw
	rcl		cl,1	; rotate lsb from carry
	rep		stosb

			add		bp,2

.fillpval:	add		dx,1234
			dec		si
			jnz		.fillplp
		ret

.filln:	mov		[.fillnval+2],di

.fillnlp:	mov		di,[BackBufferTable+bp]
			mov		bx,dx

			xchg	bh,bl	;	v???????vvvvvvvv
			add 	bh,bh	;	???????0vvvvvvvv c
			xor 	bh,bh	;	00000000vvvvvvvv c
			rcl 	bx,1	;	0000000vvvvvvvvc

			mov		cl,[FillTabLeft+bx]
			mov		ax,1010101010101010b
		shr		cl,1	; shift lsb into carry
		rep		stosw
		rcl		cl,1	; rotate lsb from carry
		rep		stosb

			mov 	al,[FillCharRightTab+bx]
			stosb

		mov		cl,[FillTabRight+bx]
		mov		ax,0101010101010101b
		shr		cl,1	; shift lsb into carry
		rep		stosw
		rcl		cl,1	; rotate lsb from carry
		rep		stosb

			add		bp,2
.fillnval:	sub		dx,1234
			dec		si
			jnz		.fillnlp

.no_slope:
		ret

.fill_whole_sky:	mov		ax,0101010101010101b
					xor		bp,bp
					mov		bx,200
					jmp		FillBlock

.fill_whole_ground:	mov		ax,1010101010101010b
					xor		bp,bp
					mov		bx,200
					jmp		FillBlock


;fill_blocks
; if hst ne 0
; calc lines = hst
; fill top block (0(bp),lines=bx,colour(ax))
; endif
; if hend ne 199
; fill bottom block(hen(bp),(199-hen)=bx,colour(ax))
; endif

;Fill_Block:
;	inc
	
; fill block()
; inc bl (number of lines)
; bl/2 (number of lines/2)
; di = vertab(bp) start line
; cx= wordcounttab(bx) (number of lines *40 (words))
; rep stosw (40*cx)
; di = vertab(bp+1) start line other page
; cx= wordcounttab(bx) (number of lines *40 (words))
; rep stosw (40*cx)
; ret

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
	mov		ds,bp			; restore ds

	ret

FillBlock:
	inc		bl
	and		bl,255-1
	add		bp,bp

	mov		di,[BackBufferTable+bp]
	mov		cx,[FillBlockTab+bx]
	rep		stosw

	mov		di,[BackBufferTable+2+bp]
	mov		cx,[FillBlockTab+bx]
	rep		stosw

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

section .data align=8 ; 16 ; 8 ; 16
	
FillCharLeftTab:
%rep    80
	db	10101010b,01101010b,01011010b,01010110b
%endrep


FillCharRightTab:
%rep    80
	db	01010101b,10010101b,10100101b,10101001b
%endrep

FillTabLeft:
	db	0,0,0,0
	db	1,1,1,1
	db	2,2,2,2
	db	3,3,3,3
	db	4,4,4,4
	db	5,5,5,5
	db	6,6,6,6
	db	7,7,7,7
	db	8,8,8,8
	db	9,9,9,9
	db	10,10,10,10
	db	11,11,11,11
	db	12,12,12,12
	db	13,13,13,13
	db	14,14,14,14
	db	15,15,15,15
	db	16,16,16,16
	db	17,17,17,17
	db	18,18,18,18
	db	19,19,19,19
	db	20,20,20,20
	db	21,21,21,21
	db	22,22,22,22
	db	23,23,23,23
	db	24,24,24,24
	db	25,25,25,25
	db	26,26,26,26
	db	27,27,27,27
	db	28,28,28,28
	db	29,29,29,29
	db	30,30,30,30
	db	31,31,31,31
	db	32,32,32,32
	db	33,33,33,33
	db	34,34,34,34
	db	35,35,35,35
	db	36,36,36,36
	db	37,37,37,37
	db	38,38,38,38
	db	39,39,39,39
	db	40,40,40,40
	db	41,41,41,41
	db	42,42,42,42
	db	43,43,43,43
	db	44,44,44,44
	db	45,45,45,45
	db	46,46,46,46
	db	47,47,47,47
	db	48,48,48,48
	db	49,49,49,49
	db	50,50,50,50
	db	51,51,51,51
	db	52,52,52,52
	db	53,53,53,53
	db	54,54,54,54
	db	55,55,55,55
	db	56,56,56,56
	db	57,57,57,57
	db	58,58,58,58
	db	59,59,59,59
	db	60,60,60,60
	db	61,61,61,61
	db	62,62,62,62
	db	63,63,63,63
	db	64,64,64,64
	db	65,65,65,65
	db	66,66,66,66
	db	67,67,67,67
	db	68,68,68,68
	db	69,69,69,69
	db	70,70,70,70
	db	71,71,71,71
	db	72,72,72,72
	db	73,73,73,73
	db	74,74,74,74
	db	75,75,75,75
	db	76,76,76,76
	db	77,77,77,77
	db	78,78,78,78
	db	79,79,79,79

FillTabRight:
	db	79,79,79,79,
	db	78,78,78,78,
	db	77,77,77,77,
	db	76,76,76,76,
	db	75,75,75,75,
	db	74,74,74,74,
	db	73,73,73,73,
	db	72,72,72,72,
	db	71,71,71,71,
	db	70,70,70,70,
	db	69,69,69,69,
	db	68,68,68,68,
	db	67,67,67,67,
	db	66,66,66,66,
	db	65,65,65,65,
	db	64,64,64,64,
	db	63,63,63,63,
	db	62,62,62,62,
	db	61,61,61,61,
	db	60,60,60,60,
	db	59,59,59,59,
	db	58,58,58,58,
	db	57,57,57,57,
	db	56,56,56,56,
	db	55,55,55,55,
	db	54,54,54,54,
	db	53,53,53,53,
	db	52,52,52,52,
	db	51,51,51,51,
	db	50,50,50,50,
	db	49,49,49,49,
	db	48,48,48,48,
	db	47,47,47,47,
	db	46,46,46,46,
	db	45,45,45,45,
	db	44,44,44,44,
	db	43,43,43,43,
	db	42,42,42,42,
	db	41,41,41,41,
	db	40,40,40,40,
	db	39,39,39,39,
	db	38,38,38,38,
	db	37,37,37,37,
	db	36,36,36,36,
	db	35,35,35,35,
	db	34,34,34,34,
	db	33,33,33,33,
	db	32,32,32,32,
	db	31,31,31,31,
	db	30,30,30,30,
	db	29,29,29,29,
	db	28,28,28,28,
	db	27,27,27,27,
	db	26,26,26,26,
	db	25,25,25,25,
	db	24,24,24,24,
	db	23,23,23,23,
	db	22,22,22,22,
	db	21,21,21,21,
	db	20,20,20,20,
	db	19,19,19,19,
	db	18,18,18,18,
	db	17,17,17,17,
	db	16,16,16,16,
	db	15,15,15,15,
	db	14,14,14,14,
	db	13,13,13,13,
	db	12,12,12,12,
	db	11,11,11,11,
	db	10,10,10,10,
	db	9,9,9,9
	db	8,8,8,8
	db	7,7,7,7
	db	6,6,6,6
	db	5,5,5,5
	db	4,4,4,4
	db	3,3,3,3
	db	2,2,2,2
	db	1,1,1,1
	db	0,0,0,0

; if < 256 ver = 100 + (pitch &255)
; else ver = 100 - ( 255 - (pitch &255) )

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

FillBlockTab:
	dw	0*40,1*40,2*40,3*40,4*40,5*40,6*40,7*40,8*40,9*40,
	dw	10*40,11*40,12*40,13*40,14*40,15*40,16*40,17*40,18*40,19*40,
	dw	20*40,21*40,22*40,23*40,24*40,25*40,26*40,27*40,28*40,29*40,
	dw	30*40,31*40,32*40,33*40,34*40,35*40,36*40,37*40,38*40,39*40,
	dw	40*40,41*40,42*40,43*40,44*40,45*40,46*40,47*40,48*40,49*40,
	dw	50*40,51*40,52*40,53*40,54*40,55*40,56*40,57*40,58*40,59*40,
	dw	60*40,61*40,62*40,63*40,64*40,65*40,66*40,67*40,68*40,69*40,
	dw	70*40,71*40,72*40,73*40,74*40,75*40,76*40,77*40,78*40,79*40,
	dw	80*40,81*40,82*40,83*40,84*40,85*40,86*40,87*40,88*40,89*40,
	dw	90*40,91*40,92*40,93*40,94*40,95*40,96*40,97*40,98*40,99*40,

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

;Test0: 	resw	1
;Test1: 	resw	1
;Test2: 	resw	1
;Test3: 	resw	1
;Test4: 	resw	1
;Test5: 	resw	1
;Test6: 	resw	1
;Test7: 	resw	1
;Test8: 	resw	1
Test9: 	resw	1
Test10: resw	1

;Test11: resw	1

HDiff:	resw	1
VDiff:	resw	1
Slope: 	resw	1

HorDir:	resw	1

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer

Exit:	resb	1


