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
 
KEY_1	EQU	0231h
KEY_2	EQU	0332h
KEY_3	EQU	0433h
KEY_4 	EQU	0534h
KEY_5 	EQU	0635h
KEY_6 	EQU	0736h
KEY_ESC	EQU 27

ODD_INC 	EQU 8192; -80; +80
EVEN_INC 	EQU -8192+80; +80; +80 ;-80

CPU 8086
bits 16
org 100h

section .text

start:
	call	BuildScreenTable
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

	xor ax,ax
	mov	[Exit],al
		
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		mov		bl,0
;		mov		ax,[XOffset]
		call	Hex16
		mov		bl,1
;		mov		ax,[YOffset]
		call	Hex16
		mov		bl,2
;		mov		ax,[ZOffset]
		call	Hex16

		mov		bl,4
;		mov		ax,[XAngle]
		call	Hex16
		mov		bl,5
;		mov		ax,[YAngle]
		call	Hex16
		mov		bl,6
;		mov		ax,[ZAngle]
		call	Hex16

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
;		mov		bl,14
;		mov		ax,[Test5]
;		call	Hex16
;		mov		bl,15
;		mov		ax,[Test6]
;		call	Hex16
;		mov		bl,16
;		mov		ax,[Test7]
;		call	Hex16
;		mov		bl,17
;		mov		ax,[Test8]
;		call	Hex16
;		mov		bl,18
;		mov		ax,[Test9]
;		call	Hex16

		mov		bp,27	; 7 lines down 
		mov		dx,30	; 30 bytes across
		mov		bx,5	; 3 words copied
		mov		al,18	; 10 lines copied
		call	SpriteV1

		mov		si,7	; 7 lines down 
		mov		di,20	; 30 bytes across
		mov		bx,5	; 3 words copied
		mov		dl,18	; 10 lines copied
		call	SpriteV2
		
		call	GetKey
		
	mov		al,[Exit]
	or		al,al
	je		MainLoop

Done:	mov		ax,3	; reset to text mode
		int		10h
		mov		ah,4ch	; exit to DOS
		int		21h

BuildScreenTable:	lea		di,[BackBufferTable]
					xor		bp,bp	; even lines

					lea		si,[BackBufferTableInc]
					mov 	bx,ODD_INC
					mov		dx,EVEN_INC

					mov		cx,100
.loop:					mov		[di],bp
						add		bp,bx	; ODD_INC
						mov		[di+2],bp
						sub		bp,8192-80

						mov		[ds:si],bx
						mov 	[ds:si+2],dx
						add		si,4

						add		di,4
						loop	.loop
					ret

;	mov		bp,7	; 1 lines down 
;	mov		dx,1	; 1 byte across
;	mov		bx,2	; 2 words copied
;	mov		al,10	; 10 lines copied

; bp = vpos
; dx = hpos
; bx = h word cnt
; al = v byte cnt
; stride - not needed in this example?

SpriteV0:		add		bp,bp
			mov		di,[BackBufferTable+bp]
			add		di,dx	

SpriteV0Loop:	mov		si,di
				mov		cx,bx
				rep		movsw

				add		bp,2
				mov		di,[BackBufferTable+bp]
				add		di,dx
				
				dec		al
				jne		SpriteV0Loop
			ret

; bp=v pos
; dx=h pos
; bx=h size
; al=v size

SpriteV1:	add		bp,bp
			add		bp,BackBufferTable
			mov		di,[bp]
			add		di,dx	

SpriteV1Loop:	mov		si,di
				mov		cx,bx
				rep		movsw

				add		bp,2
				mov		di,[bp]
				add		di,dx
				
				dec		al
				jne		SpriteV1Loop
			ret

; si=v pos
; di=h pos
; bx=h size
; dl=v size

SpriteV2:	mov		cx,bx
			add		cx,cx

			add		si,si
			mov		ax,[BackBufferTableInc+si]		; either -8192 or 8192
			mov		bp,[BackBufferTableInc+2+si]	; either 8192 or -8192
			add		di,[BackBufferTable+si]			; di=di (screen hor char pos) + screen ver pos

			sub		ax,cx
			sub		bp,cx

SpriteV2Loop:	mov		si,di
				mov		cx,bx
				rep		movsw

				add		di,ax
				xchg	ax,bp

				dec		dl							; ax = ver count
				jne		SpriteV2Loop
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
	ret
.key_esc:
	mov	al,1
	mov	[Exit],al
	ret
.key_1:
;	mov ax,[XOffset]
	sub ax,37
;	mov [XOffset],ax
	ret
.key_2:
;	mov ax,[XOffset]
	add ax,37
;	mov [XOffset],ax
	ret
.key_3:
;	mov ax,[YOffset]
	sub ax,37
;	mov [YOffset],ax
	ret
.key_4:
;	mov ax,[YOffset]
	add ax,37
;	mov [YOffset],ax
	ret
.key_5:
;	mov ax,[ZOffset]
	sub ax,37
;	mov [ZOffset],ax
	ret
.key_6:
;	mov ax,[ZOffset]
	add ax,37
;	mov [ZOffset],ax
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

	mov		ax,00101101001011010b ;
	
	xor		di,di		; DS:SI points to even back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		di,8192		; DS:SI points to odd back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		ds,bp			; restore ds

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

Hex0	db 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1	db 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2	db 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3	db 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4	db 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b
 	
%include  'sin256.inc'

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words
BackBufferTableInc:	resw 202	; 200 screen line next line increments
BackBuffer: 		resw 16384	; 2 screen buffers

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

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer

Exit:	resb	1
