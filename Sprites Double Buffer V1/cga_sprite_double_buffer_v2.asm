; nasm cga_line_v7.asm -o cgaline.com -f bin

; back_buffer - 16k - 16k screen containing background - to copy to screen_buffer - linear format
; screen_buffer - 16k - built up back_buffer and sprite - to copy to screen - linear format

; subroutine to copy vs/hs + ve/he - from back_buffer to screen_buffer - straight linear blit
; subroutine to draw vs/hs + ve/he - from sprite to screen_buffer - straight and/or linear copy
; subroutine to copy vs/hs + ve/he - from screen buffer to cga screen - copy from linear screen buffer to cga screen format (odd/even format)

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
	CLI			; disable interrupts
	call	BuildScreenTable
	mov	ax,04h 	; CGA 320 x 200 4 colors
	int		10h

;	mov	ax,0Bh 	; Pallette
;	mov	bh,1
;	mov	bl,0
;	mov	bx,00000h ; pallette 0 high
;	int	10h 

	mov		bx,1000h
	call	AllocMem
	mov		[BackBufferSeg],ax

	mov		bx,1000h
	call	AllocMem
	mov		[ScrnBufferSeg],ax
	
	xor ax,ax
	mov	[Exit],al
	
	cld
	
;	call	ResetBackBuffer

MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
;		call	CopyClearBackBuffer ; copy back to front
	
;		mov		bx,[BackBufferSeg]
;		mov		es,bx

		mov		ax,VIDEO_SEGMENT
		mov		es,ax
		
		mov		bl,0
		mov		ax,[XPos]
		call	Hex16
		mov		bl,1
		mov		ax,[YPos]
		call	Hex16


		mov		si,[XPos]	; ver pixel pos
		mov		di,[YPos]	; hor pixel pos
		mov		bx,5		; words copied
		mov		dl,9		; lines copied /2
		mov		cx,[ScrnBufferSeg]	; from back_buffer (es) ; are these source and dest segments the wrong around? 
		mov		es,cx
		mov		cx,[BackBufferSeg]	; to screen_buffer (ds) ; are these source and dest segments the wrong around? 
		call	Blitter

		mov		bp,[XPos]	; ver pixel pos
		mov		di,[YPos]	; hor pixel pos
		mov		cx,3		; bytes 
		mov		bh,4		; lines /2
		lea		si,[SpriteData]
		mov		ax,[ScrnBufferSeg]	; to screen_buffer (es)
		mov		es,ax
		mov		ax,ds				; from sprite_buffer (ds)
		call	Sprite

		mov		si,[XPos]	; ver pixel pos
		mov		di,[YPos]	; hor pixel pos
		mov		bx,5		; words copied
		mov		dl,9		; lines copied /2
		mov		cx,VIDEO_SEGMENT	; from back_buffer (es)
		mov		es,cx
		mov		cx,[ScrnBufferSeg]	; to screen_buffer (ds)
		call	Blitter
		
		call	GetKey
		
	mov		al,[Exit]
	or		al,al
	je		MainLoop

Done:	mov		ax,3	; reset to text mode
		int		10h
		mov		ah,4ch	; exit to DOS
		int		21h

AllocMem:	mov		ah,4ah	; allocate bx * 16
			push	bx
			int		21h
			mov		ah,48h
			pop		bx
			int		21h
			ret

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

; si=v pos
; di=h pos
; bx=h size in words
; dl=v size /2
; es=dest segement
; cx=source segment - to populate ds

; copy permanent background to temp background - ds = perm background + es = temp background
; copy temp background to screen - ds = temp background + es = screen

Blitter: ; blit from ds to es - either (back buffer (ds) to screen buffer (es) ) or (screen buffer (ds) to cga screen (es) )
;			mov		cl,2
;			shr		di,cl
;			shr		di,1
;			shr		di,1
;			dec		di

			add		si,si
			add		di,[BackBufferTable+si]			; di=di (screen hor char pos) + screen ver pos
			mov		ax,[BackBufferTableInc+si]		; either -8192 or 8192
			mov		bp,[BackBufferTableInc+2+si]	; either 8192 or -8192

	push ds
	mov	ds,cx
	
			mov		cx,bx
			add		cx,cx

			sub		ax,cx
			sub		bp,cx

.loop			mov		si,di
				mov		cx,bx
				rep		movsw
				add		di,ax

				mov		si,di
				mov		cx,bx
				rep		movsw
				add		di,bp

				dec		dl							; ver count
				jne		.loop

	pop	ds
			ret

; draw sprite on temp background - ds:si = sprite + es:di = temp background

; bp=v pos
; di=h pos
; cx=h size char pos
; bh=v size /2
; ds:si=sprite
; ax=source segment - to populate ds

Sprite: ; linear to linear
;			mov		cl,2
;			shr		di,cl
;			shr		di,1
;			shr		di,1

			add		bp,bp
			add		di,[BackBufferTable+bp]			; di=di (screen hor char pos) + screen ver pos
			mov		dx,[BackBufferTableInc+bp]		; either -8192 or 8192
			mov		bp,[BackBufferTableInc+2+bp]	; either 8192 or -8192

	push ds
	mov	ds,ax

			sub		dx,cx
			sub		bp,cx
			mov		bl,cl

.loop1:			mov		cl,bl

.loop2:				lodsw				; get ax from ds:si and inc si - al/ah = mask/sprite
					and		al,[es:di]	; get background
					or		al,ah
					stosb				; store al in es:di and inc di
					loop	.loop2

				add		di,dx
				mov		cl,bl

.loop3				lodsw				; get ax from ds:si and inc si - al/ah = mask/sprite
					and		al,[es:di]	; get background
					or		al,ah
					stosb				; store al in es:di and inc di
					loop	.loop3
					
				add		di,bp

				dec		bh							; ver count
				jne		.loop1

	pop	ds
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
	mov ax,[XPos]
	dec ax
	mov [XPos],ax
	ret
.key_2:
	mov ax,[XPos]
	inc ax
	mov [XPos],ax
	ret
.key_3:
	mov ax,[YPos]
	dec ax
	mov [YPos],ax
	ret
.key_4:
	mov ax,[YPos]
	inc ax
	mov [YPos],ax
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
		
ResetBackBuffer:
	mov		ax,[BackBufferSeg]
	mov		es,ax

	mov		ax,0101101011110000b ; 00101101001011010b ;
	
	xor		di,di		; DS:SI points to even back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		di,8192		; DS:SI points to odd back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

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

XPos:	dw		100
YPos:	dw		100
ZPos:	dw		100

Hex0	db 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1	db 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2	db 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3	db 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4	db 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b

SpriteData:
	dw	0000000011111111b,0000000011111111b,0000000011111111b
	dw	0011111111111111b,1111110011111111b,0000000011111111b
	dw	0010101011111111b,1010100011111111b,0000000011111111b
	dw	0011000011111111b,0000110011111111b,0000000011111111b
	dw	0011000011111111b,0000110011111111b,0000000011111111b
	dw	0001010111111111b,0101010011111111b,0000000011111111b
	dw	0011111111111111b,1111110011111111b,0000000011111111b
	dw	0000000011111111b,0000000011111111b,0000000011111111b

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words
BackBufferTableInc:	resw 202	; 200 screen line next line increments

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

ScrnBufferSeg	resw	1	; pointer to the segment containing the screen buffer
BackBufferSeg	resw	1	; pointer to the segment containing the back buffer

Exit:	resb	1

; work out the size of the block of data that needs to be moved
; add 1 to each side of the block - -1 from ver_start +1 to ver_end -1 from hor_start +1 to ver_end
; copy the block from back buffer to screen buffer - linear copy a word at a time - es=bb ds=sb ?
; draw the sprites on the screen buffer if they are within this square - linear copy and/or copy a byte at a time - es=sb ds=spr?
; copy the block from screen buffer to cga screen - non-linear copy a word at a time - es=cga ds=sb?
