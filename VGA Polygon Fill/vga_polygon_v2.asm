; nasm vga3d_v16.asm -o vga3d.com -f bin

VIDEO_SEGMENT	equ	0A000h 	; display memory segment for true CGA graphics modes

SCREEN_WIDTH	equ	320

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
KEY_ESC	EQU 	27

CPU 386
bits 16
org 100h

section .text

start:
	mov	ax,13h 	; VGA 320 x 200 256 colors
	int	10h

	mov	ah, 4ah
	mov	bx, 1000h
	int	21h
	mov	ah, 48h
	mov	bx, 1000h
	int	21h
	mov	[BackBufferSeg], ax
		
	mov	ah, 4ah
	mov	bx, 1000h
	int	21h
	mov	ah, 48h
	mov	bx, 1000h
	int	21h
	mov	[ScreenBufferSeg], ax

	xor 	eax,eax 	; mov		ax,00101101001011010b ;

	mov		ax,[BackBufferSeg]		; es points to ds
	mov		es,ax
	xor		di,di		; DS:SI points to even back buffer	
	mov		cx,16000
	rep		stosd		; clear odd back buffer
	
	mov		ax,[ScreenBufferSeg]		; es points to ds
	mov		es,ax
	xor		di,di		; DS:SI points to even back buffer	
	mov		cx,16000

	cld
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front

	mov	bx,[BackBufferSeg]
	mov	es,bx

	mov	al,20 ; 100
	mov	cx,3
	call	Fill

;	mov	bx,[PointList]
;	mov	di,[PointList+2]
;	mov	al,100
;	call	Plot
;	mov	bx,[PointList+4]
;	mov	di,[PointList+6]
;	mov	al,100
;	call	Plot
;	mov	bx,[PointList+8]
;	mov	di,[PointList+10]
;	mov	al,100
;	call	Plot

	mov	cx,3
	call	Move
	
		
		call	GetKey
	jnc		MainLoop

Done: ; Finished, clear key, reset screen mode and exit.
	mov	ax,3		; reset to text mode
	int		10h
	
	push	es
	mov	ax,[BackBufferSeg]
        mov	es, ax
        mov	ah, 49h
        int	21h
	pop	es

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
	clc
	ret
.key_2:
	clc
	ret
.key_3:
	clc
	ret
.key_4:
	clc
	ret
.key_5:
	clc
	ret
.key_6:
	clc
	ret
	
CopyClearBackBuffer:
	mov	bp,ds			; backup ds

	mov	dx,[BackBufferSeg]	; DS:SI points to back buffer memory.
	mov	ds,dx
	xor	esi,esi

	mov	ax,VIDEO_SEGMENT 		; ES:DI points to CGA memory.
	mov	es,ax
	xor	edi,edi

	mov	ecx,16000
	cld
	rep	movsd			; copy from ds:si to es:di
	
	mov	ds,bp			; restore ds

	mov	es,dx			; ES:DI points to back buffer memory
	xor	di,di

	xor	eax,eax
	mov	cx,16000
	rep	stosd			; write eax to es:di

	ret

Plot:	add	bx,bx 			; start ver *2
	add	di,[BackBufferTable+bx]
	mov	[es:di],al
	ret

Fill:		mov		[Colour],al		; backup colour
	
			lea		si,[PointList]	; point list
			mov		bx,cx			; points
			shl		bx,2			; copy first point to last point
			mov		ebp,[si]		; get start xy words
			mov		[si+bx],ebp		; put xy words to end

			mov		bx,000ffh		; set lowest and highest

.point_loop		push	cx

				mov		ax,[si]		; vstart
				mov		bp,[si+2]	; hstart
				mov		cx,[si+4]	; vend
				mov		dx,[si+6]	; hend

				lea		di,[EdgeTable]	; get edge table start - default left edge
				cmp 	ax,cx 			; compare vstart vend
				jz 		.next 			; if same ignore
				jc		.noswap
					xchg 	ax,cx 		; swap vstart vend
					xchg 	bp,dx 		; swap hstart hend
					add		di,4		; add 4 for right edge

.noswap:		cmp 	bl,al 	; smallest xpos
				jc 		.low
					mov 	bl,al 	; yes

.low:			cmp 	bh,cl	; biggest xpos
				jnc 	.high
					mov		bh,cl 	; yes

.high			sub		cx,ax		; vdiff

				shl		ax,3		; *8
				add		di,ax		; added to di
	
				sub		dx,bp		; hdiff

.positive		shl		ebp,16		; hstart *65536
				movsx	eax,dx		; hdiff
				movsx	ecx,cx
				CDQ
				shl		eax,9		; shift left ax 9 times
				idiv	ecx			; divide dx:ax by cx
				shl		eax,7		; shift result left 7 times so becomes 16:16

.edge_loop			mov		[di],ebp		; store hstart = temp
					add		ebp,eax
					add		di,8
					dec		cl
					jnz		.edge_loop
						
.next			add		si,4
				pop		cx
				dec		cl
				jnz		.point_loop

			sub		bh,bl			; end - start
			jc		.exit			; all on same line?
			mov		dl,bh
			xor		bh,bh

			mov		al,[Colour]
			mov 	ah,al
			shl 	eax,8
			mov 	al,ah
			shl 	eax,8
			mov 	al,ah

			add		bx,bx 				; start ver *2
			mov		si,[BackBufferTable+bx]	; start of ver line
			shl		bx,2					; start ver *4*8
			lea		bp,[EdgeTable+bx+2]		; point to first 

.loop			mov		cx,[bp]
				mov		di,[bp+4]

				cmp		cx,di
				jnc		.no_swap
						xchg 	cx, di
.no_swap		sub		cx,di		; subtract start from end
				add		di,si		; add start screen pos to di

				mov 	bx,cx 		; start
				and 	cx,3
				rep 	stosb
				mov 	cx,bx
				shr 	cx,2
				rep 	stosd

				add		bp,8
				add		si,320
				dec		dl
				jnz		.loop

.exit		ret
	
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
	lea	di,[DirList]
 
.loop		mov	ax,[si]
		mov	bx,[si+2]
		mov	dx,[di]
		mov	bp,[di+2]
		add	ax,dx
		add	bx,bp
		mov	[si],ax
		mov	[si+2],bx

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
		mov	[di+2],bp
 
		add	si,4
		add	di,4
		dec	cx
		jne	.loop
	ret

section .data 
align	2 ; 16 ; 8 ; 16
	
BackBufferTable:
		dw SCREEN_WIDTH*000,SCREEN_WIDTH*001,SCREEN_WIDTH*002,SCREEN_WIDTH*003,SCREEN_WIDTH*004,SCREEN_WIDTH*005,SCREEN_WIDTH*006,SCREEN_WIDTH*007,SCREEN_WIDTH*008,SCREEN_WIDTH*009
		dw SCREEN_WIDTH*010,SCREEN_WIDTH*011,SCREEN_WIDTH*012,SCREEN_WIDTH*013,SCREEN_WIDTH*014,SCREEN_WIDTH*015,SCREEN_WIDTH*016,SCREEN_WIDTH*017,SCREEN_WIDTH*018,SCREEN_WIDTH*019
		dw SCREEN_WIDTH*020,SCREEN_WIDTH*021,SCREEN_WIDTH*022,SCREEN_WIDTH*023,SCREEN_WIDTH*024,SCREEN_WIDTH*025,SCREEN_WIDTH*026,SCREEN_WIDTH*027,SCREEN_WIDTH*028,SCREEN_WIDTH*029
		dw SCREEN_WIDTH*030,SCREEN_WIDTH*031,SCREEN_WIDTH*032,SCREEN_WIDTH*033,SCREEN_WIDTH*034,SCREEN_WIDTH*035,SCREEN_WIDTH*036,SCREEN_WIDTH*037,SCREEN_WIDTH*038,SCREEN_WIDTH*039
		dw SCREEN_WIDTH*040,SCREEN_WIDTH*041,SCREEN_WIDTH*042,SCREEN_WIDTH*043,SCREEN_WIDTH*044,SCREEN_WIDTH*045,SCREEN_WIDTH*046,SCREEN_WIDTH*047,SCREEN_WIDTH*048,SCREEN_WIDTH*049
		dw SCREEN_WIDTH*050,SCREEN_WIDTH*051,SCREEN_WIDTH*052,SCREEN_WIDTH*053,SCREEN_WIDTH*054,SCREEN_WIDTH*055,SCREEN_WIDTH*056,SCREEN_WIDTH*057,SCREEN_WIDTH*058,SCREEN_WIDTH*059
		dw SCREEN_WIDTH*060,SCREEN_WIDTH*061,SCREEN_WIDTH*062,SCREEN_WIDTH*063,SCREEN_WIDTH*064,SCREEN_WIDTH*065,SCREEN_WIDTH*066,SCREEN_WIDTH*067,SCREEN_WIDTH*068,SCREEN_WIDTH*069
		dw SCREEN_WIDTH*070,SCREEN_WIDTH*071,SCREEN_WIDTH*072,SCREEN_WIDTH*073,SCREEN_WIDTH*074,SCREEN_WIDTH*075,SCREEN_WIDTH*076,SCREEN_WIDTH*077,SCREEN_WIDTH*078,SCREEN_WIDTH*079
		dw SCREEN_WIDTH*080,SCREEN_WIDTH*081,SCREEN_WIDTH*082,SCREEN_WIDTH*083,SCREEN_WIDTH*084,SCREEN_WIDTH*085,SCREEN_WIDTH*086,SCREEN_WIDTH*087,SCREEN_WIDTH*088,SCREEN_WIDTH*089
		dw SCREEN_WIDTH*090,SCREEN_WIDTH*091,SCREEN_WIDTH*092,SCREEN_WIDTH*093,SCREEN_WIDTH*094,SCREEN_WIDTH*095,SCREEN_WIDTH*096,SCREEN_WIDTH*097,SCREEN_WIDTH*098,SCREEN_WIDTH*099
		dw SCREEN_WIDTH*100,SCREEN_WIDTH*101,SCREEN_WIDTH*102,SCREEN_WIDTH*103,SCREEN_WIDTH*104,SCREEN_WIDTH*105,SCREEN_WIDTH*106,SCREEN_WIDTH*107,SCREEN_WIDTH*108,SCREEN_WIDTH*109
		dw SCREEN_WIDTH*110,SCREEN_WIDTH*111,SCREEN_WIDTH*112,SCREEN_WIDTH*113,SCREEN_WIDTH*114,SCREEN_WIDTH*115,SCREEN_WIDTH*116,SCREEN_WIDTH*117,SCREEN_WIDTH*118,SCREEN_WIDTH*119
		dw SCREEN_WIDTH*120,SCREEN_WIDTH*121,SCREEN_WIDTH*122,SCREEN_WIDTH*123,SCREEN_WIDTH*124,SCREEN_WIDTH*125,SCREEN_WIDTH*126,SCREEN_WIDTH*127,SCREEN_WIDTH*128,SCREEN_WIDTH*129
		dw SCREEN_WIDTH*130,SCREEN_WIDTH*131,SCREEN_WIDTH*132,SCREEN_WIDTH*133,SCREEN_WIDTH*134,SCREEN_WIDTH*135,SCREEN_WIDTH*136,SCREEN_WIDTH*137,SCREEN_WIDTH*138,SCREEN_WIDTH*139
		dw SCREEN_WIDTH*140,SCREEN_WIDTH*141,SCREEN_WIDTH*142,SCREEN_WIDTH*143,SCREEN_WIDTH*144,SCREEN_WIDTH*145,SCREEN_WIDTH*146,SCREEN_WIDTH*147,SCREEN_WIDTH*148,SCREEN_WIDTH*149
		dw SCREEN_WIDTH*150,SCREEN_WIDTH*151,SCREEN_WIDTH*152,SCREEN_WIDTH*153,SCREEN_WIDTH*154,SCREEN_WIDTH*155,SCREEN_WIDTH*156,SCREEN_WIDTH*157,SCREEN_WIDTH*158,SCREEN_WIDTH*159
		dw SCREEN_WIDTH*160,SCREEN_WIDTH*161,SCREEN_WIDTH*162,SCREEN_WIDTH*163,SCREEN_WIDTH*164,SCREEN_WIDTH*165,SCREEN_WIDTH*166,SCREEN_WIDTH*167,SCREEN_WIDTH*168,SCREEN_WIDTH*169
		dw SCREEN_WIDTH*170,SCREEN_WIDTH*171,SCREEN_WIDTH*172,SCREEN_WIDTH*173,SCREEN_WIDTH*174,SCREEN_WIDTH*175,SCREEN_WIDTH*176,SCREEN_WIDTH*177,SCREEN_WIDTH*178,SCREEN_WIDTH*179
		dw SCREEN_WIDTH*180,SCREEN_WIDTH*181,SCREEN_WIDTH*182,SCREEN_WIDTH*183,SCREEN_WIDTH*184,SCREEN_WIDTH*185,SCREEN_WIDTH*186,SCREEN_WIDTH*187,SCREEN_WIDTH*188,SCREEN_WIDTH*189
		dw SCREEN_WIDTH*190,SCREEN_WIDTH*191,SCREEN_WIDTH*192,SCREEN_WIDTH*193,SCREEN_WIDTH*194,SCREEN_WIDTH*195,SCREEN_WIDTH*196,SCREEN_WIDTH*197,SCREEN_WIDTH*198,SCREEN_WIDTH*199

; BackBufferSeg	dw	1	; pointer to the segment containing the back buffer
align	4
PointList

 dw 0b8h, 13ch, 22h, 0ebh, 0c2h, 016h
 
; dw 0a1h, 12bh, 039h, 0d4h, 0b5h, 01h
 
; dw	056h,0e0h 
; dw	084h,089h 
; dw	06ah,04ch 

; dw 0bah, 03ah,120h
; dw 0edh,0c0h
; dw 018h,0bah

; dw 095h, 011fh 
; dw 045h, 0c8h
; dw 0a9h, 00dh
 
;		dw	10,10
;		dw	190,90
;		dw	30,310
		dw	0,0
		dw	0,0
		dw	0,0
		dw	0,0
		dw	0,0
;		resw	256

DirList
 dw 1,-1,-1,1,-1,1
 
; dw 1,1,-1,1,1,-1
 
; dw 1,-1
; dw -1,1
; dw -1,1
 
 dw	1,1
 dw	-1,1
 dw	1,-1
 dw 	+1,-1,+1,-1,+1,-1,+1,-1,+1,-1,+1,-1
		
section .bss 	; put uninitialized data here
align	4
EdgeTable	resd	800
BackBufferSeg	resw	1	; pointer to the segment containing the back buffer
ScreenBufferSeg	resw	1	; pointer to the segment containing the screen buffer thats a copy of the current screen

Colour		resb	1

; Interrupt 21H Service 72 : Allocate Memory
; Allocates a specified number of memory paragraphs.

; Input:
; AH = 48h
; BX = Number of memory paragraphs to be allocated

; Output:
; AX = Segment address of allocated memory
;        Error code if CF is set
;        7 : Memory control blocks destroyed
;        8: Insufficient memory
; BX = Size of largest available block (CF set)

; Call Function 59h for extended error code information (DOS 3.0 and above).

; Function 48h dynamically allocates memory, in multiples of 16 bytes (one paragraph). The amount of memory to be allocated, in paragraphs, is specified in BX. If the function is successful, AX:0000 points to the allocated memory block. (AX holds the segment address; the offset is always 0000).
