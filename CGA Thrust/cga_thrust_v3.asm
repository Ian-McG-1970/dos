
; nasm cga_line_v7.asm -o cgaline.com -f bin


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
RIGHT_CC	equ	8
BEHIND_CC	equ	16
BACK_CC		equ 16
FRONT_CC 	equ 32

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

MIDDLE_H EQU MIDDLE_X

MAP_WIDTH EQU 256

CPU 486
bits 16
org 100h


;%macro fill_cont	0
;	pop		di
;	pop 	bp
;	
;	ror 	eax,2
;	
;	add		bp,8
;	add 	di,2
;	dec		dl
;	jnz		Fill.plotloop
;%endmacro

section .text

start:
	call	BuildScreenTable
	mov		ax,04h 	; CGA 320 x 200 4 colors
	int		10h

    mov ah, 0Bh
    mov bx, 0101h       ; palette (cyan, magenta, white)
;    int 10h
    xor bx, bx          ; low intensity
;    int 10h

    mov ah, 0Bh
    mov bx, 0100h       ; palette (red, green, brown)
    int 10h
    xor bx, bx          ; low intensity
    int 10h
		
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

						mov		di,[BackBufferSeg]		; es points to ds
						mov		es,di
						xor		edi,edi					; DS:SI points to even back buffer	
						mov		cx,2000
						rep		stosd					; clear odd back buffer
						mov		di,8192					; DS:SI points to odd back buffer	
						mov		cx,2000
						rep		stosd					; clear odd back buffer

						mov		di,[ScreenBufferSeg]	; es points to ds
						mov		es,di
						xor		di,di					; DS:SI points to even back buffer	
						mov		cx,2000
						rep		stosd					; clear odd back buffer
						mov		di,8192					; DS:SI points to odd back buffer	
						mov		cx,2000
						rep		stosd					; clear odd back buffer

	xor ax,ax
	mov	[Exit],al

;	mov	[Test0],ax
	
MainLoop:
		call	WaitVSync 				; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBufferV9 	; copy back to front

	mov		bl,8
	mov		ax,[testx]
	call	Hex16
	mov		bl,9
	mov		ax,[testy]
	call	Hex16
	mov		bl,10
	mov		ax,[testz]
	call	Hex16

	call	Draw_Screen_Tiles_V2
;		mov		bl,22
;		mov		ax,[Test0]
;		call	Hex16
;		mov		bl,23
;		mov		ax,[Test1]
;		call	Hex16
;		mov		bl,24
;		mov		ax,[Test2]
;		call	Hex16
;		mov		bl,25
;		mov		ax,[Test3]
;		call	Hex16
;		mov		bl,26
;		mov		ax,[Test4]
;		call	Hex16
;		mov		bl,27
;		mov		ax,[Test5]
;		call	Hex16
;		mov		bl,28
;		mov		ax,[Test6]
;		call	Hex16
;		mov		bl,29
;		mov		ax,[Test7]
;		call	Hex16
;		mov		bl,30
;		mov		ax,[Test8]
;		call	Hex16
		mov		bl,31
		mov		ax,[Test9]
		call	Hex16
		
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
					mov		cx,100
.loop:					mov		[di],bp
						add		bp,8192
						mov		[di+2],bp
						sub		bp,8192-80
						add		di,4
						loop	.loop
					ret

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
;	mov ax,[HOffset]
;	sub ax,1
;	mov [HOffset],ax
;	mov ax,[testx]
;	dec ax
;	and ax,511
;	mov [testx],ax
	ret
.key_2:
;	mov ax,[HOffset]
;	add ax,1
;	mov [HOffset],ax
;	mov ax,[testx]
;	inc ax
;	and ax,511
;	mov [testx],ax
	ret
.key_3:
;	mov ax,[VOffset]
;	sub ax,1
;	mov [VOffset],ax
;	mov ax,[testy]
;	dec ax
;	and ax,511
;	mov [testy],ax
	ret
.key_4:
;	mov ax,[VOffset]
;	add ax,1
;	mov [VOffset],ax
;	mov ax,[testy]
;	inc ax
;	and ax,511
;	mov [testy],ax
	ret
.key_5:
;	mov ax,[ZOffset]
;	sub ax,1
;	mov [ZOffset],ax
;	mov ax,[testz]
;	dec ax
;	and ax,511
;	mov [testz],ax
	ret
.key_6:
;	mov ax,[ZOffset]
;	add ax,1
;	mov [ZOffset],ax
;	mov ax,[testz]
;	inc ax
;	and ax,511
;	mov [testz],ax
	ret

WaitVSync: 			mov		dx,INPUT_STATUS_1	; Wait for the leading edge of vertical sync pulse.
WaitNotVSyncLoop:		in		al,dx
						and		al,VSYNC_MASK
						jnz		WaitNotVSyncLoop
;WaitVSyncLoop:			in		al,dx
;						and		al,VSYNC_MASK
;						jz		WaitVSyncLoop
					ret
		
CopyClearBackBuffer:	cld

						mov		bp,ds			; backup ds

						mov		dx,[BackBufferSeg]
						mov		ds,dx

						mov		ax,VIDEO_SEGMENT
						mov		es,ax

						xor		esi,esi		; DS:SI points to even back buffer
						mov		edi,esi		; ES:DI points to CGA memory.

						mov		ecx,2000
						rep		movsd		; copy from ds:si to es:di

						mov		si,8192		; DS:SI points to odd back buffer
						mov		di,si		; ES:DI points to CGA memory.

						mov		cx,2000
						rep		movsd		; copy from ds:si to es:di

						mov		ax,ds		; es points to ds
						mov		es,ax

						xor 	eax,eax 	; mov		ax,00101101001011010b ;
	
						xor		di,di		; DS:SI points to even back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						mov		di,8192		; DS:SI points to odd back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						mov		ds,bp		; restore ds

;						mov		dx,[BackBufferSeg]
;						mov		ax,[ScreenBufferSeg]
;						mov		[BackBufferSeg],ax
;						mov		[ScreenBufferSeg],dx

						mov		bx,[BackBufferSeg]
						mov		es,bx

						ret

CopyClearBackBufferV8:	cld
						
						mov		bp,ds			; backup ds

						mov		dx,[ScreenBufferSeg]
						mov		es,dx

						mov		dx,[BackBufferSeg]
						mov		ds,dx

						mov		ax,VIDEO_SEGMENT
						mov		fs,ax

						xor		esi,esi
						mov		edi,esi
						
;	mov	bx,si
;	mov	dx,si

						mov		ecx,2001
.loop1:						repe	cmpsd
							jcxz	.exit1
								mov		eax,[ds:si-4] ;-4
								mov		[fs:di-4],eax ;-4
								mov		[es:di-4],eax ;-4
;	inc	bx
							jmp		.loop1
.exit1:

						mov		si,8192
						mov		di,si
						mov		cx,2001
.loop2:						repe	cmpsd
							jcxz	.exit2
								mov		eax,[ds:si-4] ;-4
								mov		[fs:di-4],eax ;-4
								mov		[es:di-4],eax ;-4
;	inc	dx
							jmp	.loop2
.exit2:


						mov		ds,bp		; restore ds

;	mov	[Test8],bx
;	mov	[Test9],dx

						mov		ax,[BackBufferSeg]		; es points to ds
						mov		es,ax

						xor 	eax,eax 	; mov		ax,00101101001011010b ;
	
						xor		di,di		; DS:SI points to even back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						mov		di,8192		; DS:SI points to odd back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						mov		ds,bp		; restore ds

						ret

CopyClearBackBufferV9:	cld
						
						mov		bp,ds			; backup ds

						mov		dx,[ScreenBufferSeg]
						mov		es,dx

						mov		dx,[BackBufferSeg]
						mov		ds,dx

						mov		ax,VIDEO_SEGMENT
						mov		fs,ax

						xor		esi,esi
						mov		edi,esi
						
;	mov	bx,si
;	mov	dx,si

						mov		ecx,2001
.loop1:						repe	cmpsd
							jcxz	.exit1
								mov		eax,[ds:si-4] ;-4
								mov		[fs:di-4],eax ;-4
								mov		[es:di-4],eax ;-4
;	inc	bx
							jmp		.loop1
.exit1:

						mov		ds,bp		; restore ds

;	mov	[Test8],bx
;	mov	[Test9],dx

						mov		ax,[BackBufferSeg]		; es points to ds
						mov		es,ax

						xor 	eax,eax 	; mov		ax,00101101001011010b ;
	
						xor		di,di		; DS:SI points to even back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						ret

Plot00:
	add		bx,bx 			; start ver *2
	mov		di,[BackBufferTable+bx]	; lookup start ver
	add		bp,bp			; start hor *2
	add		di,[HorPosTable+bp]	; add start hor
	mov		bl,[es:di]
	and		bl,[AndTable+bp]
	mov		[es:di],bl
	ret

Plot11:
	add		bx,bx 			; start ver *2
	mov		di,[BackBufferTable+bx]	; lookup start ver
	add		bp,bp			; start hor *2
	add		di,[HorPosTable+bp]	; add start hor
	mov		bl,[es:di]
	or		bl,[OrTable+bp] 
	mov		[es:di],bl
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

			lea		di,[si+1]	;mov		di,si
;			inc		di

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

; edi ; screen - could be push/pop to become spare?
; esi ; map pos - could be push/pop to become spare?
; ebp ; used - char pointer? - could be spare?
; eax ; used - holds 4 chars from map
; ebx ; used - char pointer?
; ecx ; counter - could be push/pop to become spare?
; edx ; used - holds 4 lines of char to be displayed

;   mov edx,[bp+bx]  ; could become mov edx,[gs:ebx], releaseing ebp?

; draw screen 4 chars at a time in a set of loops v1

; can 4 registers be spare and populated so that screens updates happen as 4 dwords instead of 16 bytes?

;  get 4 chars
;   for each get tile pos
;    get tile0[tile] dword put on screen
; ...
;    get tile7[tile] dword put on screen

Draw_Screen_Tiles_V2: ; 4x16

		xor		bh,bh
		xor		di,di

		mov 	ch,12

		lea		si,[Map1]
	
.vloop:		mov 	cl,5
.hloop:			lodsd 					; mov eax,[ds:si] ; get dword from map

				mov 	bl,al			; get first char
				mov 	edx,[Tile0+bx]	; lookup 4 bytes for char to be drawn on even lines only
				mov 	ebp,[Tile1+bx]
				mov 	[es:di+0+(80*0)],edx
				mov 	[es:di+0+(80*1)],ebp
				mov 	edx,[Tile2+bx]
				mov 	ebp,[Tile3+bx]
				mov 	[es:di+0+(80*2)],edx
				mov 	[es:di+0+(80*3)],ebp
				mov 	edx,[Tile4+bx]
				mov 	ebp,[Tile5+bx]
				mov 	[es:di+0+(80*4)],edx
				mov 	[es:di+0+(80*5)],ebp
				mov 	edx,[Tile6+bx]
				mov 	ebp,[Tile7+bx]
				mov 	[es:di+0+(80*6)],edx
				mov 	[es:di+0+(80*7)],ebp

				mov 	bl,ah			; get second char
				mov 	edx,[Tile0+bx]	; lookup 4 bytes for char to be drawn on even lines only
				mov 	ebp,[Tile1+bx]
				mov 	[es:di+4+(80*0)],edx
				mov 	[es:di+4+(80*1)],ebp
				mov 	edx,[Tile2+bx]
				mov 	ebp,[Tile3+bx]
				mov 	[es:di+4+(80*2)],edx
				mov 	[es:di+4+(80*3)],ebp
				mov 	edx,[Tile4+bx]
				mov 	ebp,[Tile5+bx]
				mov 	[es:di+4+(80*4)],edx
				mov 	[es:di+4+(80*5)],ebp
				mov 	edx,[Tile6+bx]
				mov 	ebp,[Tile7+bx]
				mov 	[es:di+4+(80*6)],edx
				mov 	[es:di+4+(80*7)],ebp

				bswap	eax

				mov 	bl,ah			; get third char
				mov 	edx,[Tile0+bx]	; lookup 4 bytes for char to be drawn on even lines only
				mov 	ebp,[Tile1+bx]
				mov 	[es:di+8+(80*0)],edx
				mov 	[es:di+8+(80*1)],ebp
				mov 	edx,[Tile2+bx]
				mov 	ebp,[Tile3+bx]
				mov 	[es:di+8+(80*2)],edx
				mov 	[es:di+8+(80*3)],ebp
				mov 	edx,[Tile4+bx]
				mov 	ebp,[Tile5+bx]
				mov 	[es:di+8+(80*4)],edx
				mov 	[es:di+8+(80*5)],ebp
				mov 	edx,[Tile6+bx]
				mov 	ebp,[Tile7+bx]
				mov 	[es:di+8+(80*6)],edx
				mov 	[es:di+8+(80*7)],ebp

				mov 	bl,al			; get fourth char
				mov 	edx,[Tile0+bx]	; lookup 4 bytes for char to be drawn on even lines only
				mov 	ebp,[Tile1+bx]
				mov 	[es:di+12+(80*0)],edx
				mov 	[es:di+12+(80*1)],ebp
				mov 	edx,[Tile2+bx]
				mov 	ebp,[Tile3+bx]
				mov 	[es:di+12+(80*2)],edx
				mov 	[es:di+12+(80*3)],ebp
				mov 	edx,[Tile4+bx]
				mov 	ebp,[Tile5+bx]
				mov 	[es:di+12+(80*4)],edx
				mov 	[es:di+12+(80*5)],ebp
				mov 	edx,[Tile6+bx]
				mov 	ebp,[Tile7+bx]
				mov 	[es:di+12+(80*6)],edx
				mov 	[es:di+12+(80*7)],ebp

				add 	di,16
				dec 	cl
				jnz 	.hloop
	
			add 	di,640-80 		; 3 lines down?
;			add 	si,MAP_WIDTH-80	; next map line down?
			dec 	ch
			jnz 	.vloop
		ret

section .data 
align 4

OrTable:	times 320/4		dw	192,48,12,3

AndTable:	times 320/4 	dw	255-192,255-48,255-12,255-3
	
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

Map1:	db 004,004,000,012, 000,004,008,012, 000,004,008,012, 000,000,000,000, 004,004,008,008
		db 004,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,008
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,012
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,004
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000
		db 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,004
		db 004,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,012
		db 008,012,000,000, 000,000,000,000, 000,000,000,000, 000,000,000,000, 000,000,012,012

testx dw 0
testy dw 0
testz dw 0

align 4
Tile0:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile1:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile2:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile3:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile4:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile5:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile6:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b
Tile7:	dd	011111111111111111111111111111111b,010101010101010101010101010101010b,001010101010101010101010101010101b,000000000000000000000000000000000b

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words

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

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer to be drawn on
ScreenBufferSeg	resw	1	; pointer to the segment containing the screen buffer thats a copy of the current screen

Exit:	resb	1

;00000000
;00000000
;00000000
;00000000
;00000000
;00000000
;00000000
;00000000

