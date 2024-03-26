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

CPU 386
bits 16
org 100h

; get screen								(ecx) = es:di							mov	ecx,[es:di]
; and with gap (left/right)					(ecx) = screen that is still needed		and ecx,[leftgaptab+bx]
; get colour								(esi) = eax								mov	esi,eax
; and with not_gap (left/right)				(esi) = colour that is still needed		and esi,[leftnongaptab+bx]
; or gap and not_gap						(ecx) = final result					or	ecx,esi
; put on screen								(ecx) = es:di							mov [es:di],ecx

%macro left_edge 0 
	shl		bx,2			; start hor *4
	mov		ecx,[es:di]					; get screen								(ecx) = es:di							mov	ecx,[es:di]
	and		ecx,[FillLeftTab32V2+bx]	; and with gap (left/right)					(ecx) = screen that is still needed		and ecx,[leftgaptab+bx]
	mov		esi,eax						; get colour								(esi) = eax								mov	esi,eax
	and		esi,[FillLeftTab32+bx]		; and with not_gap (left/right)				(esi) = colour that is still needed		and esi,[leftnongaptab+bx]
	or		ecx,esi						; or gap and not_gap						(ecx) = final result					or	ecx,esi
	mov		[es:di],ecx					; put on screen								(ecx) = es:di							mov [es:di],ecx
%endmacro

%macro right_edge 0 
	shl		bp,2			; start hor *4
	mov		ecx,[es:di]					; get screen								(ecx) = es:di							mov	ecx,[es:di]
	and		ecx,[FillRightTab32V2+bp]	; and with gap (left/right)					(ecx) = screen that is still needed		and ecx,[leftgaptab+bx]
	mov		esi,eax						; get colour								(esi) = eax								mov	esi,eax
	and		esi,[FillRightTab32+bp]		; and with not_gap (left/right)				(esi) = colour that is still needed		and esi,[leftnongaptab+bx]
	or		ecx,esi						; or gap and not_gap						(ecx) = final result					or	ecx,esi
	mov		[es:di],ecx					; put on screen								(ecx) = es:di							mov [es:di],ecx
%endmacro

%macro fill_cont	0
	pop		di
	pop 	bp
	
	ror 	eax,2
	
	add		bp,8
	add 	di,2
	dec		dl
	jnz		Fill.plotloop
%endmacro

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
		call	CopyClearBackBufferV8 	; copy back to front

	mov	cx,3
	call	Move
	
	mov	cx,3
	call	Plots

	mov		al,11011101b ;20 ; 100
	mov		cx,3
	lea		si,[PointList2]		; point list
	call	Fill

	mov		al,10011001b ;20 ; 100
	mov		cx,3
	lea		si,[PointList]		; point list
	call	Fill

	mov		al,10111011b ;20 ; 100
	mov		cx,3
	lea		si,[PointList3]
;	call	Fill


	mov		al,00010001b
	mov		cx,3
	lea		si,[PointList4]
	call	Fill

	mov		al,00000000b
	mov		cx,3
	lea		si,[PointList5]
	call	Fill

	mov		al,00100010b
	mov		cx,3
	lea		si,[PointList6]
	call	Fill

	mov		al,00110011b
	mov		cx,3
	lea		si,[PointList7]
	call	Fill

	mov		al,01000100b
	mov		cx,3
	lea		si,[PointList8]
	call	Fill

	mov		al,01010101b
	mov		cx,3
	lea		si,[PointList9]
	call	Fill

	mov		al,01100110b
	mov		cx,3
	lea		si,[PointList10]
	call	Fill

	mov		al,01110111b
	mov		cx,3
	lea		si,[PointList11]
	call	Fill

	mov		al,10001000b
	mov		cx,3
	lea		si,[PointList12]
	call	Fill

	mov		al,10011001b
	mov		cx,3
	lea		si,[PointList13]
	call	Fill

	mov		al,10101010b
	mov		cx,3
	lea		si,[PointList14]
	call	Fill

	mov		al,10111011b
	mov		cx,3
	lea		si,[PointList15]
	call	Fill

	mov		al,11001100b
	mov		cx,3
	lea		si,[PointList16]
	call	Fill

	mov		al,11011101b
	mov		cx,3
	lea		si,[PointList17]
	call	Fill

	mov		al,11101110b
	mov		cx,3
	lea		si,[PointList18]
	call	Fill

	mov		al,11111111b
	mov		cx,3
	lea		si,[PointList19]
	call	Fill

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

						mov		ecx,2000
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
						mov		cx,2000
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

Plots:	lea 	si,[PointList]
 
.loop		push	cx
			mov		bx,[si]
			mov		bp,[si+2]
			call 	Plot11
			pop		cx
 
			add	si,4
			dec	cl
			jne	.loop
		ret

Move:	lea 	si,[PointList]
		lea		di,[DirList]
 
.loop		mov		ax,[si]
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
			mov	[di+2],bp
 
			add		si,4
			add		di,4
			dec		cx
			jne		.loop
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

Fill:	mov		[Colour],al		; backup colour
	
;		lea		si,[PointList]		; point list
		mov		bx,cx			; points
		shl		bx,2			; copy first point to last point
		mov		ebp,[si]			; get start xy words
		mov		[si+bx],ebp		; put xy words to end

		mov		bx,000ffh		; set lowest and highest

.point_loop		push	cx

				mov		ax,[si]		; vstart
				mov		bp,[si+2]	; hstart
				mov		cx,[si+4]	; vend
				mov		dx,[si+6]	; hend

				lea		di,[EdgeTable]	; get edge table start - default left edge
				cmp 	ax,cx 		; compare vstart vend
				jz 		.next 		; if same ignore
				jc		.noswap
					xchg 	ax,cx 		; swap vstart vend
					xchg 	bp,dx 		; swap hstart hend
					add		di,4		; add 4 for right edge

.noswap:		cmp 	bl,al 	; smallest xpos
				jc 	.low
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

.edge_loop		mov		[di],ebp		; store hstart = temp
				add		ebp,eax
				add		di,8
				dec		cl
				jnz		.edge_loop

.next			add		si,4
				pop		cx
				dec		cl
				jnz		.point_loop
 
			sub		bh,bl			; end - start
			jc		FillExit		; all on same line?
	
			mov		dl,bh
			xor		bh,bh
	
			mov		al,[Colour]
			mov 	ah,al
			shl 	eax,8
			mov 	al,ah
			shl 	eax,8
			mov 	al,ah

			add		bx,bx 			; start ver *2
			mov		di,bx			; screen line
			shl		bx,2			; start ver *4*8
			lea		di,[BackBufferTable+di]
			lea		bp,[EdgeTable+bx+2]	; point to first 

.plotloop		push	bp
				push 	di

				mov		bx,[bp+4]
				mov		bp,[bp]
				mov		di,[di]	; lookup start ver

				cmp		bp,bx
				jnc		.no_swap
					xchg 	bp, bx

.no_swap:		mov 	cx,bx
				shr 	cx,4
				shl 	cx,2
				add 	di,cx

				mov		cx,bx
				mov		si,bp
			
				shr		si,4
				shr		cx,4

				sub		si,cx
				add		si,si
				mov		si,[FillTable+si]
				JMP		(si)

Fill000:	fill_cont
FillExit:	ret

Fill004:	left_edge
			add		di,4
			right_edge
			fill_cont
			ret

Fill008:	left_edge
			add		di,4
			stosd
			right_edge
			fill_cont
			ret

Fill012:	left_edge
			add		di,4
			mov		cx,2
			rep		stosd
			right_edge
			fill_cont
			ret

Fill016:	left_edge
			add		di,4
			mov		cx,3
			rep		stosd
			right_edge
			fill_cont
			ret

Fill020:left_edge
			add		di,4
			mov		cx,4
			rep		stosd
			right_edge
			fill_cont
			ret

Fill024:left_edge
			add		di,4
			mov		cx,5
			rep		stosd
			right_edge
			fill_cont
			ret

Fill028:left_edge
			add		di,4
			mov		cx,6
			rep		stosd
			right_edge
			fill_cont
			ret

Fill032:	left_edge
			add		di,4
			mov		cx,7
			rep		stosd
			right_edge
			fill_cont
			ret

Fill036:	left_edge
			add		di,4
			mov		cx,8
			rep		stosd
			right_edge
			fill_cont
			ret

Fill040:	left_edge
			add		di,4
			mov		cx,9
			rep		stosd
			right_edge
			fill_cont
			ret

Fill044:	left_edge
			add		di,4
			mov		cx,10
			rep		stosd
			right_edge
			fill_cont
			ret

Fill048:	left_edge
			add		di,4
			mov		cx,11
			rep		stosd
			right_edge
			fill_cont
			ret

Fill052:	left_edge
			add		di,4
			mov		cx,12
			rep		stosd
			right_edge
			fill_cont
			ret

Fill056:	left_edge
			add		di,4
			mov		cx,13
			rep		stosd
			right_edge
			fill_cont
			ret

Fill060:	left_edge
			add		di,4
			mov		cx,14
			rep		stosd
			right_edge
			fill_cont
			ret

Fill064:	left_edge
			add		di,4
			mov		cx,15
			rep		stosd
			right_edge
			fill_cont
			ret

Fill068:	left_edge
			add		di,4
			mov		cx,16
			rep		stosd
			right_edge
			fill_cont
			ret

Fill072:	left_edge
			add		di,4
			mov		cx,17
			rep		stosd
			right_edge
			fill_cont
			ret

Fill076:	left_edge
			add		di,4
			mov		cx,18
			rep		stosd
			right_edge
			fill_cont
			ret

Fill080:	left_edge
			add		di,4
			mov		cx,19
			rep		stosd
			right_edge
			fill_cont
			ret

Fill084:	left_edge
			add		di,4
			mov		cx,20
			rep		stosd
			right_edge
			fill_cont
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

section .data align=8 ; 16 ; 8 ; 16

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

HorPosTab32:
 times 16 dd 0*4
 times 16 dd 1*4
 times 16 dd 2*4
 times 16 dd 3*4
 times 16 dd 4*4
 times 16 dd 5*4
 times 16 dd 6*4
 times 16 dd 7*4
 times 16 dd 8*4
 times 16 dd 9*4
 times 16 dd 10*4
 times 16 dd 11*4
 times 16 dd 12*4
 times 16 dd 13*4
 times 16 dd 14*4
 times 16 dd 15*4
 times 16 dd 16*4
 times 16 dd 17*4
 times 16 dd 18*4
 times 16 dd 19*4

FillLeftTab32: times 320/16 	dd 	11111111111111111111111111111111b,11111111111111111111111100111111b,11111111111111111111111100001111b,11111111111111111111111100000011b,11111111111111111111111100000000b,11111111111111110011111100000000b,11111111111111110000111100000000b,11111111111111110000001100000000b,11111111111111110000000000000000b,11111111001111110000000000000000b,11111111000011110000000000000000b,11111111000000110000000000000000b,11111111000000000000000000000000b,00111111000000000000000000000000b,00001111000000000000000000000000b,00000011000000000000000000000000b
FillRightTab32: times 320/16 	dd 	00000000000000000000000011000000b,00000000000000000000000011110000b,00000000000000000000000011111100b,00000000000000000000000011111111b,00000000000000001100000011111111b,00000000000000001111000011111111b,00000000000000001111110011111111b,00000000000000001111111111111111b,00000000110000001111111111111111b,00000000111100001111111111111111b,00000000111111001111111111111111b,00000000111111111111111111111111b,11000000111111111111111111111111b,11110000111111111111111111111111b,11111100111111111111111111111111b,11111111111111111111111111111111b

FillLeftTab32V2: times 320/16	dd	00000000000000000000000000000000b,00000000000000000000000011000000b,00000000000000000000000011110000b,00000000000000000000000011111100b,00000000000000000000000011111111b,00000000000000001100000011111111b,00000000000000001111000011111111b,00000000000000001111110011111111b,00000000000000001111111111111111b,00000000110000001111111111111111b,00000000111100001111111111111111b,00000000111111001111111111111111b,00000000111111111111111111111111b,11000000111111111111111111111111b,11110000111111111111111111111111b,11111100111111111111111111111111b
FillRightTab32V2: times 320/16 	dd	11111111111111111111111100111111b,11111111111111111111111100001111b,11111111111111111111111100000011b,11111111111111111111111100000000b,11111111111111110011111100000000b,11111111111111110000111100000000b,11111111111111110000001100000000b,11111111111111110000000000000000b,11111111001111110000000000000000b,11111111000011110000000000000000b,11111111000000110000000000000000b,11111111000000000000000000000000b,00111111000000000000000000000000b,00001111000000000000000000000000b,00000011000000000000000000000000b,00000000000000000000000000000000b

FillTable:	dw	Fill000,Fill004,Fill008,Fill012,Fill016,Fill020,Fill024,Fill028,Fill032,Fill036,Fill040,Fill044,Fill048,Fill052,Fill056,Fill060,Fill064,Fill068,Fill072,Fill076,Fill080,Fill084
		
Hex0	db 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1	db 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2	db 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3	db 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4	db 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b

PointList: dw 0b8h, 13ch, 22h, 0ebh, 0c2h, 016h, 0, 0

PointList2: dw 0, 0, 7, 319, 199, 315, 0, 0

PointList3: dw 0,0, 199,319, 50,40, 0, 0 

PointList4: dw 5,10, 0,319, 10,159, 0,0
PointList5: dw 17,10, 12,319, 22,159, 0,0
PointList6: dw 29,10, 24,319, 34,159, 0,0
PointList7: dw 41,10, 36,319, 46,159, 0,0
PointList8: dw 53,10, 48,319, 58,159, 0,0
PointList9: dw 65,10, 60,319, 70,159, 0,0
PointList10: dw 77,10, 72,319, 82,159, 0,0
PointList11: dw 89,10, 84,319, 94,159, 0,0
PointList12: dw 101,10, 96,319, 106,159, 0,0
PointList13: dw 113,10, 108,319, 118,159, 0,0
PointList14: dw 125,10, 120,319, 130,159, 0,0
PointList15: dw 137,10, 132,319, 142,159, 0,0
PointList16: dw 149,10, 144,319, 154,159, 0,0
PointList17: dw 161,10, 156,319, 166,159, 0,0
PointList18: dw 173,10, 168,319, 178,159, 0,0
PointList19: dw 181,10, 180,319, 190,159, 0,0
 
DirList:	dw 1,-1,-1,1,-1,1 
 dw	1,1
 dw	-1,1
 dw	1,-1
 dw 	+1,-1,+1,-1,+1,-1,+1,-1,+1,-1,+1,-1

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words

EdgeTable	resd	800

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

Colour resb 1
Exit:	resb	1

; read through even dwords mov cx,2000 / xor si,si / mov di,si / fs = screen segment / ds = source screen segment / es = dest segment
; .loop comparigng source and dest - repe cmpsd
;  if different
;   copy source to dest - movsd
;   copy source to screen - mov [fs:si],eax
; loop .loop
