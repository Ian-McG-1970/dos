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
	mov	[XAngle],ax
	mov	[YAngle],ax		
	mov	[ZAngle],ax
	
;	mov	[Test0],ax
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		mov		bl,0
		mov		ax,[XOffset]
;		call	Hex16
		mov		bl,1
		mov		ax,[YOffset]
;		call	Hex16
		mov		bl,2
		mov		ax,[ZOffset]
;		call	Hex16

		mov		bl,4
		mov		ax,[XAngle]
;		call	Hex16
		mov		bl,5
		mov		ax,[YAngle]
;		call	Hex16
		mov		bl,6
		mov		ax,[ZAngle]
;		call	Hex16

	mov		bl,8
	mov		ax,[testx]
	call	Hex16
	mov		bl,9
	mov		ax,[testy]
	call	Hex16
	mov		bl,10
	mov		ax,[testz]
	call	Hex16

	mov		bx,[testz]
	mov		dx,[testx]
	mov		cx,[testy]
	call	PerspectiveZW

	xor		bh,bh
	push	bx
	mov		bl,12
;	call	Hex16
	
	pop		ax
	mov		bl,13
;	call	Hex8

	mov		bx,[testz]
	mov		dx,[testx]
	mov		cx,[testy]
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,510
	mov		dx,510
	mov		cx,1
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,510
	mov		dx,1
	mov		cx,1
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,510
	mov		dx,510
	mov		cx,510
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,510
	mov		dx,1
	mov		cx,510
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,1
	mov		dx,510
	mov		cx,1
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,1
	mov		dx,1
	mov		cx,1
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,1
	mov		dx,510
	mov		cx,510
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

	mov		bx,1
	mov		dx,1
	mov		cx,510
	call	PerspectiveZW
	mov		bp,cx
	call	Plot11

		mov	ax,[XAngle]
		add	ax,1
		and ax,03ffh
		mov	[XAngle],ax
		mov	ax,[YAngle]
		sub	ax,1
		and ax,03ffh
		mov	[YAngle],ax
		mov	ax,[ZAngle]
		add	ax,1
		and ax,03ffh
		mov	[ZAngle],ax
;		call	BuildRotateMatrix

;		mov word	[XOffset],0
;		mov word	[YOffset],0
;		mov word	[ZOffset],250
		mov	si,ObjectPoints00 ; points
		mov	di,ObjectLines00 ; lines
		mov	cl,8 ; 8 		; points
		mov	ch,12 ; 12	; lines
;		call 		DrawObject

;		mov word	[XOffset],200
;		mov word	[YOffset],200
;		mov word	[ZOffset],200
		mov	si,ObjectPoints00 ; points
		mov	di,ObjectLines00 ; lines
		mov	cl,8 ; 8 		; points
		mov	ch,12 ; 12	; lines
		call 		DrawObjectZW



		mov		bl,22
		mov		ax,[Test0]
		call	Hex16
		mov		bl,23
		mov		ax,[Test1]
		call	Hex16
		mov		bl,24
		mov		ax,[Test2]
		call	Hex16
		mov		bl,25
		mov		ax,[Test3]
		call	Hex16
		mov		bl,26
		mov		ax,[Test4]
		call	Hex16
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
;		mov		bl,31
;		mov		ax,[Test9]
;		call	Hex16

		mov		bl,20
		mov		ax,8192+80
;		call	Hex16
		mov		bl,21
		mov		ax,-8192+80
;		call	Hex16
	
		
;		call	Test
		
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
	mov ax,[HOffset]
	sub ax,1
	mov [HOffset],ax
	mov ax,[testx]
	dec ax
	and ax,511
	mov [testx],ax
	ret
.key_2:
	mov ax,[HOffset]
	add ax,1
	mov [HOffset],ax
	mov ax,[testx]
	inc ax
	and ax,511
	mov [testx],ax
	ret
.key_3:
	mov ax,[VOffset]
	sub ax,1
	mov [VOffset],ax
	mov ax,[testy]
	dec ax
	and ax,511
	mov [testy],ax
	ret
.key_4:
	mov ax,[VOffset]
	add ax,1
	mov [VOffset],ax
	mov ax,[testy]
	inc ax
	and ax,511
	mov [testy],ax
	ret
.key_5:
	mov ax,[ZOffset]
	sub ax,1
	mov [ZOffset],ax
	mov ax,[testz]
	dec ax
	and ax,511
	mov [testz],ax
	ret
.key_6:
	mov ax,[ZOffset]
	add ax,1
	mov [ZOffset],ax
	mov ax,[testz]
	inc ax
	and ax,511
	mov [testz],ax
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

	xor ax,ax ; mov		ax,00101101001011010b ;
	
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
	
DrawObject2:		push	di ; object lines
				push	cx ; number of lines

;				mov		di,ObjectPointsResults

				xor		ch,ch
				dec		cl
				add		cl,cl
				mov		di,cx	; point count
		
				mov		bx,255 ; high = or clip codes / low = and clip codes
	
.rotate_point_loop:	push	bx
					mov		cx,[ds:si]	; xpos
					mov		ax,[ds:si+2]	; ypos
					mov		bp,[ds:si+4]	; zpos
		
					call	Rotate

					mov		ax,cx
					mov		cx,bp
					mov		bp,bx
		
					add		ax,[YOffset]
					mov		[ObjectYRot+di],ax 		; yrot

					add		bp,[XOffset]
					mov		[ObjectXRot+di],bp 			; xrot

					add		cx,[ZOffset]
					mov		[ObjectZRot+di],cx 	; zrot

					jns		.point_in_front
						mov		dl,BEHIND_CC
						jmp		.point_behind	

.point_in_front:	call	Perspective
					mov		[ObjectYPos+di],ax 		; xpos
					mov		[ObjectXPos+di],bp 		; ypos
					call	ClassifyPoint

.point_behind:		mov		[ObjectClipCode+di],dl 	; clip code

					pop		bx
					and		bl,dl	; object and clip code
					or		bh,dl	; object or clip code
		
					add		si,6				
					sub		di,2
					jns		.rotate_point_loop

				pop		cx 	; number of lines
				pop 	si 	; object lines

				cmp		bl,0				; if and_object_clip_code ne 0 all off one side so exit
				jnz		.off_screen
 
				cmp		bh,0				; if or_object_clip_code eq 0 all on screen so no clip needed
				jnz		.draw_clipped_object

.draw_whole_object:
.draw_whole_object_line_loop:	push	cx
								push	si

								xor		bh,bh
								mov		bl,[ds:si]
								add		bl,bl	
								mov		cx,[ObjectYPos+bx] ; endy
								mov		dx,[ObjectXPos+bx] ; endx
 
								mov		bl,[ds:si+1]
								add		bl,bl
								mov		bp,[ObjectYPos+bx] ; starty
								mov		si,[ObjectXPos+bx] ; startx
								call	Line00
	 
								pop		si
								pop		cx
								add		si,2

								dec		ch
								jnz		.draw_whole_object_line_loop

.off_screen:				ret
	
.draw_clipped_object:	
.draw_clipped_object_line_loop:	xor		bh,bh
								push	cx
								push	si
		
								mov		bl,[ds:si]
								add		bl,bl
								mov		al,[ObjectClipCode+bx] 	; start clip code
								mov		bl,[ds:si+1]
								add		bl,bl
								mov		ah,[ObjectClipCode+bx] 	; end clip code

								mov		bl,al		; backup start clip code
								and		bl,ah	; both points are off 
								jnz		.line_off_screen	; then line is off screen

								or		al,ah 	; if any point is off
								jnz		.line_clip	; then line needs clipped
		
								mov		bl,[ds:si]
								add		bl,bl	
								mov		cx,[ObjectYPos+bx] ; starty
								mov		dx,[ObjectXPos+bx] ; startx

								mov		bl,[ds:si+1]
								add		bl,bl
								mov		bp,[ObjectYPos+bx] ; endy 
								mov		si,[ObjectXPos+bx] ; endx - todo - si is the problem?

.draw_clipped_line:				call	Line11

.line_off_screen:				pop		si
								pop		cx
								add		si,2
		
								dec		ch
								jnz		.draw_clipped_object_line_loop
							ret

.line_clip:	; al = point 1 clip ; ah = point 2 clip

; copy point details to 2 sets of coords temp coords zyx and temp perspective xy
; if either point is behind
;  do z clip on required point
;  do perspective
;  do rest of tests

					xor		bh,bh
					mov		bl,[ds:si]
					add		bl,bl	
					mov		dx,[ObjectXRot+bx]
					mov		[STX],dx
					mov		dx,[ObjectYRot+bx]
					mov		[STY],dx
					mov		dx,[ObjectZRot+bx]
					mov		[STZ],dx
					mov		dx,[ObjectXPos+bx]
					mov		[XST],dx
					mov		dx,[ObjectYPos+bx]	
					mov		[YST],dx	
					mov		al,[ObjectClipCode+bx]
	
					mov		bl,[ds:si+1]
					add		bl,bl
					mov		dx,[ObjectXRot+bx]
					mov		[ENX],dx
					mov		dx,[ObjectYRot+bx]
					mov		[ENY],dx
					mov		dx,[ObjectZRot+bx]
					mov		[ENZ],dx	
					mov		dx,[ObjectXPos+bx]
					mov		[XEN],dx
					mov		dx,[ObjectYPos+bx]
					mov		[YEN],dx
					mov		ah,[ObjectClipCode+bx]

					mov		[CCST],ax
	
.line_clip_loop:	mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side
	
					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line

.clip_behind_test:	test	bl,BEHIND_CC	; test if either point is off (al=al or ah)
					jz		.clip_left_test
					call	LineClipBehind

					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line
	
.clip_left_test:	test	bl,LEFT_CC
					jz		.clip_right_test
					call	LineClipLeft
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line
		
.clip_right_test:	test	bl,RIGHT_CC
					jz		.clip_top_test
					call	LineClipRight
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side
	
					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line
		

.clip_top_test:		test	bl,TOP_CC
					jz		.clip_bottom_test
					call	LineClipTop
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line

.clip_bottom_test:	test	bl,BOTTOM_CC
					jz		.line_clip_loop
					call	LineClipBottom
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

.clip_draw_test:	mov		cx,[YST]
					mov		dx,[XST]
					mov		bp,[YEN]
					mov		si,[XEN]
					jmp		.draw_clipped_line
	
LineClipBehind:	test	ah,BEHIND_CC
				jz		.behind_sback_efront
.behind_sfront_eback:	mov		ax,[STX]
						mov		cx,[STZ]
						mov		di,[ENZ]
						mov		bx,[ENX]
						call	ClipBehind
						push	di
	
						mov		ax,[STY]
						mov		cx,[STZ]
						mov		di,[ENZ]
						mov		bx,[ENY]
						call	ClipBehind

						mov		ax,di
						pop		bp

						xor		cx,cx
						call	Perspective
						mov		[YEN],ax 
						mov		[XEN],bp
						call	ClassifyPoint
						mov		[CCEN],dl
						ret
.behind_sback_efront:	mov		ax,[ENX]
						mov		cx,[ENZ]
						mov		di,[STZ]
						mov		bx,[STX]
						call	ClipBehind
						push	di
	
						mov		ax,[ENY]
						mov		cx,[ENZ]
						mov		di,[STZ]
						mov		bx,[STY]
						call	ClipBehind

						mov		ax,di
						pop		bp
		
						xor		cx,cx
						call	Perspective
						mov		[YST],ax 
						mov		[XST],bp 
						call	ClassifyPoint
						mov		[CCST],dl
						ret
	
LineClipTop:	test	ah,TOP_CC
				jz		.top_p1on_p2off
.top_p1off_p2on:	mov		ax,[YST]
					mov		cx,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]
					call	ClipTop

					mov		[YEN],di
					mov		bx,TOP_EDGE
					mov		[XEN],bx

					mov		ax,[YEN]
					mov		bp,[XEN]
					call	ClassifyPoint
					mov		[CCEN],dl
					ret
.top_p1on_p2off:	mov		ax,[YEN]
					mov		cx,[XEN]
					mov		di,[XST]
					mov		bx,[YST]
					call	ClipTop

					mov		[YST],di
					mov		bx,TOP_EDGE
					mov		[XST],bx

					mov		ax,[YST]
					mov		bp,[XST]
					call	ClassifyPoint
					mov		[CCST],dl
					ret
	
LineClipLeft:	test	ah,LEFT_CC	; end clip code is off left?
				jz		.left_p1on_p2off
.left_p1off_p2on:	mov		cx,[YST]
					mov		ax,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]	
					call	ClipLeft 	; ax returned 
	
					mov		[XEN],bx
					mov		bx,LEFT_EDGE
					mov		[YEN],bx
		
					mov		ax,[YEN]
					mov		bp,[XEN]
					call	ClassifyPoint
					mov		[CCEN],dl
					ret
.left_p1on_p2off:	mov		cx,[YEN]
					mov		ax,[XEN]
					mov		di,[XST]
					mov		bx,[YST]
					call	ClipLeft

					mov		[XST],bx
					mov		bx,LEFT_EDGE
					mov		[YST],bx

					mov		ax,[YST]
					mov		bp,[XST]
					call	ClassifyPoint
					mov		[CCST],dl
					ret

LineClipRight:	test	ah,RIGHT_CC
				jz		.right_p1on_p2off
.right_p1off_p2on:	mov		bp,[YST]
					mov		dx,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]	
					call	ClipRight

					mov		[XEN],bp
					mov		bx,RIGHT_EDGE
					mov		[YEN],bx
	
					mov		ax,[YEN]
					mov		bp,[XEN]
					call	ClassifyPoint
					mov		[CCEN],dl		
					ret

.right_p1on_p2off:	mov		bp,[YEN]
					mov		dx,[XEN]
					mov		di,[XST]
					mov		bx,[YST]	
					call	ClipRight

					mov		[XST],bp
					mov		bx,RIGHT_EDGE
					mov		[YST],bx

					mov		ax,[YST]
					mov		bp,[XST]
					call	ClassifyPoint
					mov		[CCST],dl
					ret
	
LineClipBottom:	test	ah,BOTTOM_CC
				jz		.bottom_p1on_p2off
.bottom_p1off_p2on:	mov		ax,[YST]
					mov		dx,[XST]
					mov		di,[XEN]
					mov		bx,[YEN]	
					call	ClipBottom

					mov		[YEN],cx
					mov		bx,BOTTOM_EDGE
					mov		[XEN],bx

					mov		ax,[YEN]
					mov		bp,[XEN]
					call	ClassifyPoint
					mov		[CCEN],dl		
					ret
.bottom_p1on_p2off:	mov		ax,[YEN]
					mov		dx,[XEN]
					mov		di,[XST]
					mov		bx,[YST]	
					call	ClipBottom

					mov		[YST],cx
					mov		bx,BOTTOM_EDGE
					mov		[XST],bx

					mov		ax,[YST]
					mov		bp,[XST]
					call	ClassifyPoint
					mov		[CCST],dl
					ret
	
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

RotationMatrix:	mov		bp,[XAngle]
				call	SinCos
				mov		[SinX],bp
				mov		[CosX],ax

				mov		bp,[YAngle]
				call	SinCos
				mov		[SinY],bp
				mov		[CosY],ax
	
				mov		bp,[ZAngle]
				call	SinCos
				mov		[SinZ],bp
				mov		[CosZ],ax

				ret

; cos(x) * cos(z) 										= mz3 ?
; sin(x)												= my3 ?
; cos(x) * -sin(z)										= mx3 ?
 
; (cos(y) * -sin(x) * cos(z)) + (sin(y) * sin(z)),		= mz2 ?
; cos(y) * cos(x),										= my2 ?
; (cos(y) * -sin(x) * -sin(z)) + (sin(y) * cos(z))		= mx2 ?

; (-sin(y) * -sin(x) * cos(z)) + (cos(y) * sin(z))		= mz1 ?
; -sin(y) * cos(x) 										= my1 ?
; (-sin(y) * -sin(x) * -sin(z)) + (cos(y) * cos(z)) 	= mx1 ?


; mx1= (sin(gam)*sin(bet)*sin(alp)) + (cos(gam)*cos(alp))
; my1= (-cos(bet))*sin(alp)
; mz1= (sin(gam)*cos(alp)) - (cos(gam)*sin(bet)*sin(alp))

; mx2= (cos(gam)*sin(alp)) - (sin(gam)*sin(bet)*cos(alp))
; my2= (cos(bet)*cos(alp))
; mz2= ((-cos(gam))*sin(bet)*cos(alp)) - (sin(gam)*sin(alp))

; mx3 = cos(bet)*sin(gam)
; my3 = sin(bet)
; mz3 = cos(gam)*cos(bet)

; x"'=x*mx1 + y*my1 + z*mz1
; y"'=x*mx2 + y*my2 + z*mz2
; z"'=x*mx3 + y*my3 + z*mz3.

; alp=X
; bet=Y
; gam=Z

; mx1= (SinZ*SinY*SinX) + (CosZ*CosX)
; #1 = SinZ * SinY
; #2 = #1 * SinX
; #3 = CosZ * CosX
; #4 = #2 + #3

; my1= (-CosY)*SinX
; #1 = -CosY
; #2 = #1 * SinX

; mz1= (SinZ*CosX) - (CosZ*SinY*SinX)
; #1 = SinZ * CosX
; #2 = CosZ * SinY
; #3 = #2 * SinX
; #4 = #1 - #3

; mx2= (CosZ*SinX) - (SinZ*SinY*CosX)
; #1 = CosZ * SinX
; #2 = SinZ * SinY
; #3 = #2 * CosX
; #4 = #1 - #2

; my2= (CosY*CosX)
; #1 = CosY * CosX

; mz2= ((-CosZ)*SinY*CosX) - (SinZ*SinX)
; #1 = -CosZ
; #2 = #1 * SinY
; #3 = #2 * CosX
; #4 = Sinz * SinX
; #5 = #3 - #4

; mx3 = CosY*SinZ
; #1 = CosY * SinZ

; my3 = SinY
; #1 = SinY

; mz3 = CosZ*CosY
; #1 = CosZ * CosY

BuildRotateMatrix:	mov		bp,[XAngle]
					call	SinCos
					mov		[SinX],bp
					mov		[CosX],ax

					mov		bp,[YAngle]
					call	SinCos
					mov		[SinY],bp
					mov		[CosY],ax
	
					mov		bp,[ZAngle]
					call	SinCos
					mov		[SinZ],bp
					mov		[CosZ],ax
					ret
	
Rotate:
	push es
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


	pop es

	ret

ClassifyPoint:	xor		dl,dl	; in = ax/bp	; out = dl	; clear clipcodes

				cmp		bp,TOP_EDGE
				jge		.right_test
					mov		dl,TOP_CC
					jmp		.hor_test_end

.right_test:	cmp		bp,BOTTOM_EDGE
				jle		.hor_test_end
					mov		dl,BOTTOM_CC

.hor_test_end:	cmp		ax,LEFT_EDGE
				jge		.bottom_test
					or		dl,LEFT_CC
					ret
.bottom_test:	cmp		ax,RIGHT_EDGE
				jle		.ver_test_end
					or		dl,RIGHT_CC 			
.ver_test_end:	ret

Perspective:	inc		ch	; in - bp = z / ax = x  dx = y	; z +256
		
				cwd
				mov		dl,ah
				mov		ah,al		; dx:ax = ypos *256 = ypos *256	
				idiv	cx 		; ax = dx:ax = ypos*256 / zpos+256
				add		ax,MIDDLE_Y	; ypos
	
				xchg	ax,bp

				cwd
				mov		dl,ah
				mov		ah,al		; dx:ax = xpos *256 = xpos *256
				idiv	cx 		; ax = dx:ax = xpos*256 / zpos+256
				add		ax,MIDDLE_X	; xpos
	
				ret			; output bl=clip codes / ax=xpos / bp=ypos
	
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


; pre-classify point
; get camera v
; sub 255
; store start_v
; get camera_h
; sub 255
; store start_h
; get camera_z
; sub 255
; store start_z

; classify point
; get v
; sub start_v from v
; if v neg
;  off left
; if v gt 510
;  off right
; get z
; sub start_z from z
; if z neg
;  off back
; if z gt 510
;  off front

; get h
; sub start_h from h
; if h neg
;  off top
; if h gt 510
;  off bot
 
; get object
; rotate it
; translate it
; classify each point
; if onscreen 
;  do perspective

; read through lines

MAX_EDGE EQU 510
MIDDLE_H EQU MIDDLE_X

DrawObjectZW:	push	di ; object lines
				push	cx ; number of lines

				xor		ch,ch
				dec		cl
				add		cl,cl
				mov		di,cx	; point count

 push di
				mov		bx,255 ; high = or clip codes / low = and clip codes
	
.rotate_point_loop:	push	bx
					mov		ax,[ds:si]		; vpos
					mov		bx,[ds:si+2]	; hpos
					mov		cx,[ds:si+4]	; zpos
	mov		[Test3],cx
		
					xor		dl,dl					; clip code

					add		ax,[VOffset]

;					jns		.off_right
;						mov		dl,LEFT_CC
;						jmp		.off_ver
;.off_right			cmp		ax,MAX_EDGE
;					jle		.off_ver
;						mov		dl,RIGHT_CC


.off_ver			add		bx,[HOffset]

;					jns		.off_bottom
;						or		dl,TOP_CC
;						jmp		.off_z
;.off_bottom			cmp		bx,MAX_EDGE
;					jle		.off_z
;						or		dl,BOTTOM_CC


.off_z				add		cx,[ZOffset]
	mov		[Test4],cx



;					jns		.off_back
;						or		dl,FRONT_CC
;						jmp		.off_clip
;.off_back			cmp		cx,MAX_EDGE
;					jle		.off_clip
;						or		dl,BACK_CC

.off_clip:
					mov		[ObjectYRot+di],ax 	; yrot
					mov		[ObjectXRot+di],bx 	; xrot
					mov		[ObjectZRot+di],cx 	; zrot

					mov		[ObjectClipCode+di],dl 	; clip code

					test	dl,dl
					jnz		.off_scn
						mov		dx,ax

	mov		[Test0],dx
	mov		[Test1],bx
	mov		[Test2],cx

						call	PerspectiveZW
						mov		[ObjectYPos+di],bx 		; xpos
						mov		[ObjectXPos+di],cx 		; ypos
						
;	mov		[Test1],bx
;	mov		[Test2],cx

						xor		dl,dl	; if it reaches here it must be onscreen?

.off_scn:
					pop		bx
					and		bl,dl	; object and clip code
					or		bh,dl	; object or clip code
		
					add		si,6				
					sub		di,2
					jns		.rotate_point_loop
				
				pop		cx 	; number of lines
				pop 	si 	; object lines

 pop di ; number of points

;	mov		[Test0],bx

						mov		bx,[ObjectYPos+0] 		; xpos
						mov		bp,[ObjectXPos+0] 		; ypos
						call	Plot11
						mov		bx,[ObjectYPos+2] 		; xpos
						mov		bp,[ObjectXPos+2] 		; ypos
						call	Plot11
						mov		bx,[ObjectYPos+4] 		; xpos
						mov		bp,[ObjectXPos+4] 		; ypos
						call	Plot11
						mov		bx,[ObjectYPos+6] 		; xpos
						mov		bp,[ObjectXPos+6] 		; ypos
						call	Plot11

						mov		bx,[ObjectYPos+8] 		; xpos
						mov		bp,[ObjectXPos+8] 		; ypos
						call	Plot11
						mov		bx,[ObjectYPos+10] 		; xpos
						mov		bp,[ObjectXPos+10] 		; ypos
						call	Plot11
						mov		bx,[ObjectYPos+12] 		; xpos
						mov		bp,[ObjectXPos+12] 		; ypos
						call	Plot11
						mov		bx,[ObjectYPos+14] 		; xpos
						mov		bp,[ObjectXPos+14] 		; ypos
						call	Plot11
  
	ret
				cmp		bl,0				; if and_object_clip_code ne 0 all off one side so exit
				jnz		.off_screen
 
				cmp		bh,0				; if or_object_clip_code eq 0 all on screen so no clip needed
				jnz		.draw_clipped_object

.draw_whole_object:
.draw_whole_object_line_loop:	push	cx
								push	si

								xor		bh,bh
								mov		bl,[ds:si]
								add		bl,bl	
								mov		cx,[ObjectYPos+bx] ; endy
								mov		dx,[ObjectXPos+bx] ; endx
 
								mov		bl,[ds:si+1]
								add		bl,bl
								mov		bp,[ObjectYPos+bx] ; starty
								mov		si,[ObjectXPos+bx] ; startx
								call	Line00
	 
								pop		si
								pop		cx
								add		si,2

								dec		ch
								jnz		.draw_whole_object_line_loop

.off_screen:				ret
	
.draw_clipped_object:	
.draw_clipped_object_line_loop:	xor		bh,bh
								push	cx
								push	si
		
								mov		bl,[ds:si]
								add		bl,bl
								mov		al,[ObjectClipCode+bx] 	; start clip code
								mov		bl,[ds:si+1]
								add		bl,bl
								mov		ah,[ObjectClipCode+bx] 	; end clip code

								mov		bl,al		; backup start clip code
								and		bl,ah	; both points are off 
								jnz		.line_off_screen	; then line is off screen

								or		al,ah 	; if any point is off
								jnz		.line_clip	; then line needs clipped
		
								mov		bl,[ds:si]
								add		bl,bl	
								mov		cx,[ObjectYPos+bx] ; starty
								mov		dx,[ObjectXPos+bx] ; startx

								mov		bl,[ds:si+1]
								add		bl,bl
								mov		bp,[ObjectYPos+bx] ; endy 
								mov		si,[ObjectXPos+bx] ; endx - todo - si is the problem?

.draw_clipped_line:				call	Line11

.line_off_screen:				pop		si
								pop		cx
								add		si,2
		
								dec		ch
								jnz		.draw_clipped_object_line_loop
							ret

.line_clip:	; al = point 1 clip ; ah = point 2 clip

; copy point details to 2 sets of coords temp coords zyx and temp perspective xy
; if either point is behind
;  do z clip on required point
;  do perspective
;  do rest of tests

					xor		bh,bh
					mov		bl,[ds:si]
					add		bl,bl	
					mov		dx,[ObjectXRot+bx]
					mov		[STX],dx
					mov		dx,[ObjectYRot+bx]
					mov		[STY],dx
					mov		dx,[ObjectZRot+bx]
					mov		[STZ],dx
					mov		dx,[ObjectXPos+bx]
					mov		[XST],dx
					mov		dx,[ObjectYPos+bx]	
					mov		[YST],dx	
					mov		al,[ObjectClipCode+bx]
	
					mov		bl,[ds:si+1]
					add		bl,bl
					mov		dx,[ObjectXRot+bx]
					mov		[ENX],dx
					mov		dx,[ObjectYRot+bx]
					mov		[ENY],dx
					mov		dx,[ObjectZRot+bx]
					mov		[ENZ],dx	
					mov		dx,[ObjectXPos+bx]
					mov		[XEN],dx
					mov		dx,[ObjectYPos+bx]
					mov		[YEN],dx
					mov		ah,[ObjectClipCode+bx]

					mov		[CCST],ax
	
.line_clip_loop:	mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side
	
					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line

.clip_behind_test:	test	bl,BEHIND_CC	; test if either point is off (al=al or ah)
					jz		.clip_left_test
					call	LineClipBehind

					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line
	
.clip_left_test:	test	bl,LEFT_CC
					jz		.clip_right_test
					call	LineClipLeft
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line
		
.clip_right_test:	test	bl,RIGHT_CC
					jz		.clip_top_test
					call	LineClipRight
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side
	
					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line
		

.clip_top_test:		test	bl,TOP_CC
					jz		.clip_bottom_test
					call	LineClipTop
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

					or		bl,ah			; or with end cc
					jz		.clip_draw_test		; all on so draw line

.clip_bottom_test:	test	bl,BOTTOM_CC
					jz		.line_clip_loop
					call	LineClipBottom
					mov		ax,[CCST]
					mov		bl,al			; start cc
					and		al,ah			; and with end cc
					jnz		.line_off_screen		; both off same side

.clip_draw_test:	mov		cx,[YST]
					mov		dx,[XST]
					mov		bp,[YEN]
					mov		si,[XEN]
					jmp		.draw_clipped_line

; hor = 0 to 510
; ver = 0 to 510
; z = 0 to 510

PerspectiveZW: ; in = bx=z/cx=v/dx=h/dh=hsign - out = bx=v/cx=h
	shr		bx,1
	mov		al,[persvtab8+bx]
	shr		cx,1
	MUL		cl

	mov		al,[pershtab8+bx]
	mov		bl,ah
	inc 	dx
    test 	dh,dh
	jnz		.pos

.neg	neg		dl
		MUL		dl
		mov		cl,ah
		neg		cx
		add		cx,MIDDLE_H
		ret

.pos	MUL		dl
		mov		cl,ah
		add		cx,MIDDLE_H
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

Hex0	db 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1	db 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2	db 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3	db 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4	db 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b
 
ObjectPoints00:
	dw	-30,-30,-30 		;+150
	dw	-30,-30,+30		;+150
	dw	-30,+30,-30		;+150
	dw	-30,+30,+30		;+150
	dw	+30,-30,-30 		;+150
	dw	+30,-30,+30 		;+150
	dw	+30,+30,-30		;+150
	dw	+30,+30,+30		;+150

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

HOffset:	dw	200
VOffset:	dw	200

XOffset:	dw	-500
YOffset:	dw	-500
ZOffset:	dw	200 ; 2500	

pershtab8:
 db 159
 db 159
 db 159
 db 159
 db 158
 db 158
 db 158
 db 158
 db 158
 db 157
 db 157
 db 157
 db 157
 db 156
 db 156
 db 156
 db 156
 db 156
 db 155
 db 155
 db 155
 db 155
 db 155
 db 154
 db 154
 db 154
 db 154
 db 153
 db 153
 db 153
 db 153
 db 153
 db 152
 db 152
 db 152
 db 152
 db 152
 db 151
 db 151
 db 151
 db 151
 db 150
 db 150
 db 150
 db 150
 db 150
 db 149
 db 149
 db 149
 db 149
 db 149
 db 148
 db 148
 db 148
 db 148
 db 147
 db 147
 db 147
 db 147
 db 147
 db 146
 db 146
 db 146
 db 146
 db 146
 db 145
 db 145
 db 145
 db 145
 db 144
 db 144
 db 144
 db 144
 db 144
 db 143
 db 143
 db 143
 db 143
 db 143
 db 142
 db 142
 db 142
 db 142
 db 141
 db 141
 db 141
 db 141
 db 141
 db 140
 db 140
 db 140
 db 140
 db 140
 db 139
 db 139
 db 139
 db 139
 db 138
 db 138
 db 138
 db 138
 db 138
 db 137
 db 137
 db 137
 db 137
 db 137
 db 136
 db 136
 db 136
 db 136
 db 135
 db 135
 db 135
 db 135
 db 135
 db 134
 db 134
 db 134
 db 134
 db 134
 db 133
 db 133
 db 133
 db 133
 db 132
 db 132
 db 132
 db 132
 db 132
 db 131
 db 131
 db 131
 db 131
 db 130
 db 130
 db 130
 db 130
 db 130
 db 129
 db 129
 db 129
 db 129
 db 129
 db 128
 db 128
 db 128
 db 128
 db 127
 db 127
 db 127
 db 127
 db 127
 db 126
 db 126
 db 126
 db 126
 db 126
 db 125
 db 125
 db 125
 db 125
 db 124
 db 124
 db 124
 db 124
 db 124
 db 123
 db 123
 db 123
 db 123
 db 123
 db 122
 db 122
 db 122
 db 122
 db 121
 db 121
 db 121
 db 121
 db 121
 db 120
 db 120
 db 120
 db 120
 db 120
 db 119
 db 119
 db 119
 db 119
 db 118
 db 118
 db 118
 db 118
 db 118
 db 117
 db 117
 db 117
 db 117
 db 117
 db 116
 db 116
 db 116
 db 116
 db 115
 db 115
 db 115
 db 115
 db 115
 db 114
 db 114
 db 114
 db 114
 db 114
 db 113
 db 113
 db 113
 db 113
 db 112
 db 112
 db 112
 db 112
 db 112
 db 111
 db 111
 db 111
 db 111
 db 111
 db 110
 db 110
 db 110
 db 110
 db 109
 db 109
 db 109
 db 109
 db 109
 db 108
 db 108
 db 108
 db 108
 db 108
 db 107
 db 107
 db 107
 db 107
 db 106
 db 106
 db 106
 db 106
 db 106
 db 105
 db 105
 db 105
 db 105
 db 105

persvtab8:
 db 199
 db 199
 db 199
 db 198
 db 198
 db 198
 db 198
 db 197
 db 197
 db 197
 db 197
 db 196
 db 196
 db 196
 db 195
 db 195
 db 195
 db 195
 db 194
 db 194
 db 194
 db 194
 db 193
 db 193
 db 193
 db 192
 db 192
 db 192
 db 192
 db 191
 db 191
 db 191
 db 191
 db 190
 db 190
 db 190
 db 190
 db 189
 db 189
 db 189
 db 188
 db 188
 db 188
 db 188
 db 187
 db 187
 db 187
 db 187
 db 186
 db 186
 db 186
 db 185
 db 185
 db 185
 db 185
 db 184
 db 184
 db 184
 db 184
 db 183
 db 183
 db 183
 db 183
 db 182
 db 182
 db 182
 db 181
 db 181
 db 181
 db 181
 db 180
 db 180
 db 180
 db 180
 db 179
 db 179
 db 179
 db 178
 db 178
 db 178
 db 178
 db 177
 db 177
 db 177
 db 177
 db 176
 db 176
 db 176
 db 176
 db 175
 db 175
 db 175
 db 174
 db 174
 db 174
 db 174
 db 173
 db 173
 db 173
 db 173
 db 172
 db 172
 db 172
 db 171
 db 171
 db 171
 db 171
 db 170
 db 170
 db 170
 db 170
 db 169
 db 169
 db 169
 db 169
 db 168
 db 168
 db 168
 db 167
 db 167
 db 167
 db 167
 db 166
 db 166
 db 166
 db 166
 db 165
 db 165
 db 165
 db 164
 db 164
 db 164
 db 164
 db 163
 db 163
 db 163
 db 163
 db 162
 db 162
 db 162
 db 161
 db 161
 db 161
 db 161
 db 160
 db 160
 db 160
 db 160
 db 159
 db 159
 db 159
 db 159
 db 158
 db 158
 db 158
 db 157
 db 157
 db 157
 db 157
 db 156
 db 156
 db 156
 db 156
 db 155
 db 155
 db 155
 db 154
 db 154
 db 154
 db 154
 db 153
 db 153
 db 153
 db 153
 db 152
 db 152
 db 152
 db 152
 db 151
 db 151
 db 151
 db 150
 db 150
 db 150
 db 150
 db 149
 db 149
 db 149
 db 149
 db 148
 db 148
 db 148
 db 147
 db 147
 db 147
 db 147
 db 146
 db 146
 db 146
 db 146
 db 145
 db 145
 db 145
 db 145
 db 144
 db 144
 db 144
 db 143
 db 143
 db 143
 db 143
 db 142
 db 142
 db 142
 db 142
 db 141
 db 141
 db 141
 db 140
 db 140
 db 140
 db 140
 db 139
 db 139
 db 139
 db 139
 db 138
 db 138
 db 138
 db 138
 db 137
 db 137
 db 137
 db 136
 db 136
 db 136
 db 136
 db 135
 db 135
 db 135
 db 135
 db 134
 db 134
 db 134
 db 133
 db 133
 db 133
 db 133
 db 132
 db 132
 db 132
 db 132
 db 131
 db 131
 db 131
 db 131

testx dw 0
testy dw 0
testz dw 0
	
%include  'sin256.inc'

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words
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

XAngle:	resw	1
YAngle:	resw	1
ZAngle:	resw	1

Exit:	resb	1

STX:		resw	1
STY:		resw	1
STZ:		resw	1
ENX:		resw	1
ENY:		resw	1
ENZ:		resw	1

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
