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
RIGHT_CC		equ	8
BEHIND_CC	equ	16
MIDDLE_X		equ	160
MIDDLE_Y		equ	100
TOP_EDGE	equ	MIDDLE_Y-100
BOTTOM_EDGE	equ	MIDDLE_Y+99
LEFT_EDGE	equ	MIDDLE_X-160
RIGHT_EDGE	equ	MIDDLE_X+159

KEY_1	EQU	'1'
KEY_2	EQU	'2'
KEY_3	EQU	'3'
KEY_4 	EQU	'4'
KEY_5 	EQU	'5'
KEY_6 	EQU	'6'
KEY_ESC	EQU 1bh
KEY_Q	EQU 'q'
KEY_W	EQU 'w'
KEY_E	EQU 'e'
KEY_R	EQU 'r'
KEY_T	EQU 't'
KEY_Y	EQU 'y'

CPU 386
bits 16

%include "exebin.mac"

EXE_begin

section .text

start:
	call	BuildScreenTable
	mov		ax,05h ; 04h	; CGA 320 x 200 4 colors ; more 05 instead of mode 04
	int		10h

;	mov	ax,0Bh 	; Pallette
;	mov	bh,1
;	mov	bl,0
;	mov	bx,00000h ; pallette 0 high
;	int	10h 
	
	mov		bx,	16384 ; 8000 words (160x100 1 char byte 1 attribute byte)
	call	AllocMem
	mov		[BackBufferSeg], ax

	mov		bx,	16384
	call	AllocMem
	mov		[ScreenBufferSeg], ax	
	
	xor 	eax,eax 
	call	ClearBackBuffer
	
	xor ax,ax
	mov	[Exit],al
	mov	[XAngle],ax
	mov	[YAngle],ax		
	mov	[ZAngle],ax
		
MainLoop:
		call	WaitVSync 				; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBufferV8 	; copy back to front
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		mov		bl,3
		mov		ax,[XAngle]
		call	Hex16
		mov		bl,4
		mov		ax,[YAngle]
		call	Hex16
		mov		bl,5
		mov		ax,[ZAngle]
		call	Hex16

		mov		bl,6
		movsx	ax,[SinX_B]
		call	Hex16
		mov		bl,7
		movsx	ax,[CosX_B]
		call	Hex16
		mov		bl,8
		movsx	ax,[SinY_B]
		call	Hex16
		mov		bl,9
		movsx	ax,[CosY_B]
		call	Hex16
		mov		bl,10
		movsx	ax,[SinZ_B]
		call	Hex16
		mov		bl,11
		movsx	ax,[CosZ_B]
		call	Hex16

		mov		bl,13
		mov		ax,[Test0]
		call	Hex16
		mov		bl,14
		mov		ax,[Test1]
		call	Hex16
		mov		bl,15
		mov		ax,[Test2]
		call	Hex16

		mov		eax,[TestD0]
		mov		bl,27
		call	Hex32
		mov		eax,[TestD1]
		mov		bl,28
		call	Hex32
		mov		eax,[TestD2]
		mov		bl,29
		call	Hex32

		mov		eax,[xLocation]
		mov		bl,30
		call	Hex32
		mov		eax,[yLocation]
		mov		bl,31
		call	Hex32
		mov		eax,[zLocation]
		mov		bl,32
		call	Hex32

		call	BuildRotateMatrixV2

		mov		si,0; 5	; number of objects
		call	DrawObjects

		mov		si,ObjectPoints00 ; points
		mov		di,ObjectLines00 ; lines
		mov		cl,8 ; 8 		; points
		mov		ch,12 ; 12	; lines
		call 	DrawObject
		
		call	GetKey
		
		mov		al,[Exit]
		or		al,al
		je		MainLoop

Done:	mov		ax,3	; reset to text mode
		int		10h
		mov		ah,4ch	; exit to DOS
		int		21h

DrawObjects	shl		si,2	; *4
			
.object_loop:	push	si

				mov		ebp,[xObjectPos+si]
				mov		ecx,[zObjectPos+si]
				mov		esi,[yObjectPos+si]

;	mov		[TestD0],ebp

				sub		ebp,[xLocation]
				sub		esi,[yLocation]
				sub		ecx,[zLocation]

	mov		[TestD0],ebp
	mov		[TestD1],esi
	mov		[TestD2],ecx
	
				call	RotateV3
				jns		.object_front
						mov		dl,BEHIND_CC
						jmp		.point_behind	
.object_front:
				call	PerspectiveV3
				call	ClassifyPoint ;V3
				test	dl,dl
				jnz		.point_behind
					mov		bx,bp
					mov		bp,ax
					call	Plot00
.point_behind	pop		si
				sub		si,4
				jns		.object_loop

			ret

AllocMem:	shr		bx, 4	; divide memory required into paragraphs
			inc		bx		; add 1 
			mov		ah, 48h
			int		21h
;			jnc		.ok
;				xor		ax,ax	; reset
.ok			ret

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
	cmp 	al,KEY_1
	je 	.key_1
	cmp 	al,KEY_2
	je 	.key_2
	cmp 	al,KEY_3
	je 	.key_3
	cmp 	al,KEY_4
	je 	.key_4
	cmp 	al,KEY_5
	je 	.key_5
	cmp 	al,KEY_6
	je 	.key_6
	cmp 	al,KEY_Q
	je 	.key_q
	cmp 	al,KEY_W
	je 	.key_w
	cmp 	al,KEY_E
	je 	.key_e
	cmp 	al,KEY_R
	je 	.key_r
	cmp 	al,KEY_T
	je 	.key_t
	cmp 	al,KEY_Y
	je 	.key_y
.key_none:
	ret
.key_esc:	mov	al,1
		mov	[Exit],al
		ret
.key_1:	mov eax,[xLocation]
		sub eax,37
		mov [xLocation],eax
		ret
.key_2:	mov eax,[xLocation]
		add eax,37
		mov [xLocation],eax
		ret
.key_3:	mov eax,[yLocation]
		sub eax,37
		mov [yLocation],eax
		ret
.key_4:	mov eax,[yLocation]
		add eax,37
		mov [yLocation],eax
		ret
.key_5:	mov eax,[zLocation]
		sub eax,37
		mov [zLocation],eax
		ret
.key_6:	mov eax,[zLocation]
		add eax,37
		mov [zLocation],eax
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
.key_t:	mov	ax,[ZAngle]
		inc	ax
		and ah,3
		mov	[ZAngle],ax
		ret
.key_y:	mov	ax,[ZAngle]
		dec	ax
		and ah,3
		mov	[ZAngle],ax
		ret

WaitVSync: 			mov		dx,INPUT_STATUS_1	; Wait for the leading edge of vertical sync pulse.
WaitNotVSyncLoop:		in		al,dx
						and		al,VSYNC_MASK
						jnz		WaitNotVSyncLoop
;WaitVSyncLoop:			in		al,dx
;						and		al,VSYNC_MASK
;						jz		WaitVSyncLoop
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

						mov		eax,001011010010110100101101001011010b ;

ClearBackBuffer:		mov		cx,[BackBufferSeg]		; es points to ds
						mov		es,cx
	
						xor		di,di		; DS:SI points to even back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						mov		di,8192		; DS:SI points to odd back buffer	
						mov		cx,2000
						rep		stosd		; clear odd back buffer

						ret

Plot00:		add		bx,bx 					; start ver *2
			mov		di,[BackBufferTable+bx]	; lookup start ver
			add		bp,bp					; start hor *2
			add		di,[HorPosTable+bp]		; add start hor

			mov		cl,[es:di]
			and		cl,[AndTable+bp]
			mov		[es:di],cl

;			mov		cl,[AndTable+bp]
;			and byte [es:di],cl

			ret

Plot11:		add		bx,bx 					; start ver *2
			mov		di,[BackBufferTable+bx]	; lookup start ver
			add		bp,bp					; start hor *2
			add		di,[HorPosTable+bp]		; add start hor

			mov		cl,[es:di]
			or		cl,[OrTable+bp]
			mov		[es:di],cl

;			mov		cl,[OrTable+bp]
;			or byte [es:di],cl

			ret

DrawObject:		push	di ; object lines
				push	cx ; number of lines

;				mov		di,ObjectPointsResults

				xor		ch,ch
				dec		cl		
				add		cl,cl
				add		cl,cl	; 32_bit_increase 
				mov		di,cx	; point count
;	lea		di,[cx*4-4]
		
				mov		bx,255 ; high = or clip codes / low = and clip codes
	
.rotate_point_loop:	push	si
					push	bx

					movsx	ebp,word [ds:si]	; xpos
					add		ebp,[xLocation]
					movsx	ecx,word [ds:si+4]	; zpos
					add		ecx,[zLocation]
					movsx	esi,word [ds:si+2]	; ypos
					add		esi,[yLocation]

;				sub		ebp,[xLocation]
;				sub		esi,[yLocation]
;				sub		ecx,[zLocation]
				
					call	RotateV3
		
					mov		[ObjectYRot+di],eax 	; 32_bit_increase
					mov		[ObjectXRot+di],ebp 	; 32_bit_increase
					mov		[ObjectZRot+di],ecx 	; 32_bit_increase

	test	ecx,ecx	; 32_bit_increase
					jns		.point_in_front
						mov		dl,BEHIND_CC
						jmp		.point_behind	

.point_in_front:	call	PerspectiveV3
					mov		[ObjectYPos+di],ax 		; xpos
					mov		[ObjectXPos+di],bp 		; ypos
					call	ClassifyPoint

.point_behind:		mov		[ObjectClipCode+di],dl 	; clip code

					pop		bx
					and		bl,dl	; object and clip code
					or		bh,dl	; object or clip code
		
					pop		si
					add		si,6				
					sub		di,4	; 32_bit_increase
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
								add		bl,bl	; 32_bit_increase
								mov		cx,[ObjectYPos+bx] ; endy
								mov		dx,[ObjectXPos+bx] ; endx
 
								mov		bl,[ds:si+1]
								add		bl,bl
								add		bl,bl	; 32_bit_increase
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
;	ret		; temp
.draw_clipped_object_line_loop:	xor		bh,bh
								push	cx
								push	si
		
								mov		bl,[ds:si]
								add		bl,bl
								add		bl,bl	; 32_bit_increase
								mov		al,[ObjectClipCode+bx] 	; start clip code
								mov		bl,[ds:si+1]
								add		bl,bl
								add		bl,bl	; 32_bit_increase
								mov		ah,[ObjectClipCode+bx] 	; end clip code

								mov		bl,al		; backup start clip code
								and		bl,ah	; both points are off 
								jnz		.line_off_screen	; then line is off screen

								or		al,ah 	; if any point is off
								jnz		.line_clip	; then line needs clipped
		
								mov		bl,[ds:si]
								add		bl,bl	
								add		bl,bl	; 32_bit_increase
								mov		cx,[ObjectYPos+bx] ; starty
								mov		dx,[ObjectXPos+bx] ; startx

								mov		bl,[ds:si+1]
								add		bl,bl
								add		bl,bl	; 32_bit_increase
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
					add		bl,bl	; 32_bit_increase
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
					add		bl,bl	; 32_bit_increase
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

SinCosV3:	mov		bx,bp
			call	GetSin256
			mov		ah,al
			mov		bx,bp
			inc		bh
			and		bh,3
			call	GetSin256
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

BuildRotateMatrixV2	mov		bp,[XAngle]
					call	SinCosV3
					mov		[CosX_B],al
					mov		[SinX_B],ah
					
					mov		bp,[YAngle]
					call	SinCosV3
					mov		[CosY_B],al	; pc
					mov		[SinY_B],ah	; ps
	
					mov		bp,[ZAngle]
					call	SinCosV3
					mov		[CosZ_B],al	; yc
					mov		[SinZ_B],ah	; ys
					
;	const short prc_pyc = (rc * yc) /128;				#1 
					mov		al,[CosX_B]						; rc				
					mov		ah,[CosZ_B]						; yc
					imul	ah								; rc*yc
					add		ax,ax							; *2
					mov		[prc_pyc],ah					; /256

;	const short prc_mys = (rc * -ys) /128;				#2			
					mov		al,[SinZ_B]						; ys
					neg		al								; -ys
					mov		ah,[CosX_B]						; rc
					imul	ah								; rc*-ys
					add		ax,ax							; *2
					mov		[prc_mys],ah						; /256			

;	const long ppc_mrs_pyc = (pc * -rs * yc) /16384;	#3
					mov		al,[CosY_B]						; pc
					mov		ah,[SinX_B]						; rs
					neg		ah								; -rs
					imul	ah								; pc*-rs
					movsx	dx,byte [CosZ_B]						; yc
					imul	dx								; pc*-rs*yc	; answer is dx:ax
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry

; shift ah by 1 into carry = shr dl,1 = d1*2
;	shl		dl,1			;	ASL 		onent *2
;	shl		dh,1			;	ASL 	ab06	; shift msb putting sign into carry ; carry isnt being set?
;	rcl		dl,1			;	ROL 			; rotate carry into first bit of exponent - bit 0 is sign
															
					mov		[ppc_mrs_pyc],dl						; /16384

;	const short pps_pys = (ps * ys) /128;				#4
					mov		al,[SinY_B]						; ps
					mov		ah,[SinZ_B]						; ys
					imul	ah								; ps*ys
					add		ax,ax							; *2
					mov		[pps_pys],ah					; /256			

;	const short ppc_prc = (pc * rc) /128;				#5
					mov		al,[CosY_B]						; pc
					mov		ah,[CosX_B]						; rc
					imul	ah								; pc*irc
					add		ax,ax							; *2
					mov		[ppc_prc],ah					; /256			

;	const long ppc_mrs_mys = (pc * -rs * -ys) /16384;	#6
					mov		al,[SinX_B]						; rs
					neg		al								; -rs
					mov		ah,[CosY_B]						; pc
					imul	al								; pc*-rs
					movsx	dx,byte [SinZ_B]				; ys
					neg		dx								; -ys
					imul	dx								; pc*-rs*-ys	; answer is dx:ax
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					mov		[ppc_mrs_mys],dl				; /16384

;	const short pps_pyc = (ps * yc) /128;				#7
					mov		al,[SinY_B]						; ps
					mov		ah,[CosZ_B]						; yc
					imul	ah								; ps*yc
					add		ax,ax							; *2
					mov		[pps_pyc],ah					; /256			

;	const long mps_mrs_pyc = (-ps * -rs * yc) /16384;	#8
					mov		al,[SinY_B]						; ps
					neg		al								; -ps
					mov		ah,[SinX_B]						; rs
					neg		ah								; -rs
					imul	ah								; -ps*-rs
					movsx	dx,byte [CosZ_B]				; yc
					imul	dx								; -ps*-rs*yc		; answer is dx:ax
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					mov		[mps_mrs_pyc],dl				; /16384

;	const short ppc_pys = (pc * ys) /128;				#9
					mov		al,[CosY_B]						; pc
					mov		ah,[SinZ_B]						; ys
					imul	ah								; rc*ys
					add		ax,ax							; *2
					mov		[ppc_pys],ah					; /256

;	const short mps_prc = (-ps * rc) /128;				#10
					mov		al,[SinY_B]						; ps
					neg		al								; -ps
					mov		ah,[CosX_B]						; rc
					imul	ah								; -ps*rc
					add		ax,ax							; *2
					mov		[mps_prc],ah					; /256			

;	const long mps_mrs_mys = (-ps * -rs * -ys) /16384;	#11	?
					mov		al,[SinY_B]						; ps
					neg		al								; -ps
					mov		ah,[SinX_B]						; rs
					neg		ah								; -rs
					imul	ah								; -ps*-rs
					movsx	dx,byte [SinZ_B]						; ys
					neg		dx								; -ys
					imul	dx								; -ps*-rs*-ys	; answer is dx:ax
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					shl		ah,1							; shift ah by 1 into carry
					rcl		dl,1							; shift msb putting sign into carry
					mov		[mps_mrs_mys],dl				; /16384

;	const short ppc_pyc = (pc * yc) /128;				#12 ?
					mov		al,[CosY_B]						; pc
					mov		ah,[CosZ_B]						; yc
					imul	ah								; pc*yc
					add		ax,ax							; *2
					mov		[ppc_pyc],ah					; /256			

;	const short x0= prc_pyc;								#13
					movsx	ax,byte [prc_pyc]
					add		ax,ax
					mov		[x0],ax
			CWDE
			mov		[xd0],eax
					
;	const char y0= rs;									#14
					movsx	ax,byte [SinX_B]
					add		ax,ax
					mov		[y0],ax
			CWDE
			mov		[yd0],eax
					
;	const short z0= prc_mys;								#15
					movsx	ax,byte [prc_mys]
					add		ax,ax
					mov		[z0],ax
			CWDE
			mov		[zd0],eax

;	const long x1= ppc_mrs_pyc + pps_pys;				#16
					mov		al,[ppc_mrs_pyc]
					mov		ah,[pps_pys]
					add		al,ah
					cbw
					add		ax,ax
					mov		[x1],ax
			CWDE
			mov		[xd1],eax

;	const short y1= ppc_prc;								#17
					movsx	ax,byte [ppc_prc]
					add		ax,ax
					mov		[y1],ax
			CWDE
			mov		[yd1],eax

;	const long z1= ppc_mrs_mys + pps_pyc;				#18
					mov		al,[ppc_mrs_mys]
					mov		ah,[pps_pyc]
					add		al,ah
					cbw
					add		ax,ax
					mov		[z1],ax
			CWDE
			mov		[zd1],eax

;	const long x2= mps_mrs_pyc + ppc_pys;				#19
					mov		al,[mps_mrs_pyc]
					mov		ah,[ppc_pys]
					add		al,ah
					cbw
					add		ax,ax
					mov		[x2],ax
			CWDE
			mov		[xd2],eax

;	const short y2= mps_prc;								#20
					movsx	ax,byte [mps_prc]
					add		ax,ax
					mov		[y2],ax
			CWDE
			mov		[yd2],eax

;	const long z2= mps_mrs_mys + ppc_pyc;				#21
					mov		al,[mps_mrs_mys]
					mov		ah,[ppc_pyc]
					add		al,ah
					cbw
					add		ax,ax
					mov		[z2],ax
			CWDE
			mov		[zd2],eax
	
;			mov		ax,[x0]
;			CWDE
;			mov		[xd0],eax
;			mov		ax,[x1]
;			CWDE
;			mov		[xd1],eax
;			mov		ax,[x2]
;			CWDE
;			mov		[xd2],eax

;			mov		ax,[y0]
;			CWDE
;			mov		[yd0],eax
;			mov		ax,[y1]
;			CWDE
;			mov		[yd1],eax
;			mov		ax,[y2]
;			CWDE
;			mov		[yd2],eax

;			mov		ax,[z0]
;			CWDE
;			mov		[zd0],eax
;			mov		ax,[z1]
;			CWDE
;			mov		[zd1],eax
;			mov		ax,[z2]
;			CWDE
;			mov		[zd2],eax

					ret
												
; x"'=x*mx1 + y*my1 + z*mz1
; y"'=x*mx2 + y*my2 + z*mz2
; z"'=x*mx3 + y*my3 + z*mz3

; ecx ; xpos
; ebp ; zpos
; esi ; ypos

RotateV3:	mov		eax,ecx
			imul	eax,[xd0]
			mov		ebx,eax

			mov		eax,esi
			imul 	eax,[yd0]
			add		ebx,eax

			mov		eax,ebp
			imul 	eax,[zd0]
			add		ebx,eax
			sar		ebx,8
			push 	ebx

			mov		eax,ecx
			imul 	eax,[xd1]
			mov		ebx,eax

			mov		eax,esi
			imul 	eax,[yd1]
			add		ebx,eax

			mov		eax,ebp
			imul 	eax,[zd1]
			add		ebx,eax
			sar		ebx,8
			push 	ebx
			
			mov		eax,ecx
			imul 	eax,[xd2]
			mov		ecx,eax

			mov		eax,esi
			imul	eax,[yd2]
			add		ecx,eax

			mov		eax,ebp
			imul 	eax,[zd2]
			add		ecx,eax
			sar		ecx,8
	
			pop eax
			pop ebp

	test	ecx,ecx	; not needed?
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

Perspective:	movsx	ecx,cx
				movsx	eax,ax
				movsx	ebp,bp

PerspectiveV3:	add		ecx,128	;	inc		ecx
	
				cdq
				shl		eax,7	
				idiv	ecx 			; ax = dx:ax = ypos*256 / zpos+256
				add		ax,MIDDLE_Y	; ypos
	
				xchg	eax,ebp

				cdq
				shl		eax,7	
				idiv	ecx 			; ax = dx:ax = xpos*256 / zpos+256
				add		ax,MIDDLE_X	; xpos
	
				ret			; ax=xpos / bp=ypos

HexChar:	mov		si,di
			mov		bl,al
			shr		bl,1
			shr		bl,1
			shr		bl,1
			shr		bl,1
;	lea		bl,[al*4]
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

Hex32:		push	eax
			pop		ax
			pop		dx

			push	ax

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
;	lea		bx,[bx*12]
			add		di,[BackBufferTable+bx]	; lookup start ver
			jmp		HexChar	 ; does ret
;			ret
	
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
	dw	-300,-300,-300 		;+150
	dw	-300,-300,+300		;+150
	dw	-300,+300,-300		;+150
	dw	-300,+300,+300		;+150
	dw	+300,-300,-300 		;+150
	dw	+300,-300,+300 		;+150
	dw	+300,+300,-300		;+150
	dw	+300,+300,+300		;+150
	
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

xObjectPos	dd	0,		-10000,0,0,0,0			; object position
yObjectPos	dd	0,		0,10000,-10000,0,0
zObjectPos	dd	10000,	0,0,0,10000,-10000
	
%include  'sin256.inc'

section .bss 	; put uninitialized data here
BackBufferTable: 	resw 202 	; 200 screen lines as words
BackBuffer: 		resw 16384	; 2 screen buffers

ObjectYPos:		resd	128 ; 128 words - xpos
ObjectXPos:		resd	128 ; 128 words - ypos
ObjectClipCode:	resd	128 ; 128 words - clip codes
ObjectXRot:		resd	128 ; 128 words - x rotated points
ObjectYRot:		resd	128 ; 128 words - y rotated points
ObjectZRot:		resd	128 ; 128 words - z rotated points

ObjectPosX		resd	128
ObjectPosY		resd	128
ObjectPosZ		resd	128

;SinZ:	resw	1

SinX_B:	resb	1
CosX_B:	resb	1
SinY_B:	resb	1
CosY_B:	resb	1
SinZ_B:	resb	1
CosZ_B:	resb	1

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
Test10: resw	1
Test11: resw	1

prc_pyc		resw	1
prc_mys		resw	1
ppc_mrs_pyc	resw	1
pps_pys		resw	1
ppc_prc		resw	1
ppc_mrs_mys	resw	1
pps_pyc		resw	1
mps_mrs_pyc	resw	1
ppc_pys		resw	1
mps_prc		resw	1
mps_mrs_mys	resw	1
ppc_pyc		resw	1

orig_x		resw	1
orig_y		resw	1
orig_z		resw	1

mx0			resw	1
my0			resw	1
mz0			resw	1
mx1			resw	1
my1			resw	1
mz1			resw	1
mx2			resw	1
my2			resw	1
mz2			resw	1

x0			resw	1
y0			resw	1
z0			resw	1
x1			resw	1
y1			resw	1
z1			resw	1
x2			resw	1
y2			resw	1
z2			resw	1

xd0			resd	1
yd0			resd	1
zd0			resd	1
xd1			resd	1
yd1			resd	1
zd1			resd	1
xd2			resd	1
yd2			resd	1
zd2			resd	1

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer to be drawn on
ScreenBufferSeg	resw	1	; pointer to the segment containing the screen buffer thats a copy of the current screen

objectlist	resd	16	; list of objects to be processed

xLocation	resd	1
yLocation	resd	1
zLocation	resd	1

xTemp	resd	1
yTemp	resd	1
zTemp	resd	1

TestD0		resd	1
TestD1		resd	1
TestD2		resd	1

EXE_end

