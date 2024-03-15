; todo - page flipping

; nasm vga3d_v16.asm -o vga3d.com -f bin

VIDEO_SEGMENT	equ	0A000h 	; display memory segment for true CGA graphics modes

VIDEO_SEGMENT_PAGE_0 equ VIDEO_SEGMENT
VIDEO_SEGMENT_PAGE_1 equ VIDEO_SEGMENT_PAGE_0 + (SCREEN_SIZE/16)

LINE_SIZE_IN_BYTES equ 360
SCREEN_WIDTH_360 equ LINE_SIZE_IN_BYTES/4

SCREEN_SIZE_IN_BYTES equ 360*240
SCREEN_SIZE equ SCREEN_SIZE_IN_BYTES/4
SCREEN_SIZE_DWORDS equ SCREEN_SIZE/4

INPUT_STATUS_1	equ	03dah	; VGA status register
VSYNC_MASK	equ	08h	; vertical sync bit in status register 1
DE_MASK		equ	01h	; display enable bit in status register 1

HIGH_ADDRESS equ 0Ch
LOW_ADDRESS  equ 0Dh

MAP_MASK	equ	2		; SC map mask register
SC_INDEX		equ	3c4h	; SC index register
CRTC_INDEX equ 03d4h

KEY_1	EQU	0231h
KEY_2	EQU	0332h
KEY_3	EQU	0433h
KEY_4 	EQU	0534h
KEY_5 	EQU	0635h
KEY_6 	EQU	0736h
KEY_ESC	EQU 	27

CPU 486
bits 16
org 100h

%macro middle 0
	mov 	ax,0ff02h
	out     dx,ax		
%endmacro

%macro left_edge 0
	mov 	al,00002h
	mov 	ah,[edgeleft+di]
	out     dx,ax

	shr 	di,2
	add		di,si

	mov 	al,bh
	stosb
%endmacro

%macro right_edge 0
	mov 	al,00002h
	mov 	ah,[edgeright+bp]
	out     dx,ax

	mov 	al,bh
	stosb
%endmacro

%macro fill_cont	0
	mov		cx,bx
	pop 	bp

	add		bp,8
	add		si,360/4
	dec		cl
	jnz		Fill.fillloop
%endmacro

section .text

start:	
		call	Mode360x240

	xor	ax,ax
	mov	[ScreenBuffer],ax
	mov	ax,VIDEO_SEGMENT
	mov	[ScreenBufferSeg],ax

	mov     dx,SC_INDEX
	mov 	ax,0ff02h
	out     dx,ax		

	mov bx,[ScreenBufferSeg]
	mov	es,bx

	cld

	mov	eax,022222222h
	xor	edi,edi
	mov	ecx,SCREEN_SIZE/4
	rep	stosd

	mov	eax,033333333h
	mov	edi,SCREEN_SIZE
	mov	ecx,SCREEN_SIZE/4
	rep	stosd
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		 
		mov		bl,3
		mov		ax,0123h
		call	Hex16

		mov		bl,5
		mov		ax,4567h
		call	Hex16

		mov		bl,7
		mov		ax,089abh
		call	Hex16

		mov		bl,9
		mov		ax,0cdefh
		call	Hex16

		mov		bl,11
		mov		ax,[ScreenBuffer]
		call	Hex16

	mov	cx,3
	call	Move
	
	mov	cx,3
	call	Plots
		
	mov	al,55h ; 100
	mov	cx,3
	lea		si,[PointList2]
	call	Fill
		
	mov	al,33h ; 100
	mov	cx,3
	lea		si,[PointList]
	call	Fill

		call	GetKey
	jnc		MainLoop

Done: ; Finished, clear key, reset screen mode and exit.
	mov	ax,3		; reset to text mode
	int		10h
	
	mov	ah,4ch	; exit to DOS
	int		21h

; Wait for the leading edge of vertical sync pulse.
;
;WaitVSync	proc	near
;	mov	dx,INPUT_STATUS_1
;WaitNotVSyncLoop:
;	in	al,dx
;	and	al,VSYNC_MASK
;	jnz	WaitNotVSyncLoop
;WaitVSyncLoop:
;	in	al,dx
;	and	al,VSYNC_MASK
;	jz	WaitVSyncLoop
;	ret
;WaitVSync	endp

;/* Wait until we're not in vertical sync, so we can catch leading edge */
;      while ((inp(INPUT_STATUS_1) & 0x08) != 0) ;
;      /* Wait until we are in vertical sync */
;      while ((inp(INPUT_STATUS_1) & 0x08) == 0) ;

; When the offset registers are changed, the page flip does not occur until the end of the next vertical retrace. So after the page is flipped, the program should wait until the end of the vertical retrace before drawing to the non-visible page.

WaitVSync: ; Wait for the leading edge of vertical sync pulse.

				mov	dx,INPUT_STATUS_1
WaitNotVSyncLoop:		in		al,dx
					and	al,VSYNC_MASK 	; is bit 3 clear so its not in the vertical retrace?
					jnz		WaitNotVSyncLoop 	; no its set

;				mov	dx,INPUT_STATUS_1
WaitVSyncLoop:	in	al,dx
					and	al,VSYNC_MASK 	; is bit 3 set so its in the vertical retrace?;
					jz	WaitVSyncLoop 	; no its clear

				mov 	bx,[ScreenBuffer]
				test	bx,bx
				jnz		.page1

.page0:				mov		ax,VIDEO_SEGMENT_PAGE_1
					mov 	bx,SCREEN_SIZE
					jmp 	.cont2

.page1:				mov		ax,VIDEO_SEGMENT_PAGE_0
					xor		bx,bx

.cont2:			mov 	[ScreenBuffer],bx
				mov		[ScreenBufferSeg],ax

				mov dx, CRTC_INDEX
				mov al, LOW_ADDRESS
				mov ah,bl
				out dx,ax
				mov al, HIGH_ADDRESS
				mov ah,bh
				out dx,ax

				mov     dx, SC_INDEX
				mov 	ax, 0ff02h 
				out     dx, ax		

				mov	ax,[ScreenBufferSeg]
				mov	es,ax
				
				cld
				xor	eax,eax
				xor	edi,edi
				mov	ecx,SCREEN_SIZE/4
				rep	stosd

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

Mode360x240:
		mov    ax,12h
		int    10h          ; mode 12h to supposedly let the BIOS clear the video memory

		mov     ax, 0013h	; 1. Set the BIOS mode 13h, which is the standard 256-color mode.
        int     10h
  		
        mov     dx, SC_INDEX
        mov     ax, 0604h
        out     dx, ax		; 2. Put the CHAIN4-mode of Sequencer off
  
        mov     ax, 0F02h
        out     dx, ax
        mov     dx, VIDEO_SEGMENT
        mov     es, dx
        xor     di, di
        xor     ax, ax
        mov     cx, 8000h
        rep     stosw		; 3. Clear the video memory (setting mode 13h clears only every fourth byte from each plane)
 
        mov     dx, 03D4h
        mov     ax, 0014h
        out     dx, ax		; 4. Turn off the CRTC's LONG-mode
  
        mov     ax, 0E317h
        out     dx, ax		; 5. Turn on the CRTC's BYTE-mode
	

		mov		ax,	02c11h	; word_out(CRTC_INDEX, V_RETRACE_END, 0x2c); 0x11
		out		dx,	ax
		
        mov     dx, 032ch
		mov		al,	0e7h		;    outp(MISC_OUTPUT, 0xe7);
		out		dx,al

        mov     dx, 003D4h
		mov		ax,	0006bh
		out		dx,ax		;    word_out(CRTC_INDEX, H_TOTAL, 0x6b); 0x00

		mov		ax, 05901h	
		out		dx,ax		;    word_out(CRTC_INDEX, H_DISPLAY_END, 0x59); 0x01

		mov		ax,05a02h	
		out		dx,ax		;    word_out(CRTC_INDEX, H_BLANK_START, 0x5a); 0x02

		mov		ax,08e03h	
		out		dx,ax		;    word_out(CRTC_INDEX, H_BLANK_END, 0x8e); 0x03

		mov		ax,05e04h	
		out		dx,ax		;    word_out(CRTC_INDEX, H_RETRACE_START, 0x5e); 0x04

		mov		ax,08a05h
		out		dx,ax		;    word_out(CRTC_INDEX, H_RETRACE_END, 0x8a); 0x05

		mov		ax,02d13h
		out		dx,ax		;    word_out(CRTC_INDEX, OFFSET, 0x2d); 0x13

		mov		ax,08e11h
		out		dx,ax		;    word_out(CRTC_INDEX, V_RETRACE_END, 0x8e); ; 0x11 set vertical retrace back to normal */

		mov		ax,02c11h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_RETRACE_END, 0x2c); ; 0x11

		mov		ax,0d06h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_TOTAL, 0x0d); 0x06

		mov		ax,03e07h	
		out		dx,ax		;    word_out(CRTC_INDEX, OVERFLOW, 0x3e); 0x07

		mov		ax,0ea10h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_RETRACE_START, 0xea); 0x10

		mov		ax,0ac11h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_RETRACE_END, 0xac); 0x11

		mov		ax,0df12h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_DISPLAY_END, 0xdf); 0x12

		mov		ax,0e715h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_BLANK_START, 0xe7); 0x15

		mov		ax,0616h	
		out		dx,ax		;    word_out(CRTC_INDEX, V_BLANK_END, 0x06); 0x16
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
.testyl		cmp 	ax,239
		jne 	.testxr
			neg 	dx
.testxr		cmp 	bx,359
		jne 	.testyr
			neg 	bp
.testyr		mov 	[di],dx
		mov	[di+2],bp
 
		add	si,4
		add	di,4
		dec	cx
		jne	.loop
	ret

Plot:	; bp=v / bx=h / ch=col

	mov dx,[ScreenBufferSeg]
	mov es,dx

	mov		cl,	bl	; backup
	shr		bx,	2	; /4
	add		bp,	bp 	; start ver *2
	add		bx,	[BackBufferTable360+bp]	; ver start
;	add		bx, [ScreenBuffer]

	mov		ah,	1 ; move ax,0102h
	and 	cl,	3
	shl		ah,	cl
	mov		al,	02h ; not needed if above implemented?
	mov     dx, SC_INDEX
	out     dx, ax		; 2. Put the CHAIN4-mode of Sequencer off
	
	mov		[es:bx],ch
	ret

Plots:	lea 	si,[PointList]
 
.loop	push	cx
		mov	bp,[si]
		mov	bx,[si+2]
		mov		ch,66h
		call 	Plot
		pop		cx
 
		add	si,4
		dec	cl
		jne	.loop
	ret

Fill:	mov		[Colour],al		; backup colour
	
;;		lea		si,[PointList]		; point list
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
					add	di,4		; add 4 for right edge

.noswap:		cmp 	bl,al 	; smallest xpos
				jc 	.low
					mov 	bl,al 	; yes

.low:			cmp 	bh,cl	; biggest xpos
				jnc 	.high
					mov		bh,cl 	; yes

.high			sub		cx,ax		; vdiff

				shl		ax,3		; *8
				add		di,ax		; added to di
	
;				mov		gs,dx		; negate set to positive
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
				jc		.exit			; all on same line?

				mov		cl,[Colour]
				mov 	ch,cl
				shl 	ecx,8
				mov 	cl,ch
				shl 	ecx,8
				mov 	cl,ch

				mov		cl,bh
				xor		bh,bh
				add		bx,bx 			; start ver *2
				mov		si,[BackBufferTable360+bx]	; start of ver line
				shl		bx,2			; start ver *4*8
				lea		bp,[EdgeTable+bx+2]	; point to first 

				mov     dx,SC_INDEX

.fillloop:			push bp
 					mov		di,[bp+4]
					mov		bp,[bp]

					cmp		bp,di
					jnc		.no_swap
						xchg 	bp, di

.no_swap:			mov		ax,di
					mov		bx,bp

					shr		ax,2
					shr		bx,2
					sub		bx,ax
					add		bx,bx
					mov		ax,[FillJumpTable+bx]
	mov	ebx,ecx		; backup count/colour
					JMP		(ax)

					fill_cont
.exit			ret

Jump000:	fill_cont
			ret

Jump004:	left_edge
			right_edge
			fill_cont
			ret

Jump008:	left_edge
			middle
			mov 	al,ch
			stosb
			right_edge
			fill_cont
			ret

Jump012:	mov 	cl,ch
			left_edge
			middle
			mov 	ax,cx
			stosw
			right_edge
			fill_cont
			ret

Jump016:	mov 	cl,ch
			left_edge
			middle
			mov 	ax,cx
			stosw
			stosb
			right_edge
			fill_cont
			ret

Jump020:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosd
			right_edge
			fill_cont
			ret

Jump024:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosd
			right_edge
			fill_cont
			ret

Jump028:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			stosd
			right_edge
			fill_cont
			ret

Jump032:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			stosd
			right_edge
			fill_cont
			ret

Jump036:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,2
			rep		stosd
			right_edge
			fill_cont
			ret

Jump040:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov	cx,2
			rep	stosd
			right_edge
			fill_cont
			ret

Jump044:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov	cx,2
			rep	stosd
			right_edge
			fill_cont
			ret

Jump048:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,2
	rep	stosd
	right_edge
			fill_cont
			ret

Jump052:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,3
	rep	stosd
	right_edge
			fill_cont
			ret

Jump056:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,3
	rep	stosd
	right_edge
			fill_cont
			ret

Jump060:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,3
	rep	stosd
	right_edge
			fill_cont
			ret

Jump064:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,3
	rep	stosd
	right_edge
			fill_cont
			ret

Jump068:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,4
	rep	stosd
	right_edge
			fill_cont
			ret

Jump072:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,4
	rep	stosd
	right_edge
			fill_cont
			ret

Jump076:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,4
	rep	stosd
	right_edge
			fill_cont
			ret

Jump080:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,4
	rep	stosd
	right_edge
			fill_cont
			ret

Jump084:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,5
	rep	stosd
	right_edge
			fill_cont
			ret

Jump088:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,5
	rep	stosd
	right_edge
			fill_cont
			ret

Jump092:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,5
	rep	stosd
	right_edge
			fill_cont
			ret

Jump096:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,5
	rep	stosd
	right_edge
			fill_cont
			ret

Jump100:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,6
	rep	stosd
	right_edge
			fill_cont
			ret

Jump104:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,6
	rep	stosd
	right_edge
			fill_cont
			ret

Jump108:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,6
	rep	stosd
	right_edge
			fill_cont
			ret

Jump112:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,6
	rep	stosd
	right_edge
			fill_cont
			ret

Jump116:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,7
	rep	stosd
	right_edge
			fill_cont
			ret

Jump120:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,7
	rep	stosd
	right_edge
			fill_cont
			ret

Jump124:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,7
	rep	stosd
	right_edge
			fill_cont
			ret

Jump128:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,7
	rep	stosd
	right_edge
			fill_cont
			ret

Jump132:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,8
	rep	stosd
	right_edge
			fill_cont
			ret

Jump136:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,8
	rep	stosd
	right_edge
			fill_cont
			ret

Jump140:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,8
	rep	stosd
	right_edge
			fill_cont
			ret

Jump144:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,8
	rep	stosd
	right_edge
			fill_cont
			ret

Jump148:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,9
	rep	stosd
	right_edge
			fill_cont
			ret

Jump152:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,9
	rep	stosd
	right_edge
			fill_cont
			ret

Jump156:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,9
	rep	stosd
	right_edge
			fill_cont
			ret

Jump160:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,9
	rep	stosd
	right_edge
			fill_cont
			ret

Jump164:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,10
	rep	stosd
	right_edge
			fill_cont
			ret

Jump168:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,10
	rep	stosd
	right_edge
			fill_cont
			ret

Jump172:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,10
	rep	stosd
	right_edge
			fill_cont
			ret

Jump176:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,10
	rep	stosd
	right_edge
			fill_cont
			ret

Jump180:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,11
	rep	stosd
	right_edge
			fill_cont
			ret

Jump184:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,11
	rep	stosd
	right_edge
			fill_cont
			ret

Jump188:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,11
	rep	stosd
	right_edge
			fill_cont
			ret

Jump192:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,11
	rep	stosd
	right_edge
			fill_cont
			ret

Jump196:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,12
	rep	stosd
	right_edge
			fill_cont
			ret

Jump200:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,12
	rep	stosd
	right_edge
			fill_cont
			ret

Jump204:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,12
	rep	stosd
	right_edge
			fill_cont
			ret

Jump208:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,12
	rep	stosd
	right_edge
			fill_cont
			ret

Jump212:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,13
	rep	stosd
	right_edge
			fill_cont
			ret

Jump216:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,13
	rep	stosd
	right_edge
			fill_cont
			ret

Jump220:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,13
	rep	stosd
	right_edge
			fill_cont
			ret

Jump224:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,13
	rep	stosd
	right_edge
			fill_cont
			ret

Jump228:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,14
	rep	stosd
	right_edge
			fill_cont
			ret

Jump232:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,14
	rep	stosd
	right_edge
			fill_cont
			ret

Jump236:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,14
	rep	stosd
	right_edge
			fill_cont
			ret

Jump240:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,14
	rep	stosd
	right_edge
			fill_cont
			ret

Jump244:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,15
	rep	stosd
	right_edge
			fill_cont
			ret

Jump248:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,15
	rep	stosd
	right_edge
			fill_cont
			ret

Jump252:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosw
	mov	cx,15
	rep	stosd
	right_edge
			fill_cont
			ret

Jump256:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	stosw
	mov	cx,15
	rep	stosd
	right_edge
			fill_cont
			ret

Jump260:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	mov	cx,16
	rep	stosd
	right_edge
			fill_cont
			ret

Jump264:	mov 	cl,ch
	left_edge
	middle
	mov 	eax,ecx
	stosb
	mov	cx,16
	rep	stosd
	right_edge
			fill_cont
			ret

Jump268:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,16
			rep		stosd
			right_edge
			fill_cont
			ret

Jump272:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,16
			rep		stosd
			right_edge
			fill_cont
			ret

Jump276:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,17
			rep		stosd
			right_edge
			fill_cont
			ret

Jump280:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov		cx,17
			rep		stosd
			right_edge
			fill_cont
			ret

Jump284:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,17
			rep		stosd
			right_edge
			fill_cont
			ret

Jump288:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,17
			rep		stosd
			right_edge
			fill_cont
			ret

Jump292:
			mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,18
			rep		stosd
			right_edge
			fill_cont
			ret

Jump296:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov		cx,18
			rep		stosd
			right_edge
			fill_cont
			ret

Jump300:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,18
			rep		stosd
			right_edge
			fill_cont
			ret

Jump304:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,18
			rep		stosd
			right_edge
			fill_cont
			ret

Jump308:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,19
			rep		stosd
			right_edge
			fill_cont
			ret

Jump312:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov		cx,19
			rep		stosd
			right_edge
			fill_cont
			ret

Jump316:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,19
			rep		stosd
			right_edge
			fill_cont
			ret

Jump320:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,19
			rep		stosd
			right_edge
			fill_cont
			ret

Jump324:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,20
			rep		stosd
			right_edge
			fill_cont
			ret

Jump328:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov		cx,20
			rep		stosd
			right_edge
			fill_cont
			ret

Jump332:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,20
			rep		stosd
			right_edge
			fill_cont
			ret

Jump336:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,20
			rep		stosd
			right_edge
			fill_cont
			ret

Jump340:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,21
			rep		stosd
			right_edge
			fill_cont
			ret

Jump344:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov		cx,21
			rep		stosd
			right_edge
			fill_cont
			ret

Jump348:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,21
			rep		stosd
			right_edge
			fill_cont
			ret

Jump352:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,21
			rep		stosd
			right_edge
			fill_cont
			ret

Jump356:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			mov		cx,22
			rep		stosd
			right_edge
			fill_cont
			ret

Jump360:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			mov		cx,22
			rep		stosd
			right_edge
			fill_cont
			ret

Jump364:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosw
			mov		cx,22
			rep		stosd
			right_edge
			fill_cont
			ret

Jump368:	mov 	cl,ch
			left_edge
			middle
			mov 	eax,ecx
			stosb
			stosw
			mov		cx,22
			rep		stosd
			right_edge
			fill_cont
			ret

; 0123 = 

; 0000000000111111111122222222223333333333444444444455555555556666666666777777777788888888889999999999
; 0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789



HexChar:	mov		si,di
			mov		bl,al
			shr		bl,1
			shr		bl,1
			shr		bl,1
			shr		bl,1
			and		bx,15
			add		bx,bx
			add		bx,bx
 push ax
			mov		eax,[Hex0+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex1+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex2+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex3+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex4+bx]
			mov		[es:di],eax
 pop ax
			mov		di,si
			add		di,4

			mov		bl,al
			and		bl,15
			add		bx,bx
			add		bx,bx
			mov		eax,[Hex0+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex1+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex2+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex3+bx]
			mov		[es:di],eax
			add		di,SCREEN_WIDTH_360
			mov		eax,[Hex4+bx]
			mov		[es:di],eax
			ret

Hex24:		push	ax
			push	bx

			mov		al,bh
			call	Hex8
						
			pop		bx
			pop		ax

			push	ax
			push	bx

			mov		di,8
			call	HexByte

			pop		bx
			pop		ax

			mov		al,ah
			mov		di,8
			jmp		HexByte	; does ret

Hex16:		push	ax
			push	bx

			mov		al,ah
			call	Hex8
						
			pop		bx
			pop		ax

			mov		di,8
			jmp		HexByte ; does ret

Hex8:		xor		di,di
HexByte:	xor		bh,bh
			mov		ah,bl
			add		ah,ah	; *2
			add		bl,ah	; *3
			add		bx,bx	; *6
			add		bx,bx	; *12
			add		di,[BackBufferTable360+bx]	; lookup start ver
;				add		di, [ScreenBuffer]

			jmp		HexChar	 ; does ret
;			ret


section .data 
align	2 ; 16 ; 8 ; 16
	
BackBufferTable360:
		dw SCREEN_WIDTH_360*000,SCREEN_WIDTH_360*001,SCREEN_WIDTH_360*002,SCREEN_WIDTH_360*003,SCREEN_WIDTH_360*004,SCREEN_WIDTH_360*005,SCREEN_WIDTH_360*006,SCREEN_WIDTH_360*007,SCREEN_WIDTH_360*008,SCREEN_WIDTH_360*009
		dw SCREEN_WIDTH_360*010,SCREEN_WIDTH_360*011,SCREEN_WIDTH_360*012,SCREEN_WIDTH_360*013,SCREEN_WIDTH_360*014,SCREEN_WIDTH_360*015,SCREEN_WIDTH_360*016,SCREEN_WIDTH_360*017,SCREEN_WIDTH_360*018,SCREEN_WIDTH_360*019
		dw SCREEN_WIDTH_360*020,SCREEN_WIDTH_360*021,SCREEN_WIDTH_360*022,SCREEN_WIDTH_360*023,SCREEN_WIDTH_360*024,SCREEN_WIDTH_360*025,SCREEN_WIDTH_360*026,SCREEN_WIDTH_360*027,SCREEN_WIDTH_360*028,SCREEN_WIDTH_360*029
		dw SCREEN_WIDTH_360*030,SCREEN_WIDTH_360*031,SCREEN_WIDTH_360*032,SCREEN_WIDTH_360*033,SCREEN_WIDTH_360*034,SCREEN_WIDTH_360*035,SCREEN_WIDTH_360*036,SCREEN_WIDTH_360*037,SCREEN_WIDTH_360*038,SCREEN_WIDTH_360*039
		dw SCREEN_WIDTH_360*040,SCREEN_WIDTH_360*041,SCREEN_WIDTH_360*042,SCREEN_WIDTH_360*043,SCREEN_WIDTH_360*044,SCREEN_WIDTH_360*045,SCREEN_WIDTH_360*046,SCREEN_WIDTH_360*047,SCREEN_WIDTH_360*048,SCREEN_WIDTH_360*049
		dw SCREEN_WIDTH_360*050,SCREEN_WIDTH_360*051,SCREEN_WIDTH_360*052,SCREEN_WIDTH_360*053,SCREEN_WIDTH_360*054,SCREEN_WIDTH_360*055,SCREEN_WIDTH_360*056,SCREEN_WIDTH_360*057,SCREEN_WIDTH_360*058,SCREEN_WIDTH_360*059
		dw SCREEN_WIDTH_360*060,SCREEN_WIDTH_360*061,SCREEN_WIDTH_360*062,SCREEN_WIDTH_360*063,SCREEN_WIDTH_360*064,SCREEN_WIDTH_360*065,SCREEN_WIDTH_360*066,SCREEN_WIDTH_360*067,SCREEN_WIDTH_360*068,SCREEN_WIDTH_360*069
		dw SCREEN_WIDTH_360*070,SCREEN_WIDTH_360*071,SCREEN_WIDTH_360*072,SCREEN_WIDTH_360*073,SCREEN_WIDTH_360*074,SCREEN_WIDTH_360*075,SCREEN_WIDTH_360*076,SCREEN_WIDTH_360*077,SCREEN_WIDTH_360*078,SCREEN_WIDTH_360*079
		dw SCREEN_WIDTH_360*080,SCREEN_WIDTH_360*081,SCREEN_WIDTH_360*082,SCREEN_WIDTH_360*083,SCREEN_WIDTH_360*084,SCREEN_WIDTH_360*085,SCREEN_WIDTH_360*086,SCREEN_WIDTH_360*087,SCREEN_WIDTH_360*088,SCREEN_WIDTH_360*089
		dw SCREEN_WIDTH_360*090,SCREEN_WIDTH_360*091,SCREEN_WIDTH_360*092,SCREEN_WIDTH_360*093,SCREEN_WIDTH_360*094,SCREEN_WIDTH_360*095,SCREEN_WIDTH_360*096,SCREEN_WIDTH_360*097,SCREEN_WIDTH_360*098,SCREEN_WIDTH_360*099

		dw SCREEN_WIDTH_360*100,SCREEN_WIDTH_360*101,SCREEN_WIDTH_360*102,SCREEN_WIDTH_360*103,SCREEN_WIDTH_360*104,SCREEN_WIDTH_360*105,SCREEN_WIDTH_360*106,SCREEN_WIDTH_360*107,SCREEN_WIDTH_360*108,SCREEN_WIDTH_360*109
		dw SCREEN_WIDTH_360*110,SCREEN_WIDTH_360*111,SCREEN_WIDTH_360*112,SCREEN_WIDTH_360*113,SCREEN_WIDTH_360*114,SCREEN_WIDTH_360*115,SCREEN_WIDTH_360*116,SCREEN_WIDTH_360*117,SCREEN_WIDTH_360*118,SCREEN_WIDTH_360*119
		dw SCREEN_WIDTH_360*120,SCREEN_WIDTH_360*121,SCREEN_WIDTH_360*122,SCREEN_WIDTH_360*123,SCREEN_WIDTH_360*124,SCREEN_WIDTH_360*125,SCREEN_WIDTH_360*126,SCREEN_WIDTH_360*127,SCREEN_WIDTH_360*128,SCREEN_WIDTH_360*129
		dw SCREEN_WIDTH_360*130,SCREEN_WIDTH_360*131,SCREEN_WIDTH_360*132,SCREEN_WIDTH_360*133,SCREEN_WIDTH_360*134,SCREEN_WIDTH_360*135,SCREEN_WIDTH_360*136,SCREEN_WIDTH_360*137,SCREEN_WIDTH_360*138,SCREEN_WIDTH_360*139
		dw SCREEN_WIDTH_360*140,SCREEN_WIDTH_360*141,SCREEN_WIDTH_360*142,SCREEN_WIDTH_360*143,SCREEN_WIDTH_360*144,SCREEN_WIDTH_360*145,SCREEN_WIDTH_360*146,SCREEN_WIDTH_360*147,SCREEN_WIDTH_360*148,SCREEN_WIDTH_360*149
		dw SCREEN_WIDTH_360*150,SCREEN_WIDTH_360*151,SCREEN_WIDTH_360*152,SCREEN_WIDTH_360*153,SCREEN_WIDTH_360*154,SCREEN_WIDTH_360*155,SCREEN_WIDTH_360*156,SCREEN_WIDTH_360*157,SCREEN_WIDTH_360*158,SCREEN_WIDTH_360*159
		dw SCREEN_WIDTH_360*160,SCREEN_WIDTH_360*161,SCREEN_WIDTH_360*162,SCREEN_WIDTH_360*163,SCREEN_WIDTH_360*164,SCREEN_WIDTH_360*165,SCREEN_WIDTH_360*166,SCREEN_WIDTH_360*167,SCREEN_WIDTH_360*168,SCREEN_WIDTH_360*169
		dw SCREEN_WIDTH_360*170,SCREEN_WIDTH_360*171,SCREEN_WIDTH_360*172,SCREEN_WIDTH_360*173,SCREEN_WIDTH_360*174,SCREEN_WIDTH_360*175,SCREEN_WIDTH_360*176,SCREEN_WIDTH_360*177,SCREEN_WIDTH_360*178,SCREEN_WIDTH_360*179
		dw SCREEN_WIDTH_360*180,SCREEN_WIDTH_360*181,SCREEN_WIDTH_360*182,SCREEN_WIDTH_360*183,SCREEN_WIDTH_360*184,SCREEN_WIDTH_360*185,SCREEN_WIDTH_360*186,SCREEN_WIDTH_360*187,SCREEN_WIDTH_360*188,SCREEN_WIDTH_360*189
		dw SCREEN_WIDTH_360*190,SCREEN_WIDTH_360*191,SCREEN_WIDTH_360*192,SCREEN_WIDTH_360*193,SCREEN_WIDTH_360*194,SCREEN_WIDTH_360*195,SCREEN_WIDTH_360*196,SCREEN_WIDTH_360*197,SCREEN_WIDTH_360*198,SCREEN_WIDTH_360*199

		dw SCREEN_WIDTH_360*200,SCREEN_WIDTH_360*201,SCREEN_WIDTH_360*202,SCREEN_WIDTH_360*203,SCREEN_WIDTH_360*204,SCREEN_WIDTH_360*205,SCREEN_WIDTH_360*206,SCREEN_WIDTH_360*207,SCREEN_WIDTH_360*208,SCREEN_WIDTH_360*209
		dw SCREEN_WIDTH_360*210,SCREEN_WIDTH_360*211,SCREEN_WIDTH_360*212,SCREEN_WIDTH_360*213,SCREEN_WIDTH_360*214,SCREEN_WIDTH_360*215,SCREEN_WIDTH_360*216,SCREEN_WIDTH_360*217,SCREEN_WIDTH_360*218,SCREEN_WIDTH_360*219
		dw SCREEN_WIDTH_360*220,SCREEN_WIDTH_360*221,SCREEN_WIDTH_360*222,SCREEN_WIDTH_360*223,SCREEN_WIDTH_360*224,SCREEN_WIDTH_360*225,SCREEN_WIDTH_360*226,SCREEN_WIDTH_360*227,SCREEN_WIDTH_360*228,SCREEN_WIDTH_360*229
		dw SCREEN_WIDTH_360*230,SCREEN_WIDTH_360*231,SCREEN_WIDTH_360*232,SCREEN_WIDTH_360*233,SCREEN_WIDTH_360*234,SCREEN_WIDTH_360*235,SCREEN_WIDTH_360*236,SCREEN_WIDTH_360*237,SCREEN_WIDTH_360*238,SCREEN_WIDTH_360*239

FillJumpTable:	dw	Jump000,Jump004,Jump008,Jump012,Jump016,Jump020,Jump024,Jump028,Jump032,Jump036,Jump040,Jump044,Jump048,Jump052,Jump056,Jump060,Jump064,Jump068,Jump072,Jump076,Jump080,Jump084
				dw	Jump088,Jump092,Jump096,Jump100,Jump104,Jump108,Jump112,Jump116,Jump120,Jump124,Jump128,Jump132,Jump136,Jump140,Jump144,Jump148,Jump152,Jump156,Jump160,Jump164,Jump168,Jump172
				dw	Jump176,Jump180,Jump184,Jump188,Jump192,Jump196,Jump200,Jump204,Jump208,Jump212,Jump216,Jump220,Jump224,Jump228,Jump232,Jump236,Jump240,Jump244,Jump248,Jump252,Jump256,Jump260
				dw	Jump264,Jump268,Jump272,Jump276,Jump280,Jump284,Jump288,Jump292,Jump296,Jump300,Jump304,Jump308,Jump312,Jump316,Jump320,Jump324,Jump328,Jump332,Jump336,Jump340,Jump344,Jump348
				dw	Jump352,Jump356,Jump360,Jump364

Hex0	dd 000555555h,000005500h,000555555h,000555555h,000550055h,000555555h,000555555h,000555555h,000555555h,000555555h,000555555h,000555555h,000555555h,000005555h,000555555h,000555555h
Hex1	dd 000550055h,000005555h,000550000h,000550000h,000550055h,000000055h,000000055h,000550000h,000550055h,000550055h,000550055h,000550055h,000000055h,000550055h,000000055h,000000055h
Hex2	dd 000550055h,000005500h,000555555h,000555555h,000555555h,000555555h,000555555h,000550000h,000555555h,000555555h,000555555h,000005555h,000000055h,000550055h,000555555h,000555555h
Hex3	dd 000550055h,000005500h,000000055h,000550000h,000550000h,000550000h,000550055h,000550000h,000550055h,000550000h,000550055h,000550055h,000000055h,000550055h,000000055h,000000055h
Hex4	dd 000555555h,000555555h,000555555h,000555555h,000550000h,000555555h,000555555h,000550000h,000555555h,000555555h,000550055h,000555555h,000555555h,000005555h,000555555h,000000055h

align	4
PointList: dw 0b8h, 13ch, 22h, 0ebh, 0c2h, 016h, 0, 0

PointList2: dw 0b8h, 13ch, 22h, 0ebh, 0c2h, 016h, 0, 0
 
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

DirList:
 dw 1,-1,-1,1,-1,1
 
; dw 1,1,-1,1,1,-1
 
; dw 1,-1
; dw -1,1
; dw -1,1
 
 dw	1,1
 dw	-1,1
 dw	1,-1
 dw 	+1,-1,+1,-1,+1,-1,+1,-1,+1,-1,+1,-1
 
edgeleft:  times 360/4 db 15-1, 15-(2+1), 15-(4+2+1), 15-(8+4+2+1)
edgeright: times 360/4 db 1, (2+1), (4+2+1), (8+4+2+1) ; , (4+2+1), (2+1), 1
		
section .bss 	; put uninitialized data here
align	4
EdgeTable	resd	800

ScreenBuffer	resw	1
ScreenBufferSeg	resw	1
Colour		resb	1

; table of jmps to code based on number of dwords to draw? 360/4 jumps?

; 0 in same 4 bits

; hstart / 4
; hend / 4
; hdiff = hend - hstart
; lookup jumptable 
; jump to code to do

; %macro  prologue 1 
;        push    ebp 
;        mov     ebp,esp 
;        sub     esp,%1 
; %endmacro
; This defines a C-like function prologue as a macro: so you would invoke the macro with a call such as:
; myfunc:   prologue 12

; repeat
; buffer:        times 90 db 0