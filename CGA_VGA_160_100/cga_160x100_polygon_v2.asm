; nasm cga_line_v7.asm -o cgaline.com -f bin

VIDEO_SEGMENT	equ	0b800h 	; display memory segment for true CGA graphics modes

INPUT_STATUS_1	equ	03dah	; VGA status register
VSYNC_MASK	equ	08h	; vertical sync bit in status register 1
DE_MASK		equ	01h	; display enable bit in status register 1

MAP_MASK	equ	2		; SC map mask register
SC_INDEX		equ	3c4h	; SC index register

SCR_WIDTH equ 160

KEY_1	EQU	0231h
KEY_2	EQU	0332h
KEY_3	EQU	0433h
KEY_4 	EQU	0534h
KEY_5 	EQU	0635h
KEY_6 	EQU	0736h
KEY_ESC	EQU 27

CPU 486
bits 16

CRTCa EQU 03d8h	; 6845 mode control register 0x3d8
CRTCb EQU 03d4h ; 6845 index register
SR	  EQU 0dah	; Input Status Register
TEXT  EQU 3   ; DOS video mode for 80x25 text

;%macro fill_cont	0
;	pop		di
;	jnz		Fill.plotloop
;%endmacro

%include "exebin.mac"

EXE_begin

section .text

start:	call	Set160x100
		call	SetBuffers
		call	SetScreen

		xor al,al
		mov	[Exit],al
	
MainLoop:	call	WaitVSync 				; Wait for vertical sync so the new start address has a chance to take effect.
			call	CopyClearBackBufferV8 	; copy back to front

	mov	cx,3
	call	Move

	mov	cx,3
	call	Plots

	mov		bp,10
	mov		bx,10
	mov		al,0ffh
;	call	Plot
	
	mov		bp,12
	mov		bx,80
	mov		al,022h
;	call	Plot
	
	mov		bp,98
	mov		bx,158
	mov		al,0ffh
;	call	Plot

	mov		bp,99
	mov		bx,159
	mov		al,0ffh
;	call	Plot

	mov		bp,[testx]
	mov		bx,[testy]
	mov		al,[colour]
;	mov		al,0ffh
	call	Plot

			call	GetKey
		
			mov		al,[Exit]
			or		al,al
			je		MainLoop

Done:	call	Mode3
		mov		ah,4ch	; exit to DOS
		int		21h

AllocMem:	shr		bx, 4	; divide memory required into paragraphs
			inc		bx		; add 1 
			mov		ah, 48h
			int		21h
;			jnc		.ok
;				xor		ax,ax	; reset
.ok			ret

SetBuffers:

;	mov		bx,	0ffffh
;	call	AllocMem

	mov		bx,	16000 ; 8000 words (160x100 1 char byte 1 attribute byte)
	call	AllocMem
	mov	[BackBufferSeg], ax

	mov		bx,	16000
	call	AllocMem
	mov	[ScreenBufferSeg], ax

				xor 	eax,eax 	; mov		ax,00101101001011010b ;

;				mov		di,[BackBufferSeg]		; es points to ds
;				mov		es,di
;				xor		edi,edi					; DS:SI points to back buffer	
;				mov		ecx,2000
;				rep		stosd					; clear back buffer

;				mov		di,[ScreenBufferSeg]	; es points to ds
;				mov		es,di
;				xor		di,di					; DS:SI points to screen buffer	
;				mov		cx,2000
;				rep		stosd					; clear screen buffer

				ret

Mode3:	mov		ax,TEXT	; reset to text mode
		int		10h
		ret

plot:

		ret

VGA160x100:	mov     dx, SR		; status=inportb(SR)		; Read the input status register to reset the VGA attribute controller index/data state */
			in     	al,dx

			mov     dx, 0x3c0	; outportb(0x3c0,0x10)	; VGA attribute controller index register, mode register select */
			mov     al, 010h
			out     dx, al

			mov     dx, 0x3c1	; status=inportb(0x3c1)	; VGA attribute controller data register, read mode register */
			in     	al,dx

			and		al,0f7h		; status&=0xf7			; turn off bit 3, blink */
			mov		[status],al

			mov     dx, 0x3c0	; outportb(0x3c0,0x10)	; select the attribute control register */
			mov     al, 010h
			out     dx, al

			mov     dx, 0x3c0	;	outportb(0x3c0,status)	; write to VGA attribute controller data register */
			mov     al, [status]
			out     dx, al

			mov     ax, 0x0903		;	write_crtc(CRTCb,0x09,0x03)	; VGA has an 8x16 character cell and 4 lines makes a square since VGA has a 1:1 pixel aspect ratio */
			call    write_cga_reg

		ret 

CGA160x100:	call	Mode3		; setting 80-column text mode

			mov     dx, CRTCa		; outportb(CRTCa, 1) 			; set mode control register for 80x25 text mode and disable video output */
			mov     al, 1
			out     dx, al

			mov     ax, 0x047f      ; write_crtc(CRTCb,0x04,0x7f) ; set vert total lines to 127 */
			call    write_cga_reg
			mov     ax, 0x0664      ; write_crtc(CRTCb,0x06,0x64) ; set vert displayed char rows to 100 */
			call    write_cga_reg
			mov     ax, 0x0770      ; write_crtc(CRTCb,0x07,0x70) ; set vert sync position to 112 */
			call    write_cga_reg
			mov     ax, 0x0901      ; write_crtc(CRTCb,0x09,0x01) ; set char scan line count to 1 */
			call    write_cga_reg
		
			mov     dx, CRTCa		; outportb(CRTCa, 9) 			; re-enable the video output in 80x25 text mode */
			mov     al, 9
			out     dx, al

			ret
	
write_cga_reg:	mov     dx, CRTCb	; Program the CRT controller
				xchg	al, ah
				out     dx, ax
				ret

Set160x100:	call	Mode3			; setting 80-column text mode

			mov		AH,0x12			;
			mov 	AL,0			;
			mov 	BL,0x10			;
			int		10h
			mov		[switches],CL	;

			mov		AH,0x1a			;
			mov		AL,0			;
			int		10h

			mov		[active],BL 	;
			mov		[status],AL 	;

.vga:		mov		al,[status]
			cmp		al,01ah
			jne		.ega

				mov		al,[active]
				cmp		al,07
				je		.vga_ok
				cmp		al,08
				jne		.ega

.vga_ok:			call	VGA160x100
					ret

.ega:	; check for ega

.cga:		mov		ax,0f00h 	; get current display settings from BIOS
			int		10h
	
			cmp		al,7		; Check that this is not BW 80x25
			je		.exit

				call	CGA160x100	; do cga change
.exit		ret

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
	mov ax,[testx]
	dec ax
	mov [testx],ax
	ret
.key_2:
	mov ax,[testx]
	inc ax
	mov [testx],ax
	ret
.key_3:
	mov ax,[testy]
	dec ax
	mov [testy],ax
	ret
.key_4:
	mov ax,[testy]
	inc ax
	mov [testy],ax
	ret
.key_5:
	mov al,[colour]
	sub al,011h
	mov [colour],al
	ret
.key_6:
	mov al,[colour]
	add al,011h
	mov [colour],al
	ret

WaitVSync: 		mov		dx,INPUT_STATUS_1	; Wait for the leading edge of vertical sync pulse.
WaitNotVSyncLoop:	in		al,dx
					and		al,VSYNC_MASK
					jnz		WaitNotVSyncLoop
;WaitVSyncLoop:		in		al,dx
;					and		al,VSYNC_MASK
;					jz		WaitVSyncLoop
				ret

CopyClearBackBufferV8:	cld
						
						mov		bp,ds			; backup ds

						mov		dx,[ScreenBufferSeg]
						mov		fs,dx

						mov		dx,[BackBufferSeg]
						mov		ds,dx

						mov		ax,VIDEO_SEGMENT
						mov		es,ax

						xor		esi,esi
						mov		edi,esi

						mov		ecx,4001	; 8000 words
;						rep		movsd		; Move doubleword at address DS:(E)SI to address  ES:(E)DI

.loop1:						repe	cmpsd
							jcxz	.exit1
								mov		eax,[ds:si-4] ;-4
								mov		[fs:di-4],eax ;-4
								mov		[es:di-4],eax ;-4
;	inc	bx
							jmp		.loop1
.exit1:

						mov		ds,bp		; restore ds

SetScreen:	mov     ax, [BackBufferSeg]		; Fill the screen with character 222
			mov     es, ax
			xor     di, di
			mov     ax, 0x00de
;			mov		al, 222
			mov		ah, 13h
			push	ax
			push	ax
			pop		eax
			mov		ah, 45h
			mov     cx, 4000		; 8000 words - (1 char 1 attrib)
			rep		stosd
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
.testyl		cmp 	ax,99
			jne 	.testxr
				neg 	dx
.testxr		cmp 	bx,159
			jne 	.testyr
				neg 	bp
.testyr		mov 	[di],dx
			mov	[di+2],bp
 
			add		si,4
			add		di,4
			dec		cx
			jne		.loop
		ret

Plots:	lea 	si,[PointList]
 
.loop		push	cx
			mov		bp,[si]
			mov		bx,[si+2]
			mov		al,0ffh
			call 	Plot
			pop		cx
 
			add	si,4
			dec	cl
			jne	.loop
		ret
		
Plot1:		; colour
			and		al,[andcolourtab+bx]	; remove nibble that isnt needed from colour passed in

			add		bp,bp
			mov		bp,[vtable+bp]			; get v start pos

			add		bx,bx
			add		bp,[htable+bx]			; add h start pos
			mov		ah,[es:bp]				; get screen
			and		ah,[handtab+bx]			; remove nibble to be replaced
			or		al,ah
			mov		[es:bp],al
			ret

Plot2:		; colour
			and		al,[andcolourtab+bx]	; remove nibble that isnt needed from colour passed in
			mov		ah,[andcolourtab+1+bx]		; get nibble to be replaced

			add		bp,bp
			mov		bp,[vtable+bp]			; get v start pos

			add		bx,bx
			add		bp,[htable+bx]			; add h start pos

			and		ah,[es:bp]				; remove nibble to be replaced from screen
			or		al,ah
			mov		[es:bp],al
			ret

Plot:		; colour
			and		al,[andcolourtab+bx]	; remove nibble that isnt needed from colour passed in
			mov		ah,[andcolourtab+1+bx]		; get nibble to be replaced
			
			mov		bl,[htableb+bx]			; add h start pos
			add		bp,bp
			add		bx,[vtable+bp]			; get v start pos

			and		ah,[es:bx]				; remove nibble to be replaced from screen
			or		al,ah
			mov		[es:bx],al
			ret

section .data 
align 2

vtable	dw	00*SCR_WIDTH,01*SCR_WIDTH,02*SCR_WIDTH,03*SCR_WIDTH,04*SCR_WIDTH,05*SCR_WIDTH,06*SCR_WIDTH,07*SCR_WIDTH,08*SCR_WIDTH,09*SCR_WIDTH
		dw	10*SCR_WIDTH,11*SCR_WIDTH,12*SCR_WIDTH,13*SCR_WIDTH,14*SCR_WIDTH,15*SCR_WIDTH,16*SCR_WIDTH,17*SCR_WIDTH,18*SCR_WIDTH,19*SCR_WIDTH
		dw	20*SCR_WIDTH,21*SCR_WIDTH,22*SCR_WIDTH,23*SCR_WIDTH,24*SCR_WIDTH,25*SCR_WIDTH,26*SCR_WIDTH,27*SCR_WIDTH,28*SCR_WIDTH,29*SCR_WIDTH
		dw	30*SCR_WIDTH,31*SCR_WIDTH,32*SCR_WIDTH,33*SCR_WIDTH,34*SCR_WIDTH,35*SCR_WIDTH,36*SCR_WIDTH,37*SCR_WIDTH,38*SCR_WIDTH,39*SCR_WIDTH
		dw	40*SCR_WIDTH,41*SCR_WIDTH,42*SCR_WIDTH,43*SCR_WIDTH,44*SCR_WIDTH,45*SCR_WIDTH,46*SCR_WIDTH,47*SCR_WIDTH,48*SCR_WIDTH,49*SCR_WIDTH
		dw	50*SCR_WIDTH,51*SCR_WIDTH,52*SCR_WIDTH,53*SCR_WIDTH,54*SCR_WIDTH,55*SCR_WIDTH,56*SCR_WIDTH,57*SCR_WIDTH,58*SCR_WIDTH,59*SCR_WIDTH
		dw	60*SCR_WIDTH,61*SCR_WIDTH,62*SCR_WIDTH,63*SCR_WIDTH,64*SCR_WIDTH,65*SCR_WIDTH,66*SCR_WIDTH,67*SCR_WIDTH,68*SCR_WIDTH,69*SCR_WIDTH
		dw	70*SCR_WIDTH,71*SCR_WIDTH,72*SCR_WIDTH,73*SCR_WIDTH,74*SCR_WIDTH,75*SCR_WIDTH,76*SCR_WIDTH,77*SCR_WIDTH,78*SCR_WIDTH,79*SCR_WIDTH
		dw	80*SCR_WIDTH,81*SCR_WIDTH,82*SCR_WIDTH,83*SCR_WIDTH,84*SCR_WIDTH,85*SCR_WIDTH,86*SCR_WIDTH,87*SCR_WIDTH,88*SCR_WIDTH,89*SCR_WIDTH
		dw	90*SCR_WIDTH,91*SCR_WIDTH,92*SCR_WIDTH,93*SCR_WIDTH,94*SCR_WIDTH,95*SCR_WIDTH,96*SCR_WIDTH,97*SCR_WIDTH,98*SCR_WIDTH,99*SCR_WIDTH
		
htable	dw	(000*2)+1,(000*2)+1,(001*2)+1,(001*2)+1,(002*2)+1,(002*2)+1,(003*2)+1,(003*2)+1,(004*2)+1,(004*2)+1
		dw	(005*2)+1,(005*2)+1,(006*2)+1,(006*2)+1,(007*2)+1,(007*2)+1,(008*2)+1,(008*2)+1,(009*2)+1,(009*2)+1
		dw	(010*2)+1,(010*2)+1,(011*2)+1,(011*2)+1,(012*2)+1,(012*2)+1,(013*2)+1,(013*2)+1,(014*2)+1,(014*2)+1
		dw	(015*2)+1,(015*2)+1,(016*2)+1,(016*2)+1,(017*2)+1,(017*2)+1,(018*2)+1,(018*2)+1,(019*2)+1,(019*2)+1
		dw	(020*2)+1,(020*2)+1,(021*2)+1,(021*2)+1,(022*2)+1,(022*2)+1,(023*2)+1,(023*2)+1,(024*2)+1,(024*2)+1
		dw	(025*2)+1,(025*2)+1,(026*2)+1,(026*2)+1,(027*2)+1,(027*2)+1,(028*2)+1,(028*2)+1,(029*2)+1,(029*2)+1
		dw	(030*2)+1,(030*2)+1,(031*2)+1,(031*2)+1,(032*2)+1,(032*2)+1,(033*2)+1,(033*2)+1,(034*2)+1,(034*2)+1
		dw	(035*2)+1,(035*2)+1,(036*2)+1,(036*2)+1,(037*2)+1,(037*2)+1,(038*2)+1,(038*2)+1,(039*2)+1,(039*2)+1
		dw	(040*2)+1,(040*2)+1,(041*2)+1,(041*2)+1,(042*2)+1,(042*2)+1,(043*2)+1,(043*2)+1,(044*2)+1,(044*2)+1
		dw	(045*2)+1,(045*2)+1,(046*2)+1,(046*2)+1,(047*2)+1,(047*2)+1,(048*2)+1,(048*2)+1,(049*2)+1,(049*2)+1
		dw	(050*2)+1,(050*2)+1,(051*2)+1,(051*2)+1,(052*2)+1,(052*2)+1,(053*2)+1,(053*2)+1,(054*2)+1,(054*2)+1
		dw	(055*2)+1,(055*2)+1,(056*2)+1,(056*2)+1,(057*2)+1,(057*2)+1,(058*2)+1,(058*2)+1,(059*2)+1,(059*2)+1
		dw	(060*2)+1,(060*2)+1,(061*2)+1,(061*2)+1,(062*2)+1,(062*2)+1,(063*2)+1,(063*2)+1,(064*2)+1,(064*2)+1
		dw	(065*2)+1,(065*2)+1,(066*2)+1,(066*2)+1,(067*2)+1,(067*2)+1,(068*2)+1,(068*2)+1,(069*2)+1,(069*2)+1
		dw	(070*2)+1,(070*2)+1,(071*2)+1,(071*2)+1,(072*2)+1,(072*2)+1,(073*2)+1,(073*2)+1,(074*2)+1,(074*2)+1
		dw	(075*2)+1,(075*2)+1,(076*2)+1,(076*2)+1,(077*2)+1,(077*2)+1,(078*2)+1,(078*2)+1,(079*2)+1,(079*2)+1

htableb	db	(000*2)+1,(000*2)+1,(001*2)+1,(001*2)+1,(002*2)+1,(002*2)+1,(003*2)+1,(003*2)+1,(004*2)+1,(004*2)+1
		db	(005*2)+1,(005*2)+1,(006*2)+1,(006*2)+1,(007*2)+1,(007*2)+1,(008*2)+1,(008*2)+1,(009*2)+1,(009*2)+1
		db	(010*2)+1,(010*2)+1,(011*2)+1,(011*2)+1,(012*2)+1,(012*2)+1,(013*2)+1,(013*2)+1,(014*2)+1,(014*2)+1
		db	(015*2)+1,(015*2)+1,(016*2)+1,(016*2)+1,(017*2)+1,(017*2)+1,(018*2)+1,(018*2)+1,(019*2)+1,(019*2)+1
		db	(020*2)+1,(020*2)+1,(021*2)+1,(021*2)+1,(022*2)+1,(022*2)+1,(023*2)+1,(023*2)+1,(024*2)+1,(024*2)+1
		db	(025*2)+1,(025*2)+1,(026*2)+1,(026*2)+1,(027*2)+1,(027*2)+1,(028*2)+1,(028*2)+1,(029*2)+1,(029*2)+1
		db	(030*2)+1,(030*2)+1,(031*2)+1,(031*2)+1,(032*2)+1,(032*2)+1,(033*2)+1,(033*2)+1,(034*2)+1,(034*2)+1
		db	(035*2)+1,(035*2)+1,(036*2)+1,(036*2)+1,(037*2)+1,(037*2)+1,(038*2)+1,(038*2)+1,(039*2)+1,(039*2)+1
		db	(040*2)+1,(040*2)+1,(041*2)+1,(041*2)+1,(042*2)+1,(042*2)+1,(043*2)+1,(043*2)+1,(044*2)+1,(044*2)+1
		db	(045*2)+1,(045*2)+1,(046*2)+1,(046*2)+1,(047*2)+1,(047*2)+1,(048*2)+1,(048*2)+1,(049*2)+1,(049*2)+1
		db	(050*2)+1,(050*2)+1,(051*2)+1,(051*2)+1,(052*2)+1,(052*2)+1,(053*2)+1,(053*2)+1,(054*2)+1,(054*2)+1
		db	(055*2)+1,(055*2)+1,(056*2)+1,(056*2)+1,(057*2)+1,(057*2)+1,(058*2)+1,(058*2)+1,(059*2)+1,(059*2)+1
		db	(060*2)+1,(060*2)+1,(061*2)+1,(061*2)+1,(062*2)+1,(062*2)+1,(063*2)+1,(063*2)+1,(064*2)+1,(064*2)+1
		db	(065*2)+1,(065*2)+1,(066*2)+1,(066*2)+1,(067*2)+1,(067*2)+1,(068*2)+1,(068*2)+1,(069*2)+1,(069*2)+1
		db	(070*2)+1,(070*2)+1,(071*2)+1,(071*2)+1,(072*2)+1,(072*2)+1,(073*2)+1,(073*2)+1,(074*2)+1,(074*2)+1
		db	(075*2)+1,(075*2)+1,(076*2)+1,(076*2)+1,(077*2)+1,(077*2)+1,(078*2)+1,(078*2)+1,(079*2)+1,(079*2)+1

handtabb	times 160/2 db	00fh,0f0h

handtab	times 160/2 dw	00fh,0f0h
andcolourtab 	times ((160/2)+1) db	0f0h,00fh

testx	dw	50
testy	dw	50
colour	db	0ffh

align 2
PointList: dw 008, 13, 22, 04, 002, 016, 0, 0
DirList:	dw 1,-1,-1,1,-1,1 
 dw	1,1
 dw	-1,1
 dw	1,-1
 dw 	+1,-1,+1,-1,+1,-1,+1,-1,+1,-1,+1,-1

section .bss 	; put uninitialized data here

BackBufferSeg	resw	1	; pointer to the segment containing the back buffer to be drawn on
ScreenBufferSeg	resw	1	; pointer to the segment containing the screen buffer thats a copy of the current screen

Exit:		resb	1
switches	resb	1
active		resb	1
status		resb	1

EXE_end