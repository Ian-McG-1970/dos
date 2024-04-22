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
 

KEY_1	EQU	0231h
KEY_2	EQU	0332h
KEY_3	EQU	0433h
KEY_4 	EQU	0534h
KEY_5 	EQU	0635h
KEY_6 	EQU	0736h
KEY_ESC	EQU 27

CPU 486
bits 16
org 100h

CRTCa EQU 03d8h	; 6845 mode control register 0x3d8
CRTCb EQU 03d4h ; 6845 index register
SR	  EQU 0dah	; Input Status Register
TEXT  EQU 3   ; DOS video mode for 80x25 text

;%macro fill_cont	0
;	pop		di
;	jnz		Fill.plotloop
;%endmacro

section .text

start:	call	Set160x100
		call	SetScreen

		xor al,al
		mov	[Exit],al
	
MainLoop:	call	WaitVSync 				; Wait for vertical sync so the new start address has a chance to take effect.
			call	GetKey
		
			mov		al,[Exit]
			or		al,al
			je		MainLoop

Done:	call	Mode3
		mov		ah,4ch	; exit to DOS
		int		21h

Mode3:	mov		ax,TEXT	; reset to text mode
		int		10h
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

			mov     ax, 0xb800		; Fill the screen with character 222
			mov     es, ax
			mov     ax, 0x00de
			mov     cx, 8000
			xor     di, di
			rep     stosw

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

			mov     ax, 0xb800		; Fill the screen with character 222
			mov     es, ax
			mov     ax, 0x00de
			mov     cx, 8000
			xor     di, di
			rep     stosw
	
			ret
	
write_cga_reg:	mov     dx, CRTCb	; Program the CRT controller
				xchg	al,ah
				out     dx, al
				inc     dx
				xchg	al,ah
				out     dx, al
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
			
;;; Dumps 8000 bytes of screen data to the low-res screen.
;;;
;;; IN: [DS:SI]=the start of the 8000 bytes
;;; OUT: None. Trashes CX, SI, DI.
SetScreen:	mov     si, screen_data
			xor     di, di
			mov     cx, 8000
.lp:    		inc     di
				movsb
				loop    .lp
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

WaitVSync: 		mov		dx,INPUT_STATUS_1	; Wait for the leading edge of vertical sync pulse.
WaitNotVSyncLoop:	in		al,dx
					and		al,VSYNC_MASK
					jnz		WaitNotVSyncLoop
;WaitVSyncLoop:		in		al,dx
;					and		al,VSYNC_MASK
;					jz		WaitVSyncLoop
				ret

section .data 
align 4

screen_data:
        incbin  "cgalores.dat"

section .bss 	; put uninitialized data here

;BackBufferSeg	resw	1	; pointer to the segment containing the back buffer to be drawn on
;ScreenBufferSeg	resw	1	; pointer to the segment containing the screen buffer thats a copy of the current screen

Exit:	resb	1
switches	resb	1
active		resb	1
status		resb	1
