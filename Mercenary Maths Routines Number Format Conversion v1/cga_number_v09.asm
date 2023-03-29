; n cga_number_v08
	
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
KEY_7 	EQU	0837h
KEY_8 	EQU	0938h
KEY_ESC	EQU 27

CPU 8086
bits 16
org 100h

section .text

start:
	call		BuildScreenTable

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
	
MainLoop:
		call	WaitVSync ; Wait for vertical sync so the new start address has a chance to take effect.
		call	CopyClearBackBuffer ; copy back to front
	
		mov		bx,[BackBufferSeg]
		mov		es,bx

		mov		bh,[XPos+1]
		mov		ax,[XPos+2]
		mov		bl,1
		call	Hex24

		mov		al,[XInc]
		mov		bl,2
		call	Hex8

		mov		dl,[XPos+1]
		mov		ax,[XPos+2]
		call	BIT24TOFP
		mov		bl,3
		mov		[XFP],ax
		call	Hex16

		mov		ax,[XFP]
		call	FPTO16BIT
		mov		bl,4
		call	Hex16

		mov		ax,[XFP]
		call	FPTO8BIT
		mov		bl,6
		call	Hex8

		mov		bh,[ZPos+1]
		mov		ax,[ZPos+2]
		mov		bl,7
		call	Hex24

		mov		al,[ZInc]
		mov		bl,8
		call	Hex8

		mov		dl,[ZPos+1]
		mov		ax,[ZPos+2]
		call	BIT24TOFP
		mov		[ZFP],ax
		mov		bl,9
		call	Hex16

		mov		ax,[ZFP]
		call	FPTO16BIT
		mov		bl,10
		call	Hex16

		mov		ax,[ZFP]
		call	FPTO8BIT
		mov		bl,11
		call	Hex8

		mov		dx,[ZFP]
		mov		bx,[XFP]		
		call	FP_ADD
		mov		ax,dx
		mov		bl,13
		call	Hex16

		mov		bx,[ZFP]
		mov		dx,[XFP]		
		call	FP_MUL
		mov		[tst],ax
		mov		bl,14
		call	Hex16

		mov		bx,[tst]
		mov		dx,[XFP]		
		call	FP_DIV
		mov		bl,15
		call	Hex16

		mov		al,[Test1]
		mov		bl,23
		call	Hex8
		mov		al,[Test2]
		mov		bl,24
		call	Hex8
		mov		al,[Test3]
		mov		bl,25
		call	Hex8
		mov		al,[Test4]
		mov		bl,26
		call	Hex8
		mov		al,[Test5]
		mov		bl,27
		call	Hex8
		mov		al,[Test6]
		mov		bl,28
		call	Hex8
		mov		al,[Test7]
		mov		bl,29
		call	Hex8
		mov		al,[Test8]
		mov		bl,30
		call	Hex8
		mov		al,[Test9]
		mov		bl,31
		call	Hex8

	mov		ah,[XInc]
	mov 	al,[XPos+3]
	sub 	al,ah
	mov 	[XPos+3],al
	mov 	al,[XPos+2]
	sbb 	al,0
	mov 	[XPos+2],al
	mov 	al,[XPos+1]
	sbb 	al,0
	mov 	[XPos+1],al

	mov		ah,[ZInc]
	mov 	al,[ZPos+3]
	add 	al,ah
	mov 	[ZPos+3],al
	mov 	al,[ZPos+2]
	adc 	al,0
	mov 	[ZPos+2],al
	mov 	al,[ZPos+1]
	adc 	al,0
	mov 	[ZPos+1],al

		call	GetKey
	
	mov		al,[Exit]
	test	al,al
	jz		MainLoop

Done: ; Finished, clear key, reset screen mode and exit.
	mov	ax,3		; reset to text mode
	int		10h
	mov	ah,4ch	; exit to DOS
	int		21h

GetKey:
	mov		ah,11h
	int		16h         	; has a key been pressed
	jz		.key_none ; no
	mov		ah,10h      ; yes 
	int		16h         	; get it in AX (al= ascii, ah=scan code)
	cmp		al,KEY_ESC
	je		.key_esc
	cmp 	ax,KEY_1
	je 		.key_1
	cmp 	ax,KEY_2
	je 		.key_2
	cmp 	ax,KEY_3
	je 		.key_3
	cmp 	ax,KEY_4
	je 		.key_4
	cmp 	ax,KEY_5
	je 		.key_5
	cmp 	ax,KEY_6
	je 		.key_6
	cmp 	ax,KEY_7
	je 		.key_7
	cmp 	ax,KEY_8
	je 		.key_8
.key_none:
	ret
.key_esc:
	mov		al,1
	mov		[Exit],al
	ret
.key_1:
	mov		ah,[XInc]
	mov 	al,[XPos+3]
	sub 	al,ah
	mov 	[XPos+3],al
	mov 	al,[XPos+2]
	sbb 	al,0
	mov 	[XPos+2],al
	mov 	al,[XPos+1]
	sbb 	al,0
	mov 	[XPos+1],al
	ret
.key_2:
	mov		ah,[XInc]
	mov 	al,[XPos+3]
	add 	al,ah
	mov 	[XPos+3],al
	mov 	al,[XPos+2]
	adc 	al,0
	mov 	[XPos+2],al
	mov 	al,[XPos+1]
	adc 	al,0
	mov 	[XPos+1],al
	ret
.key_3:
	mov		ah,[ZInc]
	mov 	al,[ZPos+3]
	sub 	al,ah
	mov 	[ZPos+3],al
	mov 	al,[ZPos+2]
	sbb 	al,0
	mov 	[ZPos+2],al
	mov 	al,[ZPos+1]
	sbb 	al,0
	mov 	[ZPos+1],al
	ret
.key_4:
	mov		ah,[ZInc]
	mov 	al,[ZPos+3]
	add 	al,ah
	mov 	[ZPos+3],al
	mov 	al,[ZPos+2]
	adc 	al,0
	mov 	[ZPos+2],al
	mov 	al,[ZPos+1]
	adc 	al,0
	mov 	[ZPos+1],al
	ret
.key_5:
	mov		al,[XInc]
	dec		al
	mov 	[XInc],al
	ret
.key_6:
	mov 	al,[XInc]
	inc		al
	mov 	[XInc],al
	ret
.key_7:
	mov 	al,[ZInc]
	dec		al
	mov 	[ZInc],al
	ret
.key_8:
	mov 	al,[ZInc]
	inc		al
	mov 	[ZInc],al
	ret

BuildScreenTable:
	xor		si,si	; even lines
	mov		bp,8192 ; odd lines
	lea		di,[BackBufferTable]
	mov		cl,100
.loop:
		mov		[di],si
		mov		[di+2],bp
		add		si,80
		add		bp,80
		add		di,4
		dec		cl
		jnz		.loop
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

	mov		ax,0 ; 00101101001011010b ;
	
	xor		di,di		; DS:SI points to even back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		di,8192		; DS:SI points to odd back buffer	
	mov		cx,4000
	rep	stosw			; clear odd back buffer

	mov		ds,bp			; restore ds

	ret
	
WaitVSync:			mov		dx,INPUT_STATUS_1	; Wait for the leading edge of vertical sync pulse.
.WaitNotVSyncLoop:		in		al,dx
						and		al,VSYNC_MASK
						jnz		.WaitNotVSyncLoop
.WaitVSyncLoop:			in		al,dx
						and		al,VSYNC_MASK
						jz		.WaitVSyncLoop
					ret

;The 8086 has 14 16 bits registers.
;AX, BX, CX, DX, SI, DI, BP, SP, CS, DS, SS, ES, IP and the flags register.
;The last two are only accessed indirectly.

; A = exponent = value of (first bit position set *4?) with sign in bit 0
; Y = mantissa = value of next 8 bits of the rest of the 8/16/24 bit number

; al = lsb
; ah = psb
; dl = msb

BIT24TOFP:	xchg	ah,al
			mov		dh,dl			; store msb
			test	dl,dl			; test msb
			jns		.BIT24FP_POS
				xor		dl,dl
				neg		ax
				sbb		dl,dh
				
.BIT24FP_POS:	jnz		.BIT24
				test	ah,ah			; test psb
				jnz		.BIT16

				test	al,al			; test lsb
				jz		.BIT00

						mov		ah,al
						xor		al,al
						mov		dl,8
						jmp		.EXP_LOOP

.BIT24:		mov		al,ah
			mov		ah,dl
			mov		dl,24

.EXP_LOOP:		dec		dl			; find exponent - start from whatever x is passed in and reduce until x a bit set found 
				shl		ax,1		; shift lsb up into carry
				jnc		.EXP_LOOP	; as soon as first bit is set then exit

			shl		dl,1			;	ASL 			; exponent *2
			shl		dh,1			;	ASL 	ab06	; shift msb putting sign into carry ; carry isnt being set?
			rcl		dl,1			;	ROL 			; rotate carry into first bit of exponent - bit 0 is sign
			mov		al,ah
			mov		ah,dl
.BIT00:		ret					; overflow

.BIT16:		mov		dl,16
			jmp		.EXP_LOOP


FPTO16BIT:	test	ah,ah
			js		.negative	; if negative
			
			mov		dh,ah		; store in temp var for sign

			shr		ah,1
			shr		ah,1		; /4
			
			cmp		ah,15		; more than 16 bits
			jg		FP_error		; yes so error so exit with carry set

			add		ah,ah
			mov		[.sm_jmp+1],ah

			mov		ah,al		; unsigned long rc = ( ( (number.value >>1) +128) <<8) + ((number.value &1) <<7);
			xor		al,al
			stc
			rcr		ax,1
			
.sm_jmp		jnc		.jmp
			shr		ax,1 ;1
.jmp		shr		ax,1 ;2
			shr		ax,1 ;3
			shr		ax,1 ;4
			shr		ax,1 ;5
			shr		ax,1 ;6
			shr		ax,1 ;7
			shr		ax,1 ;8
			shr		ax,1 ;9
			shr		ax,1 ;10
			shr		ax,1 ;11
			shr		ax,1 ;12
			shr		ax,1 ;13
			shr		ax,1 ;14
			shr		ax,1 ;15

			shr		dh,1		; move input sign into carry
			jnc		.positive
				xor		al,255
				xor		ah,255
;				neg		ax
.positive   ret

.negative 	shr		ah,1		; 1st bit to carry
			jnc		.pos_sign
				mov		ax,0ffffh
				ret

.pos_sign:
FP_error:
			xor		ax,ax
			ret

FPTO8BIT:	test	ah,ah		;	TYA 				; input X/Y (mantisa/exponent) -  output X=HI/Y=LO ; transfer exponent? to a
			js		.negative	; if negative			;			BMI		b849F		; if negative

			mov		dl,ah		; store in temp var for sign

			shr		ah,1
			shr		ah,1		; /4

			cmp		ah,7				;			CMP 	#$07		; is it greater than 7
			jg		FP_error	;			BCS 	FP8BIT_ERR		; yes so error so exit with carry set

			add		ah,ah
			mov		[.sm_jmp+1],ah

			stc
			rcr		al,1		;		rc = (number.value >>1) +128;

.sm_jmp		jnc		.jmp
			shr		al,1
.jmp		shr		al,1
			shr		al,1
			shr		al,1
			shr		al,1
			shr		al,1
			shr		al,1

.cont:		adc		ah,0		;  	ADC 	#0			; add carry?
			js		FP_error	;			BMI 	b84A5		; if minus then error?

			shr		dl,1		; move input sign into carry
			jnc		.positive
				neg		al
.positive	ret

.negative:	cmp		ah,0fch		;	b849F   	CMP 	#$FC		; sets the carry flag if greater than (11111100 binary)
			xor		ah,ah		;			LDA 	#$00		; resets the output x value? 
			jmp		.cont		;			BEQ 	b848F		; jump back into code


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
			call	HexChar
			ret

MATH_LIMIT_EXIT:	xor		dl,dl
					mov		dh,al
					ret

FPADD_CONT:		mov		al,cl		;	LDA		ab18	; ($838B entry point)  
				shr		al,1		;			LSR 
				jc		FPADD_CONT2	;			BCS 	FPADD_CONT2

				mov		al,dl		;        TXA 
				adc		al,bl		;        ADC 	ab08
				rcr		al,1		;        ROR 
				add		dh,4		;        INY 
									;        INY 
									;        INY 
									;        INY 
				mov		dl,al		;        TAX 
									;        STA 	ab08
									;        STY 	ab09

	mov		[Test9], byte 0f8h
				ret					;        RTS 

FP_ADD_ERR		mov		al,dh			;		TYA 
				shl		al,1			;			ASL 
				jle		FP_ADD_ERR_1ST	;			BCC 	FP_ADD_ERR_1ST
				jmp		FP_ADD_ERR_2ND	;			JMP 	FP_ADD_ERR_2ND

FPADD_CONT2:	mov		al,dl			;		TXA 
				cmc
				sbb		al,bl			;      	SBC 	ab08

				je		FPADD_CONT3		;    	BEQ 	FPADD_CONT3
	
				jg		FPADD_CONT6		;		BCS 	FPADD_CONT6

				mov		dh,bh			;			LDY 	ab09
				neg		al				;			EOR 	#$FF
										;			ADC 	#$01
				jmp		FPADD_CONT6		;			JMP 	FPADD_CONT6

FPADD_CONT3		mov		al,dh			;		TYA 
										;				SEC 
				sub		al,024h			;				SBC 	#$24
; BVS 	_MATH_LIMIT_POS

	mov		[Test9], byte 0f7h

				jmp		MATH_LIMIT_EXIT	; jmp		MATH_LIMIT_EXIT	;					BVC 	MATH_LIMIT_EXIT
	
; A = AL
; X = DL
; Y = DH
; 08 = BL
; 09 = BH
; ab18 = CL
; ab06 = CH
	
FP_ADD:		mov		al,dh			;  	TYA			; XY + 89	; transfer num1_hi to a

	mov		[Test9], byte 000h
	
			sub		al,bh			;        SBC 	ab09			; sub num2_hi from a
; BVS 	FP_ADD_ERR		; overflow so exit
			cmc
			mov		cl,al			;        STA 	ab18			; ab18 = num1_hi - num2_hi
			adc		al,1			;        ADC 	#$01			; add 1 + carry?
; BVS 	FP_ADD_ERR		; overflow so exit
			js		FPADD_CONT8			;        BMI 	FPADD_CONT8			

			shr		al,1			;        LSR 					; divide by 2
			shr		al,1			;        LSR 					; divide by 2
			je		FPADD_CONT		;        BEQ 	FPADD_CONT		; if 0
			
			
			cmp		al,09h			;        CMP 	#$09			; gt 9
			jg		FP_ADD_ERR_1ST	;        BCS 	FP_ADD_ERR_1ST	; yes

			xor		al,15			;        EOR 	#$0F			; reverse 
			add		al,al			; *2
			mov		[FP_ADD_JMP1+1],al	;        STA 	FP_ADD_JMP1 +1		; jump forward that amount

			mov		al,bl			;        LDA		ab08			; get num_hi
			stc						;        SEC ;        SEC						; set carry 
			rcr		al,1			;		rc = (number.value >>1) +128;	;        ROR;        ROR 					; /2 and move carry into first bit

FP_ADD_JMP1: 	
			jnc		b83B3 	; self modifiction code shift
			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

			nop					;        NOP 					; NA
			nop					;        NOP 					; NA

b83B3   	shr		al,1		;		LSR 					; divide by 2
			shr		al,1		;        LSR  					; divide by 2
			shr		al,1		;        LSR  					; divide by 2
			shr		al,1		;        LSR  					; divide by 2
			shr		al,1		;        LSR  					; divide by 2
			shr		al,1		;        LSR  					; divide by 2
			shr		al,1		;        LSR  					; divide by 2

			mov		ch,al		;        STA 	ab06			; 
			mov		al,cl		;        LDA 	ab18

			shr		al,1		;        LSR 					; divide by 2 - setting carry?
			mov		al,dl		;        TXA 					; transfer num1_lo to a

			jc		FPADD_CONT5	;        BCS 	FPADD_CONT5			; carry set
	
FPADD_CONT4	adc		al,ch		;		ADC 	ab06
			jnc		FPADD_CONT7	;			BCC 	FPADD_CONT7
			
			shr		al,1		;				LSR 				; divide by 2
			add		dh,4		;				INY 				; y++
								;				INY 				; y++
								;				INY 				; y++
								;				INY 				; y++
FPADD_CONT7	mov		dl,al		;   	TAX
FP_ADD_ERR_1ST: 
								;			STX 	ab08
								;			STY 	ab09
	mov		[Test9], byte 0f5h
	mov		ax,dx				; working
			ret					;			RTS 

FPADD_CONT5	sub		al,	ch	; sbb		al,	ch			;			SBC ab06
			cmc
			jc		FPADD_CONT7		;			BCS FPADD_CONT7

FPADD_CONT6	mov		ch,dh ; al			;			STY ab06
			xor		dh,dh			;        LDY #$00

FPADD_LOOP1		dec		dh			;				DEY 
				shl		al,1		;				ASL
				jnc		FPADD_LOOP1	;				BCC		FPADD_LOOP1

				mov		dl,al			;        TAX 
				mov		al,dh			;        TYA 
				shl		al,1		;        ASL 
				shl		al,1		;        ASL 
									;        CLC 				
				add		al,ch		;        ADC 	ab06
; BVS 	FPADD_ERR3
				mov		dh,al		;			TAY 
									;			STX 	ab08
									;			STY 	ab09
	mov		[Test9], byte 0f3h
				ret					;			RTS 	; working

FPADD_ERR3:							;	LDX 	#<8400
									;			LDY 	#>8400
									;			STX 	ab08
									;			STY		ab09
	mov		[Test9], byte 002h
				ret					;			RTS 

FPADD_CONT8:	cmp		al,0e0h				;		CMP 	#$E0
				jle		FP_ADD_ERR_2ND		;			BCC 	FP_ADD_ERR_2ND

				shr		al,1				;        LSR 
				shr		al,1				;        LSR 
				and		al,07				;        AND #$07
		
				add		al,al				; *2
				mov		[FP_ADD_JMP2 +1],al	;        STA FP_ADD_JMP2 +1
				mov		al,dl				;		TXA 

				stc						;        SEC 
				rcr		al,1			;		rc = (number.value >>1) +128;	;        ROR
			
FP_ADD_JMP2		jnz		b8408			;	BNE b8408
				shr		al,1			;        LSR 
				shr		al,1			;        LSR 
				shr		al,1			;        LSR 
b8408			shr		al,1			;        LSR 
				shr		al,1			;        LSR 
				shr		al,1			;        LSR 
				shr		al,1			;        LSR 
	
				mov		ch,al			;        STA ab06
				mov		dh,bh			;        LDY ab09
				mov		al,cl			;        LDA ab18
				shr		al,1			;        LSR 
				mov		al,	bl			;        LDA ab08
				
				jnc		FPADD_CONT4		;        BCC FPADD_CONT4
				jmp		FPADD_CONT5		;        BCS FPADD_CONT5	; jmp
		
FP_ADD_ERR_2ND:					;				LDX		ab08
								;				LDY 	ab09
	mov		[Test9], byte 001h
				ret				;				RTS 

; bx fp1
; dx fp2
; ax result

FP_DIV:		mov		al,bl
			or		al,2
			mov		bl,bh
			xor		bh,bh					; STA		FP_MULTIPLY_VALUE 		; store first value
			mov		ah,[LOG_TAB+bx]			;			LDA 	LOG_TABLE,X				; get second value - lookup bh
			mov		bl,dh					;			LDX 	FP_MULTIPLY_POWER_SIGN	; get first power sign
			sub		ah,[LOG_TAB+bx]			;			ADC 	LOG_TABLE,X				; second value + first power sign ? - subtract dh
			mov		bl,ah					;			TAX 							; store in x
			mov		ah,[EXP_TAB+bx]			;			LDY 	EXP_TABLE,X				; get ouput power sign
			jnc		.cont					;			BCC 	_FP_MUL_CNT
					sub		al,4			;					ADC 	#$03			; add 4 (00000011)
.cont		sub		al,dl					;_FP_MUL_CNT	ADC 	FP_MULTIPLY_VALUE		; add first value
			and		al,0fdh					;			AND 	#$FD					; get output value
			ret								;			RTS 

; bx fp1
; dx fp2
; ax result

FP_MUL:		mov		al,bh
			xor		bh,bh					; STA		FP_MULTIPLY_VALUE 		; store first value
			mov		ah,[LOG_TAB+bx]			;			LDA 	LOG_TABLE,X				; get second value
			mov		bl,dh					;			LDX 	FP_MULTIPLY_POWER_SIGN	; get first power sign
			add		ah,[LOG_TAB+bx]			;			ADC 	LOG_TABLE,X				; second value + first power sign ?
			mov		bl,ah					;			TAX 							; store in x
			mov		ah,[EXP_TAB+bx]			;			LDY 	EXP_TABLE,X				; get ouput power sign
			jnc		.cont					;			BCC 	_FP_MUL_CNT
					add		dl,4			;					ADC 	#$03			; add 4 (00000011)
.cont		add		al,dl					;_FP_MUL_CNT	ADC 	FP_MULTIPLY_VALUE		; add first value
			and		al,0fdh					;			AND 	#$FD					; get output value
			ret								;			RTS 
	
section .data align=2 ; 16 ; 8 ; 16

LOG_TAB db 000h,001h,003h,004h,006h,007h,009h,00Ah	; x = int(0.5 +log2(1 + x/256) * 256)
		db 00Bh,00Dh,00Eh,010h,011h,012h,014h,015h
        db 016h,018h,019h,01Ah,01Ch,01Dh,01Eh,020h
		db 021h,022h,024h,025h,026h,028h,029h,02Ah
		db 02Ch,02Dh,02Eh,02Fh,031h,032h,033h,034h
        db 036h,037h,038h,039h,03Bh,03Ch,03Dh,03Eh
		db 03Fh,041h,042h,043h,044h,045h,047h,048h
		db 049h,04Ah,04Bh,04Dh,04Eh,04Fh,050h,051h
		db 052h,054h,055h,056h,057h,058h,059h,05Ah
		db 05Ch,05Dh,05Eh,05Fh,060h,061h,062h,063h
		db 064h,066h,067h,068h,069h,06Ah,06Bh,06Ch
		db 06Dh,06Eh,06Fh,070h,071h,072h,074h,075h
		db 076h,077h,078h,079h,07Ah,07Bh,07Ch,07Dh
		db 07Eh,07Fh,080h,081h,082h,083h,084h,085h
		db 086h,087h,088h,089h,08Ah,08Bh,08Ch,08Dh
		db 08Eh,08Fh,090h,091h,092h,093h,094h,095h
		db 096h,097h,098h,099h,09Ah,09Bh,09Bh,09Ch
		db 09Dh,09Eh,09Fh,0A0h,0A1h,0A2h,0A3h,0A4h
		db 0A5h,0A6h,0A7h,0A8h,0A9h,0A9h,0AAh,0ABh
		db 0ACh,0ADh,0AEh,0AFh,0B0h,0B1h,0B2h,0B2h
		db 0B3h,0B4h,0B5h,0B6h,0B7h,0B8h,0B9h,0B9h
		db 0BAh,0BBh,0BCh,0BDh,0BEh,0BFh,0C0h,0C0h	
		db 0C1h,0C2h,0C3h,0C4h,0C5h,0C6h,0C6h,0C7h
		db 0C8h,0C9h,0CAh,0CBh,0CBh,0CCh,0CDh,0CEh
		db 0CFh,0D0h,0D0h,0D1h,0D2h,0D3h,0D4h,0D4h
		db 0D5h,0D6h,0D7h,0D8h,0D8h,0D9h,0DAh,0DBh
		db 0DCh,0DCh,0DDh,0DEh,0DFh,0E0h,0E0h,0E1h
		db 0E2h,0E3h,0E4h,0E4h,0E5h,0E6h,0E7h,0E7h
		db 0E8h,0E9h,0EAh,0EAh,0EBh,0ECh,0EDh,0EEh
		db 0EEh,0EFh,0F0h,0F1h,0F1h,0F2h,0F3h,0F4h
		db 0F4h,0F5h,0F6h,0F7h,0F7h,0F8h,0F9h,0F9h
		db 0FAh,0FBh,0FCh,0FCh,0FDh,0FEh,0FFh,0FFh

EXP_TAB db 000h,001h,001h,002h,003h,003h,004h,005h	; x = int(0.5 + (2^(x/256) -1) *256)
		db 006h,006h,007h,008h,008h,009h,00Ah,00Bh
		db 00Bh,00Ch,00Dh,00Eh,00Eh,00Fh,010h,010h
		db 011h,012h,013h,013h,014h,015h,016h,016h
		db 017h,018h,019h,019h,01Ah,01Bh,01Ch,01Dh
		db 01Dh,01Eh,01Fh,020h,020h,021h,022h,023h
		db 024h,024h,025h,026h,027h,028h,028h,029h
		db 02Ah,02Bh,02Ch,02Ch,02Dh,02Eh,02Fh,030h
		db 030h,031h,032h,033h,034h,035h,035h,036h
		db 037h,038h,039h,03Ah,03Ah,03Bh,03Ch,03Dh
		db 03Eh,03Fh,040h,041h,041h,042h,043h,044h
		db 045h,046h,047h,048h,048h,049h,04Ah,04Bh
		db 04Ch,04Dh,04Eh,04Fh,050h,051h,051h,052h
		db 053h,054h,055h,056h,057h,058h,059h,05Ah
		db 05Bh,05Ch,05Dh,05Eh,05Eh,05Fh,060h,061h
		db 062h,063h,064h,065h,066h,067h,068h,069h
		db 06Ah,06Bh,06Ch,06Dh,06Eh,06Fh,070h,071h
		db 072h,073h,074h,075h,076h,077h,078h,079h
		db 07Ah,07Bh,07Ch,07Dh,07Eh,07Fh,080h,081h
		db 082h,083h,084h,085h,087h,088h,089h,08Ah
		db 08Bh,08Ch,08Dh,08Eh,08Fh,090h,091h,092h
		db 093h,095h,096h,097h,098h,099h,09Ah,09Bh
		db 09Ch,09Dh,09Fh,0A0h,0A1h,0A2h,0A3h,0A4h
		db 0A5h,0A6h,0A8h,0A9h,0AAh,0ABh,0ACh,0ADh
		db 0AFh,0B0h,0B1h,0B2h,0B3h,0B4h,0B6h,0B7h
		db 0B8h,0B9h,0BAh,0BCh,0BDh,0BEh,0BFh,0C0h
		db 0C2h,0C3h,0C4h,0C5h,0C6h,0C8h,0C9h,0CAh
		db 0CBh,0CDh,0CEh,0CFh,0D0h,0D2h,0D3h,0D4h
		db 0D6h,0D7h,0D8h,0D9h,0DBh,0DCh,0DDh,0DEh
		db 0E0h,0E1h,0E2h,0E4h,0E5h,0E6h,0E8h,0E9h
		db 0EAh,0ECh,0EDh,0EEh,0F0h,0F1h,0F2h,0F4h	
		db 0F5h,0F6h,0F8h,0F9h,0FAh,0FCh,0FDh,0FFh

Hex0	db 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1	db 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2	db 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3	db 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4	db 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b

XPos	db	0h,0ffh,0ddh,01ch ; 0feh,082h,078h ; 031h,0c8h ; 0f2h,07h ; 0f8h,0b6h ; 0f2h,07h; 08eh,06bh ; 0ffh,0eeh,0ddh,0cch
ZPos	db	0h,0ffh,0d6h,0ech ;01h,07dh,044h ; 0eh,0b4h ; 0ffh,0f2h,096h ; 00h,0eh,0b4h
XInc	db	0
ZInc	db	0
Test1	db	11h
Test2	db	22h
Test3	db	33h
Test4	db	44h
Test5	db	55h
Test6	db	66h
Test7	db	77h
Test8	db	88h
Test9	db	0
Exit	db	0

align 2

BackBufferTable		dw	SE+(SW*000),SO+(SW*001),SE+(SW*002),SO+(SW*003),SE+(SW*004),SO+(SW*005),SE+(SW*006),SO+(SW*007),SE+(SW*008),SO+(SW*009)
					dw	SE+(SW*010),SO+(SW*011),SE+(SW*012),SO+(SW*013),SE+(SW*014),SO+(SW*015),SE+(SW*016),SO+(SW*017),SE+(SW*018),SO+(SW*019)
					dw	SE+(SW*020),SO+(SW*021),SE+(SW*022),SO+(SW*023),SE+(SW*024),SO+(SW*025),SE+(SW*026),SO+(SW*027),SE+(SW*028),SO+(SW*029)
					dw	SE+(SW*030),SO+(SW*031),SE+(SW*032),SO+(SW*033),SE+(SW*034),SO+(SW*035),SE+(SW*036),SO+(SW*037),SE+(SW*038),SO+(SW*039)
					dw	SE+(SW*040),SO+(SW*041),SE+(SW*042),SO+(SW*043),SE+(SW*044),SO+(SW*045),SE+(SW*046),SO+(SW*047),SE+(SW*048),SO+(SW*049)
					dw	SE+(SW*050),SO+(SW*051),SE+(SW*052),SO+(SW*053),SE+(SW*054),SO+(SW*055),SE+(SW*056),SO+(SW*057),SE+(SW*058),SO+(SW*059)
					dw	SE+(SW*060),SO+(SW*061),SE+(SW*062),SO+(SW*063),SE+(SW*064),SO+(SW*065),SE+(SW*066),SO+(SW*067),SE+(SW*068),SO+(SW*069)
					dw	SE+(SW*070),SO+(SW*071),SE+(SW*072),SO+(SW*073),SE+(SW*074),SO+(SW*075),SE+(SW*076),SO+(SW*077),SE+(SW*078),SO+(SW*079)
					dw	SE+(SW*080),SO+(SW*081),SE+(SW*082),SO+(SW*083),SE+(SW*084),SO+(SW*085),SE+(SW*086),SO+(SW*087),SE+(SW*088),SO+(SW*089)
					dw	SE+(SW*090),SO+(SW*091),SE+(SW*092),SO+(SW*093),SE+(SW*094),SO+(SW*095),SE+(SW*096),SO+(SW*097),SE+(SW*098),SO+(SW*099)
               		dw	SE+(SW*100),SO+(SW*101),SE+(SW*102),SO+(SW*103),SE+(SW*104),SO+(SW*105),SE+(SW*106),SO+(SW*107),SE+(SW*108),SO+(SW*109)
					dw	SE+(SW*110),SO+(SW*111),SE+(SW*112),SO+(SW*113),SE+(SW*114),SO+(SW*115),SE+(SW*116),SO+(SW*117),SE+(SW*118),SO+(SW*119)
					dw	SE+(SW*120),SO+(SW*121),SE+(SW*122),SO+(SW*123),SE+(SW*124),SO+(SW*125),SE+(SW*126),SO+(SW*127),SE+(SW*128),SO+(SW*129)
					dw	SE+(SW*130),SO+(SW*131),SE+(SW*132),SO+(SW*133),SE+(SW*134),SO+(SW*135),SE+(SW*136),SO+(SW*137),SE+(SW*138),SO+(SW*139)
					dw	SE+(SW*140),SO+(SW*141),SE+(SW*142),SO+(SW*143),SE+(SW*144),SO+(SW*145),SE+(SW*146),SO+(SW*147),SE+(SW*148),SO+(SW*149)
					dw	SE+(SW*150),SO+(SW*151),SE+(SW*152),SO+(SW*153),SE+(SW*154),SO+(SW*155),SE+(SW*156),SO+(SW*157),SE+(SW*158),SO+(SW*159)
					dw	SE+(SW*160),SO+(SW*161),SE+(SW*162),SO+(SW*163),SE+(SW*164),SO+(SW*165),SE+(SW*166),SO+(SW*167),SE+(SW*168),SO+(SW*169)
					dw	SE+(SW*170),SO+(SW*171),SE+(SW*172),SO+(SW*173),SE+(SW*174),SO+(SW*175),SE+(SW*176),SO+(SW*177),SE+(SW*178),SO+(SW*179)
					dw	SE+(SW*180),SO+(SW*181),SE+(SW*182),SO+(SW*183),SE+(SW*184),SO+(SW*185),SE+(SW*186),SO+(SW*187),SE+(SW*188),SO+(SW*189)
					dw	SE+(SW*190),SO+(SW*191),SE+(SW*192),SO+(SW*193),SE+(SW*194),SO+(SW*195),SE+(SW*196),SO+(SW*197),SE+(SW*198),SO+(SW*199)

section .bss 	; put uninitialized data here

BackBuffer 	resw 16384	; 2 screen buffers
BackBufferSeg	resw	1	; pointer to the segment containing the back buffer

tst		resw	1
XFP		resw	1
ZFP		resw	1

