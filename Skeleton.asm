;***********************************************************
;*
;* 	ECE 375 Lab 6 - Fall 2012
;*	Remote.asm
;*
;*	This is the remote for Lab 6 of ECE 375. The Remote sends 
;* 	one of 6 commands to a Robot: Move Forward, Move Backward, 
;*	Turn Right, Turn Left, Halt and Send Freeze. 
;*
;*
;***********************************************************
;*
;*	 Author: Devlin Junker
;*	   Date: November 11th 2012
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	cmdr = r17				; Command Register

.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

.equ	BotID =  0b00110011		; Unique BotID = $33 (MSB = 0) 

; Use these commands between the remote and TekBot
; MSB = 1 thus:
; commands are shifted right by one and ORed with 0b10000000 = $80
.equ	MovFwd =  ($80|1<<(EngDirR-1)|1<<(EngDirL-1))	;0b10110000 Move Forwards Command
.equ	MovBck =  ($80|$00)								;0b10000000 Move Backwards Command
.equ	TurnR =   ($80|1<<(EngDirL-1))					;0b10100000 Turn Right Command
.equ	TurnL =   ($80|1<<(EngDirR-1))					;0b10010000 Turn Left Command
.equ	Halt =    ($80|1<<(EngEnR-1)|1<<(EngEnL-1))		;0b11001000 Halt Command
.equ	Freez =   ($80|$F8)								;0b11111000 Freeze Command
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt


.org	$0046					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:
		;Stack Pointer 
		ldi mpr, HIGH(RAMEND)
		out SPH, mpr
		ldi mpr, LOW(RAMEND)
		out SPL, mpr

		; Initialize Port B for output
		ldi mpr, $FF
		out DDRB, mpr		; Set Port B as Output
		ldi mpr, $00
		out PORTB, mpr		; Default Output set 0

	; Initialize Port D for input
		ldi mpr, $00
		out DDRD, mpr		; Set Port D as Input
		ldi mpr, $F3
		out PORTD, mpr		; Set Input to Hi-Z
		out PIND, mpr
	
		;USART1
		;Enable transmitter
		ldi mpr, (1<<TXEN1)
		sts UCSR1B, mpr

		;Set frame format: 8data, 2 stop bit
		ldi mpr, (1<<USBS1)|(3<<UCSZ10)
		sts UCSR1C,mpr

		;Set baudrate at 2400bps
		ldi mpr, $01
		sts UBRR1H, mpr
		ldi mpr, $A0
		sts UBRR1L, mpr

;***********************************************************
;*	Functions and Subroutines
;***********************************************************


;***********************************************************
;*	Stored Program Data
;***********************************************************



;***********************************************************
;*	Additional Program Includes
;***********************************************************
