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
.equ	BotID =  0b00110011		; Unique BotID = $33 (MSB = 0) 

; Use these commands between the server and player (OR WITH 4 BIT BOT ID TO INCLUDE ID)
; MSB = 1 thus:
.equ	New		= 0b10000000				;0b1000XXXX New Game Command X's will have ID
.equ	Join 	= 0b10010000				;0b1001XXXX Join Game Command
.equ	Start 	= 0b10100000				;0b1010XXXX Start Game Command 
.equ	Winner 	= 0b11110000				;0b1111XXXX Winner Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	Ask 	= 0b10110000				;0b1011XXXX Ask for Scored Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
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
