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
.def 	mpr 	= r16			; Multi-Purpose Register
.def 	sigr	= r17			; Register to Hold Signal
.def 	idreg	= r18			; Register to Hold Board ID
.def 	playercnt = r19			; Register to Hold Number of Players

.equ 	NUM_PLAYERS = 2

.equ	BoardID	= 0b00000011	; Unique Board ID = $3 (MSB = 0) 

; Use these commands between the server and player (OR WITH 4 BIT BOT ID TO INCLUDE ID)
; MSB = 1 thus:
.equ	New		= 0b10000000	;0b1000XXXX New Game Command X's will have ID
.equ	Join 	= 0b10010000	;0b1001XXXX Join Game Command
.equ	Start 	= 0b10100000	;0b1010XXXX Start Game Command 
.equ	Winner 	= 0b11110000	;0b1111XXXX Winner Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	Ask 	= 0b10110000	;0b1011XXXX Ask for Scored Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)

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
		; Stack Pointer 
		ldi mpr, HIGH(RAMEND)
		out SPH, mpr
		ldi mpr, LOW(RAMEND)
		out SPL, mpr

		; Initialize Port D for input
		ldi mpr, $00
		out DDRD, mpr		; Set Port D as Input
		ldi mpr, $F3
		out PORTD, mpr		; Set Input to Hi-Z
		out PIND, mpr
	
		;USART1
		; Enable transmitter
		ldi mpr, (1<<RXEN1)
		sts UCSR1B, mpr

		; Set frame format: 8data, 2 stop bit
		ldi mpr, (1<<USBS1)|(3<<UCSZ10)
		sts UCSR1C,mpr

		; Set baudrate at 2400bps
		ldi mpr, $01
		sts UBRR1H, mpr
		ldi mpr, $A0
		sts UBRR1L, mpr

		; Set Player Count
		ldi playercnt, 0

		; Load initial addresses of arrays
		ldi XL, low(PLAYERS<<1)
		ldi XH, high(PLAYERS<<1)
		ldi YL, low(SCORES<<1)
		ldi YH, high(SCORES<<1)

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

MAIN:
		; Loop for Recieve Complete
		lds mpr, UCSR1A
		sbrs mpr, RXC1		; If Recieve Complete Skip rjmp below
		rjmp MAIN

		; Load Recieved Signal into Signal Register
		lds sigr, UDR1

		; Copy to ID register
		mov idreg, sigr

		andi sigr, $F0	; Mask Out ID
		andi idreg, $0F	; Mask Out Command

		cpi sigr, Join	; Compare Command against Join
		breq addPlayer	; If Equal, add Player

		rjmp MAIN		; Otherwise, return to beginning and wait for new signal

		addPlayer:
			
			; Store ID in Player Array
			st X+, idreg
			
			; Increment Player Count
			inc playercnt
			
			; Check If We have Number of players specified
			cpi playercnt, NUM_PLAYERS
			brne MAIN	; If Not Return to Main and wait for new signal
			
			rcall STARTGAME	; If so, call STARTGAME

		rjmp MAIN


STARTGAME:
		; Enable Transmitter
		ldi mpr, (1<<TXEN1)
		sts UCSR1B, mpr

		; Load Start Game Command and ServerID
		ldi mpr, Start|BoardID
		sts UDR1, mpr	; Send Command

		; CHECK FOR RECIEVE GAME

		; After All Boards Recieved, Call ASKSCORES
		rcall ASKSCORES
		
		ret

ASKSCORES:




;***********************************************************
;*	Stored Program Data
;***********************************************************
.dseg

PLAYERS:
.byte NUM_PLAYERS
END_PLAYERS:

SCORES:
.byte NUM_PLAYERS
END_SCORES:

		
.cseg
;***********************************************************
;*	Additional Program Includes
;***********************************************************

