;***********************************************************
;*
;* 	ECE 375 Lab 7 - Fall 2012
;*	Server.asm
;*
;*	This is the server for Lab 7 of ECE 375. The Server waits 
;*	for enough players to join the game and sends each of them the 
;*	start game signal. Once the players have all played, it determines
;*	which player had the highest score and lets all of the players
;*	know who won.
;*
;*
;***********************************************************
;*
;*	 Author: Devlin Junker
;*	   Date: November 23rd 2012
;*
;***********************************************************
.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr 	= r16			; Multi-Purpose Register
.def	sigr	= r17			; Register to Hold Signal
.def	idreg	= r18			; Register to Hold Board ID
.def	winreg	= r19			; Register to Winner ID
.def	timer 	= r20

.equ	NUM_PLAYERS = 2

.equ	BOARDID	= 0b00000011	; Unique Board ID = $03 (MSB = 0) 

.equ	TIMEOUT = 100			; Timeout Interval

; Use these commands between the server and player (OR WITH 4 BIT BOT ID TO INCLUDE ID)
; MSB = 1 thus:
.equ	NEW		= 0b10000000	;0b1000XXXX New Game Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	JOIN 	= 0b10010000	;0b1001XXXX Join Game Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	START 	= 0b10100000	;0b1010XXXX Start Game Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	RECIEVE	= 0b11000000	;0b1010XXXX Recieved Command Signal (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	ASK 	= 0b10110000	;0b1011XXXX Ask for Score Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	WINNER 	= 0b11110000	;0b1111XXXX Winner Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)

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
		out DDRD, mpr			; Set Port D as Input
		ldi mpr, $F3
		out PORTD, mpr			; Set Input to Hi-Z
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
		sbrs mpr, RXC1			; If Recieve Complete Skip rjmp below
		rjmp MAIN

		; Load Recieved Signal into Signal Register
		lds sigr, UDR1

		; Copy to ID register
		mov idreg, sigr

		andi sigr, $F0			; Mask Out ID
		andi idreg, $0F			; Mask Out Command

		cpi sigr, JOIN			; Compare Command against Join
		breq addPlayer			; If Equal, add Player

		rjmp MAIN				; Otherwise, return to beginning and wait for new signal

		addPlayer:
			
			; Store ID in Player Array
			st X+, idreg
			
			; Check If Room for More Players
			cpi XL, low(END_PLAYERS<<1)
			brne MAIN			; If so, Return to Main and wait for new signal
			
			rcall STARTGAME		; If not, call STARTGAME

		rjmp MAIN


STARTGAME:
		
		ld idreg, -X			; Pre-Decrement X to return to last ID 

		startLoop:
			; Enable Transmitter
			ldi mpr, (1<<TXEN1)
			sts UCSR1B, mpr			

			mov mpr, idreg		; Move PlayerID into MPR
			ori mpr, START 		; OR with Start Game Command
			sts UDR1, mpr		; Send Command
			
			; Load Timeout Value to Timer Register
			ldi timer, TIMEOUT
		
			; Enable Reciever
			ldi mpr, (1<<RXEN1)
			sts UCSR1B, mpr

			checkRcvd:
				dec timer		; Decrement the Timer
				breq startLoop	; If 0, Resend Start Game Command
				lds mpr, UCSR1A	; Otherwise, check if Recieve Complete
				sbrs mpr, RXC1
				rjmp checkRcvd	; If not, loop for Recieve Complete

				lds sigr, UDR1	; Load Recieved Signal into Signal Register
				
				mov mpr, idreg	; Move ID we're waiting for to MPR
				ori mpr, RECIEVE; OR with Command to get Expected Signal

				cp sigr, mpr	; Compare Recieved vs Expected Signal
				brne checkRcvd	; If not equal, wait for new Recieve Complete
			
			; Check if Sent to All Players
			cpi XL, low(PLAYERS<<1)
			breq started		; If so, call ASK SCORES

			ld idreg, -X		; Otherwise, Load Next ID into ID Register
			rjmp startLoop		; And send next Start Game Command


	started:
		; After All Boards Recieved, Call ASKSCORES
		rcall ASKSCORES
		
		ret

ASKSCORES:
		ld idreg, X+			; Load First ID them Increment X

		scoreLoop:
			; Enable Transmitter
			ldi mpr, (1<<TXEN1)
			sts UCSR1B, mpr			

			; Load Command to be Sent
			mov mpr, idreg		; Move Player ID into MPR
			ori mpr, ASK		; OR with Ask Score Command
			sts UDR1, mpr		; Send Command
			
			; Load Timeout Value to TImer Register
			ldi timer, TIMEOUT
		
			; Enable Reciever
			ldi mpr, (1<<RXEN1)
			sts UCSR1B, mpr

			checkScore:
				dec timer		; Decrement the Timer
				breq scoreLoop	; If 0, Resend Ask Score Command
				lds mpr, UCSR1A
				sbrs mpr, RXC1	; Otherwise, check if Recieve Complete
				rjmp checkScore ; If not, loop for Recieve Complete

				lds sigr, UDR1

				sbrc sigr, 7	; Check if MSB is cleared (Score Value)
				rjmp checkScore	; If not, Wait for Score
				
			st Y+, sigr			; If so, Store Score in Score Array
			
			; Check if Recieved All Scores
			cpi YL, low(END_SCORES<<1)
			breq ended			; If so, call FINDWINNER

			ld idreg, X+		; Otherwise, get next ID
			rjmp scoreLoop		; And send next Ask Score Command

	ended:
		rcall FINDWINNER

		ret


FINDWINNER:
		ldi sigr, 0				; Start with Best Score of 0
		ldi winreg, 0			; Start with no Winner

		winnerLoop:
			ld idreg, -X		; Load ID Register Matching Score into ID Register
			ld mpr, -Y			; Load Score into MPR
			cp mpr, sigr		; Compare Score with Previous Best Score
			brlt keep			; If MPR < SIGR, keep what is in SIGR
			
			mov sigr, mpr		; Otherwise, store new highest value (MPR)
			mov winreg, idreg	; and Store Highest Scorers ID
			
			keep:

			; Check if All Scores Checked
			cpi idreg, low(PLAYERS<<1)
			brne winnerLoop		; If not, Loop to check next score
			
		rcall SENDWINNER		; Otherwise, Send the Winner Command

		ret

SENDWINNER:
		; Enable Transmitter
		ldi mpr, (1<<TXEN1)
		sts UCSR1B, mpr			
	
		; Create and Send Winner Command
		mov mpr, winreg			; Load Winner ID into MPR
		ori mpr, WINNER			; OR with Winner Command
		sts UDR1, mpr			; Send Command

		ret

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

;***********************************************************
;*	Additional Program Includes
;***********************************************************

