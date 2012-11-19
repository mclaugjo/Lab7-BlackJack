;***********************************************************
;*
;* 	ECE 375 Lab 7 - Fall 2012
;*	Remote.asm
;*
;*	This is the remote for Lab 7 of ECE 375.
;*
;*
;***********************************************************
;*
;*	 Author: Joshua McLaughlin
;*	   Date: November 11th 2012
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16
.def	sigr = r17
.def	waitcnt = r18
.def	ilcnt	= r19
.def	olcnt = r20
.def ReadCnt = r23

.equ	BotID =  0b00000001		; Unique BotID = $33 (MSB = 0) 

; Use these commands between the server and player (OR WITH 4 BIT BOT ID TO INCLUDE ID)
; MSB = 1 thus:
.equ	New		= 0b10000000				;0b1000XXXX New Game Command X's will have ID
.equ	Join 	= 0b10010000				;0b1001XXXX Join Game Command
.equ	Start 	= 0b10100000				;0b1010XXXX Start Game Command 
.equ	Winner 	= 0b11110000				;0b1111XXXX Winner Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	Ask 	= 0b10110000				;0b1011XXXX Ask for Scored Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)

.equ	WskrY = 0				; "Yes" Whisker Input Bit
.equ	WskrN = 1				; "No" Whisker Input Bit
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
	
		; Initialize LCD Display
		rcall LCDInit			; An RCALL statement		

		; Init variable registers
		ldi ZL, LOW(STRING_BEG << 1)
		ldi ZH, HIGH(STRING_BEG << 1)
		ldi YL, LOW(LCDLn1Addr)
		ldi YH, High(LCDLn1Addr)

		
		;Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt


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

		; Move strings from Program Memory to Data Memory
INIT_LINE1:		;Loop for first line
		lpm		mpr, Z+			; Read Program memory
		st		Y+, mpr			; Store into memory
		dec		ReadCnt			; Decrement Read Counter
		brne	INIT_LINE1		; Continue untill all data is read
		rcall	LCDWrLn1		; WRITE LINE 1 DATA


		; Init variable registers
		ldi ZL, LOW(STRING_TWO << 1)
		ldi ZH, HIGH(STRING_TWO << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)

		
		;Set read count register to be the mac LCD size
		ldi ReadCnt, LCDMaxCnt


		; Move strings from Program Memory to Data Memory
INIT_LINE2:			;Loop for second line
		lpm		mpr, Z+			; Read Program memory
		st		Y+, mpr			; Store into memory
		dec		ReadCnt			; Decrement Read Counter
		brne	INIT_LINE2		; Continue untill all data is read
		rcall	LCDWrLn2		; WRITE LINE 2 DATA


;***********************************************************
;*	Functions and Subroutines
;***********************************************************
MAIN:
		rcall StartGame

;		rcall TransJoin
;MAIN2:		
;		lds mpr, UCR1A
;		sbrs mpr, RXC1
;		rjmp MAIN
;
;		lds sigr, UDR1
;
;		mov idreg, sigr
;		andi sigrm $F0
;		andi idreg, $0F
;
;		cpi sigr, Start
;		breq StartGame
;
;		rjmp MAIN2


;***********************************************************
; Func: TransJoin
; Desc: Sends the join game signal out of it's IR
; 		
;***********************************************************
TransJoin:
		push mpr
		in mpr, SREG
		push mpr

		;Enable transmitter and disable receiver
		ldi mpr, (1<<TXEN1)|(1<<RXCIE1)
		sts UCSR1B, mpr

		;Load Send Game Signal
		ldi mpr, Join
		sts UDR1, mpr		;Transmit Signal

transmitLoop:	; Wait for any transmissions to finish
		lds mpr, UCSR1A	
		sbrs mpr, UDRE1	
		rjmp transmitLoop	; Loop if transmission not finished

		;Enable reciever and disable transmitter
		ldi mpr, (1<<RXEN1)|(1<<RXCIE1)
		sts UCSR1B, mpr	

		pop mpr
		out SREG, mpr
		pop mpr
		ret

;***********************************************************
; Func: StartGame
; Desc: Begins the Game
; 		
;***********************************************************
StartGame:
		
		;NEED RAN VALUE

Loop1:
		rcall LCDWrite

		in		mpr, PIND			;Get whisker (button) input from Port D 
		andi	mpr, (1<<WskrY|1<<WskrN)	
		cpi		mpr, (1<<WskrN)		;Check for Yes Input
		brne	NEXT				;Call Next if not Yes
		rjmp	StartGame			;If Yes, go back to StartGame

NEXT:
		cpi		mpr, (1<<WskrY)				;Check for No Input
		brne	Loop1				;If not No, return to loop
		rcall	WaitForAsk		;If no, Stop adding score and wait for server in WaitForAsk
		rjmp	Loop1				;If no input, go back to loop


;***********************************************************
; Func: WaitForAsk
; Desc: Waits for The Server's Signal
; 		
;***********************************************************
WaitForAsk:

		lds mpr, UCSR1A
		sbrs mpr, RXC1		; Check if Recieve Complete
		rjmp WaitForAsk		; If not, wait for Recieve Complete

		;If complete, get signal and check if it is Ask w/ correct BotID
		lds sigr, UDR1 	; Get signal from buffer
		ldi mpr, (BotID)|(Ask)
		CPSE sigr, mpr
		rjmp WaitForAsk		;If not equal, loop around

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRANSMIT VALUE AND CALL WAITWINNER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;?????????????????
		rcall WaitForWinner



;***********************************************************
; Func: WaitForWinner
; Desc: Waits for The Server's Signal with the Winner
; 		
;***********************************************************

WaitForWinner:

		lds mpr, UCSR1A
		sbrs mpr, RXC1			; Check if Recieve Complete
		rjmp WaitForWinner		; If not, wait for Recieve Complete

		lds sigr, UDR1 	; Get signal from buffer
		ldi mpr, (BotID)|(Winner)
		breq WIN
		rjmp Check		;If not equal, loop around
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		PROJECT WINNER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WIN:


Check:
		andi sigr, $F0
		cpi	sigr, Winner
		breq Loser
		rjmp WaitForWinner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		PROJECT LOSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
Loser:
;?????????????????????


;***********************************************************
;*	Stored Program Data
;***********************************************************

STRING_BEG:
.DB		"      HIT?      "
STRING_END:

STRING_TWO:
.DB		"      TEST      "
STRING_TWOB:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

