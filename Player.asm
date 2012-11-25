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
.def 	counter = r14
.def	sigr 	= r15
.def	mpr 	= r16

.def 	scorereg= r23
.def 	ReadCnt = r24
.def	timer	= r25

.equ	BOARDID	= 0b00000001	; Unique Board ID = $01 (MSB = 0) 

.equ	TIMEOUT = 100			; Timeout Interval

.equ	ScoreAddr = $0130		; Address of ASCII counter text

; Use these commands between the server and player (OR WITH 4 BIT BOT ID TO INCLUDE ID)
; MSB = 1 thus:
.equ	NEW		= 0b10000000	;0b1000XXXX New Game Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	JOIN 	= 0b10010000	;0b1001XXXX Join Game Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	START 	= 0b10100000	;0b1010XXXX Start Game Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	RECIEVE	= 0b11000000	;0b1010XXXX Recieved Command Signal (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	ASK 	= 0b10110000	;0b1011XXXX Ask for Score Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)
.equ	WINNER 	= 0b11110000	;0b1111XXXX Winner Command (OR WITH 4 BIT BOT ID TO INCLUDE ID)

.equ	WskrY 	= 0				; "Yes" Whisker Input Bit
.equ	WskrN 	= 1				; "No" Whisker Input Bit
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$001E					; Timer Overflow 0 Interrupt Vector
		rcall INCREMENTRANDOM 	; Increment the Random Number Value
		reti

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
		ldi mpr, $03
		out PORTD, mpr			; Set Input to Hi-Z
		out PIND, mpr
	
		;USART1
		; Enable transmitter
		ldi mpr, (1<<TXEN1)
		sts UCSR1B, mpr

		; Set frame format: 8data, 2 stop bit
		ldi mpr, (1<<USBS1)|(3<<UCSZ10)
		sts UCSR1C,mpr

		; Set baudrate at 2400bps
		ldi mpr, $01
		sts UBRR1H, mpr
		ldi mpr, $A0
		sts UBRR1L, mpr

		;LCD 
		; Call Display Driver
		rcall LCDInit

		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt

		; Init variable registers
		ldi ZL, LOW(TITLE_STRING << 1)
		ldi ZH, HIGH(TITLE_STRING<< 1)
		ldi YL, LOW(LCDLn1Addr)
		ldi YH, High(LCDLn1Addr)

		; Move Title String from Program Memory to Data Memory
		initLine1:		
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	initLine1	; Continue untill all data is read
			rcall	LCDWrLn1	; WRITE LINE 1 DATA


		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt

		; Init variable registers
		ldi ZL, LOW(SEARCH_STRING << 1)
		ldi ZH, HIGH(SEARCH_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)


		; Move Search String from Program Memory to Data Memory
		initLine2:		
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	initLine2	; Continue untill all data is read
			rcall	LCDWrLn2	; WRITE LINE 2 DATA


;***********************************************************
;*	Functions and Subroutines
;***********************************************************
MAIN:
		rcall GAME

;		rcall SENDJOIN
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
;		cpse sigr, Start
;		rjmp MAIN2
;
;		rcall SETUP
;		rjmp MAIN


;***********************************************************
; Func: TransJoin
; Desc: Sends the join game signal out of it's IR
; 		
;***********************************************************
INCREMENTRANDOM:

		ret

;***********************************************************
; Func: TransJoin
; Desc: Sends the join game signal out of it's IR
; 		
;***********************************************************
SENDJOIN:
		push mpr
		in mpr, SREG
		push mpr

		; Enable transmitter
		ldi mpr, (1<<TXEN1)
		sts UCSR1B, mpr

		; Load Join Game Command with BoardID
		ldi mpr, JOIN|BOARDID
		sts UDR1, mpr			; Transmit Signal

		transmitLoop:			; Wait for any transmissions to finish
			lds mpr, UCSR1A	
			sbrs mpr, UDRE1	
			rjmp transmitLoop	; Loop if transmission not finished

		; Enable reciever
		ldi mpr, (1<<RXEN1)
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
SETUP:
		; WRITE GAME STRING
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt

		; Init variable registers
		ldi ZL, LOW(GAME_STRING << 1)
		ldi ZH, HIGH(GAME_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)

		; Move Game String from Program Memory to Data Memory
		writeGame:		
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	writeGame	; Continue untill all data is read

		; WRITE SCORE STRING
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt/2

		; Init variable registers
		ldi ZL, LOW(SCORE_STRING << 1)
		ldi ZH, HIGH(SCORE_STRING << 1)
		ldi YL, LOW(LCDLn1Addr)
		ldi YH, High(LCDLn1Addr)

		; Move Score String from Program Memory to Data Memory
		beginScore:		
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	beginScore	; Continue untill all data is read
		
		

		rcall GAME

		ret
;***********************************************************
; Func: StartGame
; Desc: Begins the Game
; 		
;***********************************************************
GAME:
		
		mov		mpr, counter
		andi	mpr, $1F

		add		scorereg, mpr
		
		mov		mpr, scorereg	; MOVE DATA TO MPR FOR THE B2A CALL
								; SET THE INITIAL X-PTR ADDRESS
		ldi		XL, low(ScoreAddr)
		ldi		XH, high(ScoreAddr)
		rcall	Bin2ASCII		; CALL BIN2ASCII TO CONVERT DATA
								; NOTE, COUNT REG HOLDS HOW MANY CHARS WRITTEN
		
		; Write data to LCD display
		ldi		ReadCnt, 2		; always write two chars to overide existing data in LCD
		ldi		line, 1			; SET LINE TO 1 TO WRITE TO LINE 1
		ldi		count, 9		; SET COUNT TO 10 TO START WRITTING TO THE TENTH INDEX
		writeScore:
			ld		mpr, X+		; LOAD MPR WITH DATA TO WRITE
			rcall	LCDWriteByte; CALL LCDWRITEBYTE TO WRITE DATA TO LCD DISPLAY
			inc		count		; INCREMENT COUNT TO WRITE TO NEXT LCD INDEX
			dec		ReadCnt		; decrement read counter
			brne	writeScore	; Countinue until all data is written

		; Poll Player Input
		hitLoop:
			rcall LCDWrite		; Shows Score and Asks if player wants to 'Draw Again?'
			
			; Get whisker (button) input from Port D 
			in		mpr, PIND	
			; Mask Out All but Hit and Stay Buttons
			andi	mpr, (1<<WskrY|1<<WskrN)				

			; Check if 'Hit' was Pressed
			cpi		mpr, (1<<WskrY)	
			breq	GAME		; If so, get another random value
			
			; Check if 'Stay' was Pressed
			cpi		mpr, (1<<WskrN)
			brne	hitLoop		; If not, return to loop
			
		; DISPLAY WAITING STRING
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt

		; Init variable registers
		ldi ZL, LOW(WAIT_STRING << 1)
		ldi ZH, HIGH(WAIT_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)

		; Move Wait String from Program Memory to Data Memory
		writeWait:			
		lpm		mpr, Z+		; Read Program memory
		st		Y+, mpr		; Store into Data Memory
		dec		ReadCnt		; Decrement Read Counter
		brne	writeWait	; Continue untill all data is read
		rcall	LCDWrLn2	; WRITE LINE 2 DATA
			
		; Wait to be asked for score
		rcall	WAITFORASK
		
		ret


;***********************************************************
; Func: WaitForAsk
; Desc: Waits for The Server's Signal
; 		
;***********************************************************
WAITFORASK:
		
		; Enable reciever
		ldi mpr, (1<<RXEN1)
		sts UCSR1B, mpr

		rcvLoop:
			lds mpr, UCSR1A
			sbrs mpr, RXC1		; Check if Recieve Complete
			rjmp rcvLoop		; If not, wait for Recieve Complete

		;If complete, get signal and check if it is Ask w/ correct BotID
		lds sigr, UDR1 			; Get signal from buffer
		ldi mpr, (BOARDID)|(ASK)
		CPSE sigr, mpr
		rjmp WAITFORASK			;If not equal, loop around
		

		scoreLoop:
			; Enable Transmitter
			ldi mpr, (1<<TXEN1)
			sts UCSR1B, mpr			

			sts UDR1, scorereg	; Send Score
			
			; Load Timeout Value to Timer Register
			ldi timer, TIMEOUT
		
			; Enable Reciever
			ldi mpr, (1<<RXEN1)
			sts UCSR1B, mpr

			checkRcvd:
				dec timer		; Decrement the Timer
				breq scoreLoop	; If 0, Resend Start Game Command
				lds mpr, UCSR1A	; Otherwise, check if Recieve Complete
				sbrs mpr, RXC1
				rjmp checkRcvd	; If not, loop for Recieve Complete

				lds sigr, UDR1	; Load Recieved Signal into Signal Register
				
				ldi mpr, BOARDID; Move Our ID to MPR
				ori mpr, RECIEVE; OR with Recieve Command to get Expected Signal

				cp sigr, mpr	; Compare Recieved vs Expected Signal
				brne checkRcvd	; If not equal, wait for new Recieve Complete

		; Once Score is Recieved, Call WAITFORWINNER
		rcall WAITFORWINNER

		ret


;***********************************************************
; Func: WaitForWinner
; Desc: Waits for The Server's Signal with the Winner
; 		
;***********************************************************

WAITFORWINNER:
		
		; Enable Reciever
		ldi mpr, (1<<RXEN1)
		sts UCSR1B, mpr
		
		rcvLoop2:
			lds mpr, UCSR1A
			sbrs mpr, RXC1		; Check if Recieve Complete
			rjmp rcvLoop2		; If not, wait for Recieve Complete

		lds sigr, UDR1 			; Get signal from buffer
		ldi mpr, (BOARDID)|(WINNER)
		cp mpr, sigr			; Check if We Won
		breq WIN				; If So, Branch to Win

		mov mpr, sigr
		andi mpr, $F0			; Mask out ID
		cpi	mpr, WINNER			; Compare against Win Command
		breq LOSE				; If Equal, We Lost
		rjmp WAITFORWINNER		; If not, Loop for Winner COmmand

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		PROJECT WINNER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WIN:
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt
		
		; Init variable registers
		ldi ZL, LOW(WIN_STRING << 1)
		ldi ZH, HIGH(WIN_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)

		; Move string1 from Program Memory to Data Memory
		WriteWinner:		; Loop for Writing Winner
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	WriteWinner	; Continue until all data is read
			rcall	LCDWrLn2	; WRITE LINE 2 DATA
		
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		PROJECT LOSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	LOSE:
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt
		
		; Init variable registers
		ldi ZL, LOW(LOSE_STRING << 1)
		ldi ZH, HIGH(LOSE_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)

		; Move string1 from Program Memory to Data Memory
		WriteLoser:		; Loop for Writing Loser
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	WriteLoser	; Continue until all data is read
			rcall	LCDWrLn2	; WRITE LINE 2 DATA
		
		ret


;***********************************************************
;*	Stored Program Data
;***********************************************************

TITLE_STRING:
.DB		"OSU-AVRBLACKJACK"
TITLE_STRING_END:

SEARCH_STRING:
.DB		" FINDING GAME.. "
SEARCH_STRING_END:

SCORE_STRING:
.DB		" SCORE: "
SCORE_STRING_END:

GAME_STRING:
.DB		"0 = HIT|1 = STAY"
GAME_STRING_END:

WAIT_STRING:
.DB		"   WAITING...   "
WAIT_STRING_END:

WIN_STRING:
.DB		"  YOU WON! :)   "
WIN_STRING_END:

LOSE_STRING:
.DB		"  YOU LOST! :(  "
LOSE_STRING_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

