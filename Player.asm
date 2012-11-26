;***********************************************************
;*
;* 	ECE 375 Lab 7 - Fall 2012
;*	Player.asm
;*
;*	This is the BlackJack Player for Lab 7 of ECE 375. The Player
;*  joins a game and waits for the start game signal. When it is
;* 	told to start, it adds a random value (1 - 31) to the score and 
;*	asks the player if they want to hit or stay. Once the player 
;*	decides to stay, the Player waits until it is asked for its score 
;*	and then transmits it once it is it's turn. The Player then waits 
;*	to recieve the winner command, letting it know if it had the 
;*	highest score.
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

.def	waitcnt = r17				; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

.def 	scorereg= r23
.def 	ReadCnt = r24
.def	timer	= r25

.equ	BOARDID	= 0b00000110	; Unique Board ID = $01 (MSB = 0) ;

.equ	TIMEOUT = 255			; Timeout Interval

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
	
		;USART1
		; Enable reciever
		ldi mpr, (1<<RXEN1)|(1<<TXEN1)
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
		
		; Initialize Port B for output
		ldi mpr, $FF
		out DDRB, mpr		; Set Port B as Output
		ldi mpr, $00
		out PORTB, mpr		; Default Output set 0 

		; BUTTONS		
		; Initialize Port D for input
		ldi mpr, $00
		out DDRD, mpr			; Set Port D as Input
		ldi mpr, $03
		out PORTD, mpr			; Set Input to Hi-Z
		out PIND, mpr		

		; RANDOM NUM GENERATOR
		ldi	mpr, (1<<WGM01)|(1<<CS00)
		out	TCCR0, mpr			; Set timer prescalar to 0 and Clear Timer on Compare Match Mode
		ldi	mpr, 30				; Set Max Value
		out	OCR0, mpr				



;***********************************************************
;*	Functions and Subroutines
;***********************************************************
MAIN:
		;rcall SETUP			; FOR TESTING GAME

		; Loop for Recieve Complete
	;	lds mpr, UCSR1A
	;	sbrs mpr, RXC1			
	;	rjmp MAIN				; Loop Until Recieve Complete

		; Load Recieved Signal into Signal Register
	;	lds sigr, UDR1
		
	;	ldi mpr, NEW
		; Compare Signal with New Game Signal
	;	cp sigr, mpr
	;	brne MAIN				; If Not New Game Signal, return to Recieve Loop	

		rcall SENDJOIN			; If it is, send Join Command

		rjmp MAIN


;***********************************************************
; Func: TransJoin
; Desc: Sends the join game signal out of it's IR
; 		
;***********************************************************
SENDJOIN:
		push mpr
		in mpr, SREG
		push mpr
				
		; Load Join Game Command with BoardID
		ldi mpr, JOIN|BOARDID
		sts UDR1, mpr			; Transmit Signal
		
		; FOR DEBUG 
		out PORTB, mpr		
		
		; Wait Half Second
		call WAITFUNC		

		; Wait for Transmission to Complete
		finishJoin:
			lds mpr, UCSR1A	
			sbrs mpr, UDRE1	
			rjmp finishJoin		; Loop if transmission not finished
		
		; Load Timeout Value to Timer Register
		ldi timer, TIMEOUT


		checkRcvdJoin:
			dec timer			; Decrement the Timer
			breq restart		; If 0, Wait for another New Game Signal
			lds mpr, UCSR1A		; Otherwise, check if Recieve Complete
			sbrs mpr, RXC1
			rjmp checkRcvdJoin	; If not, loop for Recieve Complete

			lds sigr, UDR1		; Load Recieved Signal into Signal Register

			ldi mpr, BOARDID	; Move Our ID to MPR
			ori mpr, RECIEVE	; OR with Recieve Command to get Expected Signal

	;		out PORTB, mpr		
	;		rcall WAITFUNC			

			cp sigr, mpr		; Compare Recieved vs Expected Signal
			brne checkRcvdJoin	; If not equal, wait for new Recieve Complete

			out PORTB, sigr		
			rcall WAITFUNC

		rcall WAITFORSTART
		
	restart:
		pop mpr
		out SREG, mpr
		pop mpr
		ret


;***********************************************************
; Func: StartGame
; Desc: Begins the Game
; 		
;***********************************************************
WAITFORSTART:
		; WRITE WAIT STRING
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt

		; Init variable registers
		ldi ZL, LOW(WAIT_STRING << 1)
		ldi ZH, HIGH(WAIT_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)
		
	
		; Move Game String from Program Memory to Data Memory
		writeWaitStart:		
			lpm		mpr, Z+		; Read Program memory
			st		Y+, mpr		; Store into memory
			dec		ReadCnt		; Decrement Read Counter
			brne	writeWaitStart; Continue untill all data is read		
			rcall	LCDWrLn2	; WRITE LINE 2 DATA

		; Check for Recieved
		lds mpr, UCSR1A
		sbrs mpr, RXC1
		rjmp WAITFORSTART 		; Loop until Recieved

		; Load Recieved Signal into SIGR
		lds sigr, UDR1	
		
		; Load Expected Signal into SIGR
		ldi mpr, START|BOARDID

		; If not Expected Signal Jump to Recieve Loop
		cpse sigr, mpr
		rjmp WAITFORSTART

		; If is Expected Signal, Send Recieved Command
		; Load Recieved Command and BoardID into MPR
		ldi mpr, RECIEVE|BOARDID
		sts UDR1, mpr			; Send Command
		
		; Start Game
		rcall SETUP

		ret


;***********************************************************
; Func: StartGame
; Desc: Begins the Game
; 		
;***********************************************************
SETUP:
		; Clear Score Register
		clr scorereg
		
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
		ldi ReadCnt, LCDMaxCnt

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

		rcall LCDWrite		; Shows Score and Asks if player wants to 'Draw Again?'

		rcall GAME

		ret
;***********************************************************
; Func: StartGame
; Desc: Begins the Game
; 		
;***********************************************************
GAME:
		
		in		mpr, TCNT0		; Grab Random Value from counter register
		andi	mpr, $1F		; Mask so only 5 bits available (0 - 30)
		breq 	GAME			; If mpr == 0, get new value

		add		scorereg, mpr	; Add Value of 1 - 30 to score register
		
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

		cpi scorereg, 71
		brge bust

		rcall WAITFUNC
		
		; Poll Player Input
		hitLoop:			
			; Get whisker (button) input from Port D 
			in		mpr, PIND	
			
			; Flip Bits (Active Low)
			com mpr

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

		rjmp writeString

		bust:
		; DISPLAY LOSE STRING
		; Set read count register to be the max LCD size
		ldi ReadCnt, LCDMaxCnt

		; Init variable registers
		ldi ZL, LOW(LOSE_STRING << 1)
		ldi ZH, HIGH(LOSE_STRING << 1)
		ldi YL, LOW(LCDLn2Addr)
		ldi YH, High(LCDLn2Addr)


		; Move Wait String from Program Memory to Data Memory
		writeString:			
		lpm		mpr, Z+		; Read Program memory
		st		Y+, mpr		; Store into Data Memory
		dec		ReadCnt		; Decrement Read Counter
		brne	writeString	; Continue untill all data is read
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
		rcvLoop:
			lds mpr, UCSR1A
			sbrs mpr, RXC1		; Check if Recieve Complete
			rjmp rcvLoop		; If not, wait for Recieve Complete

		;If complete, get signal and check if it is Ask w/ correct BotID
		lds sigr, UDR1 			; Get signal from buffer
		ldi mpr, (BOARDID)|(ASK)
		CPSE sigr, mpr
		rjmp WAITFORASK			;If not equal, loop around
		
	;	out PORTB, sigr
	;	call WAITFUNC		

		scoreLoop:
			sts UDR1, scorereg	; Send Score
			
			; FOR DEBUGGING
			out PORTB, scorereg

			; Wait Half Second After Score Sent
			call WAITFUNC

			; Load Timeout Value to Timer Register
			ldi timer, TIMEOUT

	;		checkRcvd:
	;			dec timer		; Decrement the Timer
	;			breq scoreLoop	; If 0, Resend Start Game Command
	;			lds mpr, UCSR1A	; Otherwise, check if Recieve Complete
	;			sbrs mpr, RXC1
	;			rjmp checkRcvd	; If not, loop for Recieve Complete

	;			lds sigr, UDR1	; Load Recieved Signal into Signal Register
				
	;			ldi mpr, BOARDID; Move Our ID to MPR
	;			ori mpr, RECIEVE; OR with Recieve Command to get Expected Signal

	;			cp sigr, mpr	; Compare Recieved vs Expected Signal
	;			brne checkRcvd	; If not equal, wait for new Recieve Complete

		; Once Score is Recieved, Call WAITFORWINNER
		rcall WAITFORWINNER

		ret


;***********************************************************
; Func: WaitForWinner
; Desc: Waits for The Server's Signal with the Winner
; 		
;***********************************************************

WAITFORWINNER:
		; FOR DEBUGGING
		ldi mpr, $FF
		sts PORTB, mpr
		
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

		; SHOW WINNER
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
		
		rjmp END
		
		; SHOW LOSER	
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
	
	END:
		ret

;***********************************************************
; Func: WaitForWinner
; Desc: Waits for The Server's Signal with the Winner
; 		
;***********************************************************
WAITFUNC:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

		ldi 	waitcnt, TIMEOUT/2

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine


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
.DB		" SCORE:         "
SCORE_STRING_END:

GAME_STRING:
.DB		"1 = STAY|0 = HIT"
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

