;;
; Tokenizer utilities
;;

		.section code

;;
; Calculate hash of identifier in line buffer.
;
; Computes a simple hash value for the identifier currently selected in the
; line buffer (from `identStart` to `identTypeEnd`). The hash is calculated as
; the sum of all character values in the identifier, including type and array
; markers. This hash is used for fast token table lookups.
;
; \in identStart         Start index of the identifier in the line buffer
; \in identTypeEnd      End index of the identifier in the line buffer
; \out identHash        Computed hash value of the identifier
; \sideeffects          - Modifies `A` register
;                       - Preserves X register
; \see                  identStart, identTypeEnd, identHash, lineBuffer
;;
TOKCalculateHash:
		phx
		ldx 	identStart 					; needs to be same as in tokens.py - simple sum at present.
		lda 	#0
_TCHLoop:
		clc
		adc 	lineBuffer,x
		inx
		cpx 	identTypeEnd 				; do the whole thing including type and array markers.
		bne 	_TCHLoop
		sta 	identHash 					; save the hash
		plx
		rts

;;
; Extract line number from line buffer.
;
; Parses a decimal line number from the line buffer starting at position `X`.
; The line number is accumulated into `tokenLineNumber` using decimal
; arithmetic (multiply by 10 and add digit). Does not initialize
; `tokenLineNumber` to zero, allowing for accumulation of multiple digit
; groups.
;
; \in X             Current position in `lineBuffer` (pointing at first digit)
; \out X            Updated position after the line number
; \sideeffects      - Updates `tokenLineNumber` with parsed value
;                   - Modifies `A` register during parsing
; \see              tokenLineNumber, lineBuffer
;;
TOKExtractLineNumber:
		lda 	tokenLineNumber+1 			; push current value on stack
		pha
		lda 	tokenLineNumber
		pha
		jsr 	_LCLNTimes2 				; line # x 2
		jsr 	_LCLNTimes2 				; line # x 4
		;
		clc 								; add stacked value
		pla
		adc 	tokenLineNumber
		sta 	tokenLineNumber
		pla
		adc 	tokenLineNumber+1
		sta 	tokenLineNumber+1 			; line # x 5
		jsr 	_LCLNTimes2 				; line # x 10
		;
		lda 	lineBuffer,x 				; get and consume character
		inx
		and 	#15 						; add to line #
		clc
		adc 	tokenLineNumber
		sta 	tokenLineNumber
		bcc 	_TLENNoCarry
		inc 	tokenLineNumber+1
_TLENNoCarry:
		lda 	lineBuffer,x 				; more digits ?
		cmp 	#'0'
		bcc 	_TLENExit
		cmp 	#'9'+1
		bcc 	TOKExtractLineNumber
_TLENExit:
		rts

_LCLNTimes2:
		asl 	tokenLineNumber 			; doubles tokenLineNumber.
		rol 	tokenLineNumber+1
		rts

;;
; Write byte to token buffer.
;
; Writes a single byte to the tokenized output buffer at the current position
; and advances the buffer pointer. Used to output tokens, length bytes, and
; data during the tokenization process.
;
; \in A             Byte value to write to the token buffer
; \sideeffects      - Writes byte to `tokenBuffer` at current `tokenOffset`
;                   - Increments `tokenOffset` to next available position
;                   - Preserves `X` and `A` registers
; \see              tokenBuffer, tokenOffset
;;
TOKWriteByte:
		phx
		ldx 	tokenOffset 				; next slot to write to
		sta 	tokenOffset,x 				; write byte out
		inc 	tokenOffset 				; advance slot.
		plx
		rts

		.send code
