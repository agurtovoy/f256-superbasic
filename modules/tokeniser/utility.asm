;;
; Tokenizer utilities
;;

		.section code

;;
; Calculate hash of identifier in line buffer.
;
; Computes a simple hash for the identifier in the line buffer, from
; `identStart` to `identTypeEnd`. The hash is the sum of all character values
; in the identifier, including any type marker. Since identifiers are
; case-insensitive, lowercase letters are converted to uppercase before
; summing. The hash is used for fast token table lookups.
;
; \in identStart        Start index of the identifier in the line buffer
; \in identTypeEnd      End index of the identifier in the line buffer
; \out identHash        Computed hash value of the identifier
; \sideeffects          - Modifies `A` register
;                       - Updates `identHash` with the computed hash value
;                       - Preserves `X` register
; \see                  identStart, identTypeEnd, identHash, lineBuffer
;;
TOKCalculateHash:
		; the hash algorithm needs to match the one used to generate the token
		; table in `tokens.py` -- a simple sum at present
		phx
		ldx 	identStart					; `X` points to start of identifier
		stz 	identHash					; reset the hash

	_loop:
		lda 	lineBuffer,x				; load character in A
		cmp 	#'_'+1						; check if uppercase, _, or a type marker ($, #)
		bcc 	_upper						;
		eor 	#$20						; lowercase, convert to uppercase by flipping bit 5
	_upper:
		clc									; clear carry for addition
		adc 	identHash					; add hash to the character in A to get the new hash
		sta 	identHash 					; save the updated hash
		inx									; advance to next character
		cpx 	identTypeEnd 				; continue until we consumed both the
		bne 	_loop						; identifier and the type marker

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
		jsr 	_ln_times_2 				; line # x 2
		jsr 	_ln_times_2 				; line # x 4
		;
		clc 								; add stacked value
		pla
		adc 	tokenLineNumber
		sta 	tokenLineNumber
		pla
		adc 	tokenLineNumber+1
		sta 	tokenLineNumber+1 			; line # x 5
		jsr 	_ln_times_2 				; line # x 10
		;
		lda 	lineBuffer,x 				; get and consume character
		inx
		and 	#15 						; add to line #
		clc
		adc 	tokenLineNumber
		sta 	tokenLineNumber
		bcc 	_no_carry
		inc 	tokenLineNumber+1

	_no_carry:
		lda 	lineBuffer,x 				; more digits ?
		cmp 	#'0'
		bcc 	_exit
		cmp 	#'9'+1
		bcc 	TOKExtractLineNumber

	_exit:
		rts

	_ln_times_2:
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
