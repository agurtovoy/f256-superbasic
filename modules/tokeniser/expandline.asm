;;
; Expand tokenized line back to human-readable source code.
;
; Utilities to convert tokenized program lines back to human-readable text
; format with syntax highlighting.
;;

		.section code

;;
; Set current syntax highlighting color.
;
; \in \1            Color name from `OptionsColors` structure.
; \sideeffects      - Modifies A register.
;                   - Calls `LCLWriteColor` for optimized color output.
; \see              LCLWriteColor, OptionsColors, OptionsDefaultColors
;;
write_color .macro
		lda 	OptionsStorage + OptionsColors.\1
		jsr 	LCLWriteColor
		.endm

;;
; Initialize tokenizer/detokenizer color settings.
;
; Resets all syntax highlighting colors to their default values.
;
; \out OptionsStorage   Updated with default color values from
;                       the `OptionsDefaultColors` table.
; \sideeffects          - Modifies registers A and X.
; \see                  OptionsDefaultColors, OptionsColors, write_color
;;
Export_TKInitialise:
        ldx 	#(size(OptionsColors) - 1)	; load index for last color entry
	_loop:
		lda 	OptionsDefaultColors,x		; load default color value
		sta 	OptionsStorage,x			; store in active options
		dex									; move to previous entry
		bpl 	_loop						; continue until all copied (X >= 0)
		rts

;;
; Convert tokenized line to formatted text with syntax highlighting.
;
; Converts tokens back to human-readable text with proper formatting,
; indentation, and color coding. Used by the [list] command and error
; reporting to display source code.
;
; The resulting `tokenBuffer` contains:
;
;  - Color-highlighted line number with padding
;  - Block indentation based on structure level
;  - Syntax-highlighted tokens with embedded color codes
;  - Formatted strings/constants with appropriate delimiters
;  - Zero-byte terminator
;
; \in A                 Indent adjustment (-128 to +127) for structured formatting.
; \in codePtr           Points to tokenized line to convert.
; \out tokenBuffer      Contains formatted ASCIIZ text with embedded color codes.
; \out tbOffset         Set to length of output text.
; \out listIndent       Updated based on indent adjustment parameter.
; \sideeffects          - Modifies registers A, X, Y and various zero page locations.
;                       - Modifies `tokenBuffer`, `tbOffset`, and `currentListColour`.
;                       - May update `listIndent` for structured code formatting.
; \see                  LCLWrite, LCLWriteColor, write_color macro
;;
Export_TKListConvertLine:
		pha 								; save indent on the stack
		stz 	tbOffset					; reset token buffer position
		stz 	tokenBuffer					; write empty string to the token buffer
		stz 	currentListColour			; reset current color
		.write_color LineNumber				; set color for line numbers
		;
		;		Output the line number
		;
		ldy 	#2 							; convert line number to string
		.cget
		tax
		dey
		.cget
		jsr 	LCLWriteNumberXA
		;
		;		Pad out for indentation.
		;
		pla 								; adjustment to indent
		pha 								; save on stack
		bpl 	_no_adjust 					; don't adjust indent if +ve, do after.
		clc 								; add to list indent and make 0 if goes -ve.
		adc 	listIndent
		sta 	listIndent
		bpl 	_no_adjust
		stz 	listIndent

	_no_adjust:
		clc		 							; work out actual indent.
		lda 	listIndent
		asl 	a
		adc 	#7
		sta 	zTemp0

	_pad_out:
		lda 	#' '						; pad out to 6+indent characters
		jsr 	LCLWrite
		lda 	tbOffset
		cmp 	zTemp0
		bne 	_pad_out
		ldy 	#global.FIRST_TOKEN_OFFSET	; set Y to first token offset
		;	-------------------------------------------------------------------
		;
		;							Main List Loop
		;
		;	-------------------------------------------------------------------

	_main_loop:
		.write_color Punctuation 			; default listing color
		.cget 								; get next token
		cmp 	#KWC_EOL 					; end of line ?
		beq 	_exit
		;
		cmp 	#16 						; 0-5 are the double punctuations
		bcc 	_doubles
		cmp 	#32 						; 16-31 are shifted punctuation from 64-91
		bcc 	_shift_punct
		cmp 	#64 						; 32-64 are as stored, punc and digits
		bcc 	_punctuation
		cmp 	#128 						; 64-127 are variable identifiers.
		bcc 	_identifiers
		cmp 	#254 						; 128-253 are tokenized words
		bcc 	_tokens
		jmp 	_data 						; 254-5 are data objects
		;
		;		Exit - do +ve indent here.
		;
	_exit:
		pla 								; get old indent adjust
		bmi 	_exit2
		clc 								; add to indent if +ve
		adc 	listIndent
		sta 	listIndent
	_exit2:
		rts
		;	-------------------------------------------------------------------
		;
		;					  Doubles << >> <= >= <> (0-5)
		;
		;	-------------------------------------------------------------------

	_doubles:
		pha
		lsr 	a 							; put bit 2 into bit 1
		and 	#2
		ora 	#60 						; make < >
		jsr 	LCLWrite
		pla 								; restore, do lower bit
		and 	#3
		ora 	#60
		bra		_punctuation 				; print, increment, loop

		;	-------------------------------------------------------------------
		;
		;				Upper punctuation (was 64-127) (16-31)
		;
		;	-------------------------------------------------------------------

	_shift_punct:
		tax 								; save in X
		and 	#7 							; lower 3 bits
		beq 	_no_add
		ora 	#24 						; adds $18 to it.
	_no_add:
		cpx 	#24 						; if >= 24 add $20
		bcc 	_no_add2
		ora 	#32 						; adds $20
	_no_add2:
		ora 	#$40 						; shift into 64-127 range and fall through.

		;	-------------------------------------------------------------------
		;
		;							Punctuation (32-63)
		;
		;	-------------------------------------------------------------------

	_punctuation:
		cmp 	#'.'
		beq 	_is_const
		cmp 	#'0'
		bcc 	_not_const
		cmp 	#'9'+1
		bcs 	_not_const
	_is_const:
		pha
		.write_color Constant
		pla

	_not_const:
		;
		; add a space if `:`
		;
		cmp 	#':' 						;
		bne 	_continue
		jsr 	LCLWrite 					; write ':'
		lda 	#' '						; follow with a space

	_continue:
		jsr 	LCLWrite 					; write the character in `A`
		iny 								; next token
		bra 	_main_loop 				; go round again.

		;	-------------------------------------------------------------------
		;
		;							Identifiers (64-127)
		;
		;	-------------------------------------------------------------------

	_identifiers:
		clc 								; convert to physical address
		adc 	#((VariableSpace >> 8) - $40) & $FF
		sta 	zTemp0+1
		iny
		.cget
		sta 	zTemp0
		iny
		phy 								; save position

		jsr		LCLAddLeadingSpaceIfNeeded ; add leading space if needed
		.write_color Identifier 			; set list color
		ldy 	#7 							; output the identifier at +8

	_out_identifier:
		iny
		lda 	(zTemp0),y					; bit 7 set = end.
		and 	#$7F
		jsr 	LCLLowerCase
		jsr 	LCLWrite
		lda 	(zTemp0),y				 	; ends when bit 7 set.
		bpl 	_out_identifier
		ply 								; restore position
		jsr 	LCLAddSpaceIfNeeded
		jmp 	_main_loop

		;	-------------------------------------------------------------------
		;
		;							Tokens (129-253)
		;
		;	-------------------------------------------------------------------

	_tokens:
		tax 								; token in X
		.set16 	zTemp0,KeywordSet2 			; identify keyword set
		cpx 	#$82
		beq 	_use_shift
		.set16 	zTemp0,KeywordSet1
		cpx 	#$81
		beq 	_use_shift
		.set16  zTemp0,KeywordSet0
		bra 	_no_shift

	_use_shift:								; skip over token if using $81/$82 shifts
		iny

	_no_shift:
;		.cget 								; get the token again
;		cmp 	#KWD_COLON 					; check if :
;		beq 	_no_shift_no_space
		;jsr 	LCLCheckSpaceRequired 		; do we need a space ?

;	_no_shift_no_space:
		.cget 								; get the token again
		tax 								; into X

	_find_text:
		dex
		bpl 	_found_text 				; found text.
		lda 	(zTemp0) 					; length of text
		inc 	a 							; one extra for size
		sec 								; one extra for checksum
		adc 	zTemp0 						; go to next token
		sta 	zTemp0
		bcc 	_find_text
		inc 	zTemp0+1
		bra 	_find_text

	_found_text:
		phy 								; save List position
		lda 	(zTemp0)					; count to print
		tax

		jsr		LCLAddLeadingSpaceIfNeeded ; add leading space if needed

		.write_color Token
		ldy 	#2

	_copy_token:								; copy token out.
		lda 	(zTemp0),y
		jsr 	LCLLowerCase
		jsr 	LCLWrite
		iny
		dex
		bne 	_copy_token
;		cmp 	#"(" 						; if last char not ( print a space
;		beq 	_no_space
;		lda 	#' '
;		jsr 	LCLWrite

;	_no_space:
		ply 								; restore position.
		iny 								; next token
		jsr 	LCLAddSpaceIfNeeded

		jmp 	_main_loop 				; and go around again.

		;	-------------------------------------------------------------------
		;
		;							Data (254-5)
		;
		;	-------------------------------------------------------------------

	_data:
		pha 								; save type $FE/$FF
		ldx 	#'$' 						; figure out $ or "
		cmp 	#$FE
		beq 	_have_opener
		ldx 	#'"'
		.write_color Data
		;
		;		Check for comment on line by itself.
		;
		cpy 	#4 							; must be 2nd thing on line
		bne 	_have_opener
		dey 								; what precedes it ?
		.cget
		iny
		cmp 	#KWD_QUOTE 					; if quote
		bne 	_have_opener
		lda 	#9 							; tab
		jsr 	LCLWrite
		lda 	OptionsStorage + OptionsColors.BgComment
		bmi 	_have_opener
		ora 	#$90
		jsr 	LCLWrite
		.write_color FgComment

	_have_opener:
		txa 								; output prefix (# or ")
		jsr 	LCLWrite
		iny 								; get count
		.cget
		tax
		iny 								; point at first character

	_out_data:
		.cget 								; get next
		cmp 	#0
		beq 	_no_print
		jsr 	LCLWrite

	_no_print:
		iny
		dex
		bne 	_out_data
		pla 								; closing " required ?
		cmp 	#$FF 						; not required for hex constant.
		bne 	_no_quote
		lda 	#'"'
		jsr 	LCLWrite
		lda 	EXTTextColour
		and 	#$0F
		ora 	#$90
		jsr 	LCLWrite

	_no_quote:
		jmp 	_main_loop


LCLAddLeadingSpaceIfNeeded:
		lda 	lcLastCharacter 			; `A` = last written character
		cmp 	#'"'
		beq 	_space						; add space after a string literal
		cmp 	#'0' 						;
		bcc 	_exit						; exit if less than '0'
		cmp 	#'9'						;
		bcs 	_exit						; exit if greater than '9'
	_space:
		lda 	#' '						;
		jsr 	LCLWrite					; write space to buffer
	_exit:
		rts


LCLAddSpaceIfNeeded:
		.cget								; get next token
		cmp 	#$40 						;
		bcs 	_trailing_space				; identifier or keyword, add trailing space
		cmp 	#KWD_9 						;
		bcs 	_out					; punctuation
		cmp 	#KWD_0 						;
		bcc 	_out					; punctuation

	_trailing_space:
		lda 	#' '
		jsr 	LCLWrite
	_out:
		rts


;;
; Write color control code only if it has changed.
;
; Outputs a color control code ($80-$8F) to the token buffer only if it differs
; from the current listing color. This minimizes redundant color changes in the
; output stream. The color value is always updated in `currentListColour`
; regardless of whether it's written.
;
; \in A                     Color value (0-15) to set.
; \out currentListColour    Updated to the new color value.
; \out tokenBuffer          May be updated with new color code if changed.
; \out tbOffset             May be incremented if color code is written.
; \sideeffects              - Modifies `A`.
; \see                      LCLWrite, .write_color
;;
LCLWriteColor:
		and 	#$0F
		ora 	#$80
		cmp 	currentListColour 			; has the colour changed
		sta 	currentListColour 			; (update it anyway)
		bne 	LCLWrite 					; if different, output it
		rts

;;
; Write character to token buffer.
;
; Writes a single character to the `tokenBuffer` at the current offset
; position `tbOffset` and null-terminates the buffer. Updates the last
; character tracker `lcLastCharacter` unless the character is a color
; control code (bit 7 set).
;
; \in A                 Character to write to the buffer.
; \out tokenBuffer      Updated with the new character at current offset.
; \out tbOffset         Incremented to point to the next write position.
; \out lcLastCharacter  Updated to the written character (unless it's a color
;                       code).
; \sideeffects          - Updates `tbOffset` and potentially `lcLastCharacter`
; \see                   LCLWriteColor, LCLDeleteLastSpace
;;
LCLWrite:
		phx
		ldx 	tbOffset 					; write out make ASCIIZ
		sta 	tokenBuffer,x
		stz 	tokenBuffer+1,x
		inc 	tbOffset 					; bump the position
		ora 	#0 							; don't update last character if colour data
		bmi 	_no_color
		sta 	lcLastCharacter
	_no_color:
		plx
		rts

;;
; Delete last character from token buffer if it's a space.
;
; Removes the last written character from the token buffer only if it's a
; space character (ASCII 32). This is used for cleaner formatting when
; certain punctuation (like colons) should not be preceded by spaces.
;
; \out tbOffset     May be decremented if last character was a space.
; \out tokenBuffer  Last character removed if it was a space.
; \sideeffects      - Only modifies tbOffset if last character is a space.
; \see              LCLWrite
;;
LCLDeleteLastSpace:
		pha									; save `A` and `X` registers
		phx									;
		ldx 	tbOffset					; load current buffer position into `X`
		beq 	_exit						; exit if buffer is empty
		lda 	tokenBuffer-1,x				; load last character in the buffer
		cmp 	#' '						; is it a space?
		bne 	_exit						; exit if not a space
		dec 	tbOffset					; remove the space by backing up position
	_exit:
		plx									; restore `A` and `X` registers
		pla									;
		rts

;;
; Check if a space is required before the next token.
;
; Determines whether a space character should be inserted before the next
; token based on the last character written to the output buffer. Adds a
; space if the last character was alphanumeric, '$', '#', or ')' to ensure
; proper token separation in the formatted output.
;
; \in lcLastCharacter   The last character written to the token buffer.
; \out tokenBuffer      May be updated with a space character.
; \out tbOffset         May be incremented if space is added.
; \sideeffects          - Modifies register A.
; \see                  LCLWrite, LCLLowerCase, lcLastCharacter
;;
LCLCheckSpaceRequired:
		lda 	lcLastCharacter 			; `A` = last written character
		cmp 	#'$' 						;
		beq 	_space						; string variable
		cmp 	#')'						; function call end
		beq 	_space
		cmp 	#'#'						; float variable
		beq 	_space
		;
		; check if alphanumeric character
		;
		jsr 	LCLLowerCase 				; convert to lowercase for easier checking
		cmp 	#"0" 						;
		bcc 	_exit						; exit if less than '0'
		cmp 	#"9"+1						;
		bcc 	_space						; if less or equal '9', then it's a digit
		cmp 	#"a"						;
		bcc 	_exit						; exit if less than 'a'
		cmp 	#"z"+1						;
		bcs 	_exit						; exit if greater than 'z'
	_space: 								;
		lda 	#' '						;
		jsr 	LCLWrite					; write space to buffer
	_exit:
		rts

;;
; Convert uppercase letter to lowercase
;
; Converts an uppercase ASCII letter (A-Z) to its lowercase equivalent (a-z).
; Non-alphabetic characters are passed through unchanged. Used for case-
; insensitive character comparisons and lowercase output formatting.
;
; \in A             Character to convert (any ASCII value 0-255).
; \out A            Lowercase version if input was A-Z, otherwise unchanged.
; \sideeffects      - Modifies only the A register.
;                   - Uses carry flag for comparison operations.
; \see              LCLCheckSpaceRequired
;;
LCLLowerCase:
		cmp 	#"A"						; compare with capital 'A'
		bcc 	_exit						; exit if less than 'A'
		cmp 	#"Z"+1						; compare with capital 'Z'+1
		bcs 	_exit						; exit if greater than 'Z'
		adc 	#$20						; add 32 to convert A-Z to a-z
	_exit:
		rts

;;
; Convert 16-bit number to decimal string and write to token buffer
;
; Converts a 16-bit unsigned integer (0-65535) to its decimal string
; representation and writes it to the token buffer. Leading zeros are
; suppressed except for the units digit. Uses repeated subtraction
; algorithm with a powers-of-10 table.
;
; \in XA            16-bit number to convert (X=high byte, A=low byte).
; \out tokenBuffer  Updated with decimal digits of the number.
; \out tbOffset     Incremented by the number of digits written.
; \sideeffects      - Modifies registers A, X, Y.
;                   - Uses zTemp0 and zTemp0+1 as temporary storage.
;                   - Calls LCLWrite for each digit output.
; \see              LCLWrite, _out_digit, _pow10_table
;;
LCLWriteNumberXA:
		stz 	zTemp0+1 					; index into digit table
	_loop1:
		stz 	zTemp0 						; subtraction count
	_loop2:
		pha 								; save initial LSB
		sec
		ldy 	zTemp0+1 					; position in table
		sbc 	_pow10_table,y
		pha
		txa
		sbc 	_pow10_table+1,y
		bcc 	_underflow
		;
		inc 	zTemp0 						; subtracted one without borrow
		tax 								; update X
		pla 								; restore A
		ply 								; throw original
		bra 	_loop2 						; try again.
	_underflow:
		ldy 	zTemp0 						; count of subtractions
		bne 	_out
		lda 	tbOffset 					; suppress leading zeroes
		dec 	a
		beq 	_next
	_out:
		tya
		jsr 	_out_digit
	_next:
		ply 							 	; restore original value.
		pla
		ldy 	zTemp0+1					; bump the index
		iny
		iny
		sty 	zTemp0+1
		cpy 	#8							; done all 4
		bne 	_loop1
	_out_digit:
		ora 	#'0'
		jsr 	LCLWrite
		rts

	_pow10_table:
		.word 	10000
		.word 	1000
		.word 	100
		.word 	10

;;
; Default color values for syntax highlighting
;;
OptionsDefaultColors .dstruct OptionsColors, CONBrown, CONYellow, CONRed, CONOrange, CONCyan, CONYellow, CONPink, CONWhite

		.send code
