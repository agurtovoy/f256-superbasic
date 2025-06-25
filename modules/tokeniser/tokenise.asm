;;
; Line tokenizer
;
; Tokenized lines are stored in the following format:
;
;	- A one-byte offset to the next line ($00 means end of program)
;	- A two-byte low/high line number
;	- The tokenized line
;	- EOL token
;;

		.section code

;;
; Tokenize ASCIIZ line in the line buffer.
;
; Converts a source line from ASCII text format into tokenized format for
; faster interpretation. The tokenized line is stored in `tokenBuffer`
; starting at `tokenOffset`. Handles line numbers, keywords, identifiers,
; strings, hex constants, and punctuation tokens.
;
; \in lineBuffer        ASCIIZ string containing the input line to tokenize.
; \out tokenBuffer      Tokenized representation of the input line.
; \out tokenOffset      Updated to point after the tokenized line.
; \out tokenLineNumber  Set to extracted line number (if present), otherwise 0.
; \sideeffects          - Modifies registers `A`, `X`, `Y`.
;                       - Resets token buffer to empty (3 bytes for line number & offset).
;                       - May create variable records for new identifiers.
; \see                  TOKExtractLineNumber, TOKIdentifier, TOKTokenString,
;                       TOKHexConstant,,TOKSearchTable, TOKCheckCreateVariableRecord
;;
Export_TKTokenizeLine:
		;
		;		Reset the token buffer and line number
		;
		lda 	#global.FIRST_TOKEN_OFFSET
		sta 	tokenOffset 				; (offset for the line number etc.)
		stz 	tokenLineNumber				; reset token line number
		stz 	tokenLineNumber+1
		;
		;		Find the first non-space character
		;
		ldx 	#-1							; Keep the current char's offset in X
											; (pre-increments to 0 below)
	_find_first:
		inx
		lda 	lineBuffer,x				; next character, exit if zero EOL
		beq 	_exit
		cmp 	#' '
		bcc 	_find_first					; keep looping if space found
		;
		;		If the current char is 0-9, we're editing a program line,
		;		extract a 2 byte integer into the token line number
		;
		cmp 	#'0'
		bcc 	_no_line_number
		cmp 	#'9'+1
		bcs 	_no_line_number
		jsr 	TOKExtractLineNumber

	_no_line_number:

		; Main tokenizing loop

	_tokenize_loop:
		lda 	lineBuffer,x 				; next character, exit if zero EOL.
		beq 	_exit
		inx
		cmp 	#' '
		beq 	_tokenize_loop  			; keep looping if space found.
		dex 								; undo last get, A contains character, X is position.

		; See https://en.wikipedia.org/wiki/ASCII#Character_set

		bcc 	_nonprintable				; a non-printable control char

		cmp 	#'0'						; check for digits
		bcc 	_punctuation
		cmp 	#'9'+1
		bcc 	_decimal

		cmp 	#'A'						; check for [A-Z_]
		bcc 	_punctuation
		cmp 	#'Z'+1
		bcc 	_identifier
		cmp 	#'_'
		beq 	_identifier

		; lower-case characters or punctuation
		cmp 	#'a'						; check for [a-z]
		bcc 	_punctuation
		cmp 	#'z'+1
		bcs 	_punctuation

		; lower-case character found, convert to upper-case
		eor 	#$20						; convert to upper-case
		bra		_identifier

		; If we got here, we have a punctuation character, a DEL, or an
		; extended ASCII char. Special cases are:
		;
		;    - punctuation char codes > KWC_LAST_PUNCTUATION
		;    - `<` or `>` followed by `=`, `>`, or `<` (double punctuation)
		;    - `"` for string literals
		;    - `$` for hexadecimal constants
		;    - `.` followed by 0 to 9 for floating point constants
		;
		; See `PunctuationToken.generate_all` in `tokens.py`.

	_punctuation:
		cmp 	#'"'						; string literal
		beq 	_string
		cmp 	#'$'						; hexadecimal constant (`$` only
		beq 	_hex_const					; appears at end of identifiers)
		cmp 	#'<' 						; check for double punctuation
		beq 	_check_double_punct
		cmp 	#'>'
		beq 	_check_double_punct
		cmp 	#'.'
		beq 	_check_decimal

	_single_punct:
		lda 	lineBuffer,x 				; load the punctuation token back into `A`
		cmp 	#KWC_LAST_PUNCTUATION + 1 	; are we outside of the punctuation tokens range?
		bcc 	_save_punct

		; If we got here, we have an extended punctuation character (ASCII 64-126),
		; a DEL (ASCII 127), or an extended ASCII character (ASCII 128-255).

		cmp 	#127 						; check if it's a DEL
		beq 	_nonprintable 				; translate DEL to a non-printable token
		bcs     _extended_ascii 			; 128 or higher is extended ASCII

		; Extended punctuation characters are mapped to token IDs 16-31,
		; see `tokens.py`/`kwdconst.inc`.

		pha 								; save A on the stack
		and 	#7 							; lower 3 bits in zTemp0
		sta 	zTemp0
		pla
		and 	#32 						; bit 5
		lsr 	a 							; shift into bit 3
		lsr 	a
		ora 	zTemp0
		ora 	#$10						; now in the range 16-31

	_save_punct:
		jsr 	TOKWriteByte 				; write the punctuation character
		inx 								; consume the character

		cmp 	#KWD_QUOTE 					; check for single quote (') comment
		bne 	_tokenize_loop  			;
		jsr 	TOKTokenComment 			; tokenize comment text
		bra 	_tokenize_loop

		; exit point, writes EOL and returns
	_exit:
		lda 	#KWC_EOL 					; write end of line byte
		jsr 	TOKWriteByte
		rts

	_identifier:
		jsr     TOKIdentifier				; tokenize an identifier or keyword
		bra 	_tokenize_loop

	_decimal:								; tokenize a decimal constant, e.g. 42 or 3.14
		jsr 	TOKTokenDecimal
		bra 	_tokenize_loop

	_string:								; tokenize a string "Hello world"
		jsr 	TOKTokenString
		bra 	_tokenize_loop

	_hex_const:								; tokenize hex constant #A277
		jsr 	TOKHexConstant
		jmp 	_tokenize_loop

	_nonprintable:
		._write_ext_char_token KWC_NONPRINTABLE
		jmp 	_tokenize_loop

		; We have `<` or `>`, check if the following character is `<`, `=`, or `>`.
		; The double punctuation is mapped to IDs $08-$0f, see `tokens.py`/`kwdconst.inc`.

	_check_double_punct:
		lda 	lineBuffer+1,x 				; get next character
		cmp 	#'<'						; if not <, =, or >, which are consecutive,
		bcc 	_single_punct 				; go back to the single punctuation handler
		cmp 	#'>'+1
		bcs 	_single_punct
		;
		lda 	lineBuffer,x 				; this is < (60) or > (62)
		and 	#2 							; now < (0) or > (2)
		asl 	a 							; now < (0) or > (4), CC also
		adc 	lineBuffer+1,x 				; add < = > codes - < code
		sec
		sbc 	#('<' - KWC_FIRST_PUNCTUATION)
		jsr 	TOKWriteByte 				; this is in the range KWC_FIRST_PUNCTUATION + 0-7
		inx 								; consume both chars
		inx
		jmp 	_tokenize_loop

	_check_decimal:
		lda 	lineBuffer+1,x 				; peek next character
		cmp 	#'0'						; if not 0-9, then it's a single
		bcc 	_single_punct				; punctuation, go back
		cmp 	#'9'+1
		bcs 	_single_punct
		bra 	_decimal

	_extended_ascii:
		._write_ext_char_token KWC_EXTASCII
		jmp 	_tokenize_loop

;;
; Write extended character token macro.
;
; Outputs a data token followed by the extended character byte. Used for
; non-printable control characters and extended ASCII characters that cannot
; be represented as simple punctuation tokens.
;
; \in \1        Token ID to write (`KWC_NONPRINTABLE` or `KWC_EXTASCII`)
; \in A         Character byte to write after the token
; \sideeffects  - Writes token ID and character byte to token buffer
; \see          TOKWriteByte, KWC_NONPRINTABLE, KWC_EXTASCII
;;
_write_ext_char_token .macro
		pha									; save A on the stack
		lda 	#(\1) 						; write out the corresponding token
		jsr 	TOKWriteByte 				;
		pla									; restore A
		jsr 	TOKWriteByte 				; write the extended ASCII character
		inx									; consume the character
		.endm

;;
; Tokenize identifier or keyword.
;
; Processes an identifier starting with A-Z or underscore, extracting the complete
; identifier including optional type suffixes (`#` for float, `$` for string) and
; checking if it matches any keywords in the token tables. If not found as a
; keyword, creates or updates a variable record for the identifier.
;
; \in X         Current position in `lineBuffer` (pointing at first identifier character)
; \out X        Updated position after the identifier and any type suffix
; \sideeffects  - Sets `identStart`, `identTypeStart`, `identTypeEnd`, `identTypeByte` globals
;               - Writes keyword token(s) if identifier matches a keyword
;               - Creates variable record if identifier is not a keyword
;               - May write `KWC_COMMENT` token for REM statements
; \see          TOKCalculateHash, TOKSearchTable, TOKCheckCreateVariableRecord,
;               TOKTokenComment, KeywordSet0, KeywordSet1, KeywordSet2
;;
TOKIdentifier:
		stx 	identStart 					; save identifier's start offset
		stz 	identTypeByte 				; reset identifier's type to integer

	_loop:
		inx 								; look at next, we know first is identifier already
		lda  	lineBuffer,x
		; legal chars are 0-9, A-Z, _, a-z
		cmp	 	#"0"
		bcc 	_end_identifier
		cmp 	#"9"+1
		bcc 	_loop
		cmp	 	#"A"
		bcc 	_end_identifier
		cmp 	#"Z"+1
		bcc 	_loop
		cmp 	#"_"
		beq 	_loop
		cmp	 	#"a"
		bcc 	_end_identifier
		cmp 	#"z"+1
		bcc 	_loop

	_end_identifier:
		; Look for # or $ type

		stx 	identTypeStart 				; save start of type text (if any !)
		;
		ldy 	#NSTFloat 					; check for a float identifier
		cmp 	#"#"
		beq 	_has_type_char
		ldy 	#NSTString					; check for a string identifier
		cmp 	#"$"
		bne 	_no_type_char

	_has_type_char:
		sty 	identTypeByte 				; Y has # or $, save the type
		inx 								; consume the type character
		lda 	lineBuffer,x

		; Look for array

	_no_type_char:
; 		cmp 	#"("						; is it open parenthesis (e.g. array)
; 		bne 	_TKNoArray
; 		inx 								; skip the (
; 		lda 	identTypeByte 				; set bit 2 (e.g. array) in type byte
; 		ora 	#$04
; 		sta 	identTypeByte
; _TKNoArray:
		stx 	identTypeEnd 				; save end marker, e.g. continue from here.
		jsr 	TOKCalculateHash 			; calculate the hash for those tokens

		; Search the token tables to see if this is a keyword
		; (all keywords are identifier-compliant)

		._check_tokens KeywordSet0			; check the three token tables for the keyword
		ldx 	#0
		bcs 	_found_token

		._check_tokens KeywordSet1
		ldx 	#KWC_KWDSET1 				; if not found, check for KWDSET1
		bcs 	_found_token

		._check_tokens KeywordSet2			; if not found, check for KWDSET2
		ldx 	#KWC_KWDSET2
		bcs 	_found_token

		; No keyword found, so it's a procedure or a variable declaration

		jsr 	TOKCheckCreateVariableRecord ; failed all, it's a variable,
											; create record if does not exist

		ldx 	identTypeEnd 				; X points to following byte
		bra 	_exit

		; Found a token, `X` contains the keyword set, `A` the token

	_found_token:
		pha 								; save token
		txa 								; token set in X, is there one ?
		beq 	_skip_token_set_write
		jsr 	TOKWriteByte 				; if so, write it out

	_skip_token_set_write:
		pla 								; restore and write token
		jsr 	TOKWriteByte
		cpx 	#0 							; check for REM, which is in the main token set
		bne 	_not_rem
		cmp 	#KWD_REM
		bne 	_not_rem
		ldx 	identTypeEnd 				; tokenize comment text
		jsr 	TOKTokenComment
		bra 	_exit

	_not_rem:
		ldx 	identTypeEnd 				; X points to following byte
		bra 	_exit

	_exit:
		rts

;;
; Check token tables macro.
;
; Searches the specified keyword table for the current identifier. Sets up the
; table address in Y:A and calls `TOKSearchTable` to perform the lookup.
;
; \in \1        Address of keyword table to search (e.g. `KeywordSet0`)
; \out A        Token ID if found (with carry set)
; \out C        Set if token found, clear if not found
; \see          TOKSearchTable, KeywordSet0, KeywordSet1, KeywordSet2
;;
_check_tokens .macro
		ldy 	#(\1) >> 8
		lda 	#(\1) & $FF
		jsr 	TOKSearchTable
		.endm

;;
; Tokenize comment text.
;
; Processes comment text following REM statements or single quote (') characters.
; The comment is stored as a `KWC_COMMENT` token followed by a data block containing
; all remaining text on the line up to the end-of-line marker.
;
; \in X         Current position in `lineBuffer` (pointing at start of comment text)
; \out X        Updated position at end of line
; \sideeffects  - Writes `KWC_COMMENT` token to token buffer
;               - Writes comment text data block to token buffer
;               - Advances through `lineBuffer` to end of line
; \see          TOKWriteByte, TOKWriteBlockXY, lineBuffer, tokenBuffer
;;
TOKTokenComment:
		lda 	#KWC_COMMENT 				; comment token
		jsr 	TOKWriteByte
		inx									; start of the comment text
		phx 								; save it on the stack
		dex 								; because we pre-increment
	_find_end:
		inx
		lda 	lineBuffer,x 				; next character
		bne 	_find_end

		ply  								; restore the comment start into `Y`
		jsr 	TOKWriteBlockXY 			; write chars `X` to `Y` as a data block
		rts


;;
; Tokenize string literal.
;
; Parses a string literal enclosed in quotes and stores it as a `KWC_STRING`
; token followed by a data block containing the string contents and a null
; terminator.
;
; \in X         Current position in `lineBuffer` (pointing at opening quote)
; \out X        Updated position after the string (past closing quote or end of line)
; \sideeffects  - Writes `KWC_STRING` token to token buffer
;               - Writes string data block to token buffer
;               - Advances through `lineBuffer` until closing quote or end of line
; \see          TOKWriteByte, TOKWriteBlockXY, lineBuffer, tokenBuffer
;;
TOKTokenString:
		lda 	#KWC_STRING 				; string token.
		jsr 	TOKWriteByte
		inx									; start of quoted string.
		phx 								; push start of string on top
		dex 								; because we pre-increment
	_find_end:
		inx
		lda 	lineBuffer,x 				; next character
		beq 	_end_of_string 				; no matching quote, we don't mind.
		cmp 	#'"' 						; go back if quote not found
		bne 	_find_end
		;
	_end_of_string:
		ply  								; so now Y is first character,
											; X is character after end

		pha 								; save terminating character
		jsr 	TOKWriteBlockXY 			; write X to Y as a data block
		pla 								; terminating character
		beq 	_exit						; if it wasn't EOS skip it
		inx
	_exit:
		rts

;;
; Tokenize hexadecimal constant.
;
; Parses a hexadecimal constant and converts it into a `KWC_HEXCONST` token,
; followed by a data block containing the hex digits and a null terminator.
;
; \in X         Current position in `lineBuffer` (pointing at '$' character)
; \out X        Updated position after the hex constant
; \sideeffects  - Writes `KWC_HEXCONST` token to token buffer
;               - Writes hex digit data block to token buffer
;               - Advances through `lineBuffer` consuming hex digits [0-9A-F]
; \see          TOKWriteByte, TOKWriteBlockXY, lineBuffer, tokenBuffer
;;
TOKHexConstant:
		lda 	#KWC_HEXCONST 				; hex constant token
		jsr 	TOKWriteByte
		inx									; point `X` to the first hex digit
		phx 								; push start of constant on top
		dex
	_find_loop:
		inx 	 							; this is stored in a block, so find
		lda 	lineBuffer,x 				; out how long the hex constant is
		cmp 	#"0"
		bcc 	_found
		cmp 	#"9"+1
		bcc 	_find_loop
		cmp 	#"A"
		bcc 	_found
		cmp 	#"F"+1
		bcc 	_find_loop
	_found:
		ply 								; restore start
		jsr 	TOKWriteBlockXY 			; output the block
		rts

;;
; Tokenize decimal constant.
;
; Parses a decimal number (integer or floating point) and converts it into a
; KWC_DECIMAL token followed by a data block containing the digits and decimal
; point. Handles numbers starting with a digit or decimal point (e.g., "42",
; "3.14", ".5") and ensures only one decimal point is allowed per number.
;
; \in X         Current position in `lineBuffer` (pointing at first digit or '.')
; \out X        Updated position after the decimal constant
; \sideeffects  - Writes `KWC_DECIMAL` token to token buffer
;               - Writes decimal digit data block to token buffer
;               - Uses `zTemp0` for decimal point counting
;               - Advances through `lineBuffer` consuming digits [0-9] and one '.'
; \see          TOKWriteByte, TOKWriteBlockXY, lineBuffer, tokenBuffer
;;
TOKTokenDecimal:
		pha									; save A on the stack
		lda 	#KWC_DECIMAL 				; decimal constant token
		jsr 	TOKWriteByte

		stz 	zTemp0 						; the number of decimal periods so far
		pla									; restore A
		cmp 	#"."						; check if the first character is a period
		bne 	_first_char
		inc 	zTemp0						; if so, increment the period count
		lda     #"0"						; write a zero to start the number
		jsr 	TOKWriteByte
		lda     #"."						; then write the period

	_first_char:
		jsr 	TOKWriteByte				; save the first digit or period
		phx 								; push start of constant on the stack

	_find_loop:
		inx 	 							; this is stored in a block, so find
		lda 	lineBuffer,x 				; out how long the constant is
		cmp 	#"0"
		bcc 	_found
		cmp 	#"9"+1
		bcc 	_find_loop
		cmp 	#"."
		bne 	_found

		; Found a decimal point - check if we've already seen one
		lda 	zTemp1
		bne 	_found						; already seen a decimal point, stop here
		inc 	zTemp1						; mark that we've seen a decimal point
		bra 	_find_loop

	_found:
		ply 								; restore start
		jsr 	TOKWriteBlockXY 			; output the block
		rts

;;
; Copy a block of data from the line buffer to the token buffer.
;
; Writes a block of data from the line buffer to the token buffer, including
; a length byte prefix and a null terminator. Used for string literals, hex
; constants, and other variable-length data that needs to be stored in the
; tokenized output.
;
; \in Y         Start position in `lineBuffer` (inclusive)
; \in X         End position in `lineBuffer` (exclusive)
; \sideeffects  - Writes length byte followed by data block to token buffer
;               - Writes null terminator after the data
;               - Modifies registers `A`, `X`, and `Y`
;               - Modifies `zTemp0` for calculations
; \see          TOKWriteByte, lineBuffer, tokenBuffer
;;
TOKWriteBlockXY:
		stx 	zTemp0 						; save end character
		tya 								; use 2's complement to work out the byte size
		eor 	#$FF
		sec
		adc 	zTemp0
		inc 	a 							; one extra for NULL
		jsr 	TOKWriteByte
	_loop:
		cpy 	zTemp0 						; exit if reached the end
		beq 	_exit
		lda 	lineBuffer,y 				; write byte out
		jsr 	TOKWriteByte
		iny
		bra 	_loop
	_exit:
		lda 	#0 							; add NULL
		jsr 	TOKWriteByte
		rts


		.send code
