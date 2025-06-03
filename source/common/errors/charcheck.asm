;;
; Character checking utility functions.
;;

		.section code

;;
; Macro to generate character checking functions.
;
; Creates a function that checks if the next character matches the specified
; token and advances the parser position. Jumps to syntax error if no match.
;
; \param \1    The expected character/token constant (e.g., `KWD_COMMA`)
;;
checknext .macro
		.cget 								; get next character and skip it
		iny
		cmp 	#\1 						; exit if matches
		bne 	CNAFail
		rts
		.endm

;;
; Check for right parenthesis ')' character.
;
; Validates that the next character in the statement is a right parenthesis
; and advances the parser position past it.
;
; \in Y        Current parsing position in the statement.
; \out Y       Updated parsing position after consuming the ')' character.
; \sideeffects Jumps to `SyntaxError` if ')' is not found.
;;
CheckRightBracket:
		.checknext KWD_RPAREN

;;
; Check for comma ',' character.
;
; Validates that the next character in the statement is a comma and
; advances the parser position past it.
;
; \in Y        Current parsing position in the statement.
; \out Y       Updated parsing position after consuming the ',' character.
; \sideeffects Jumps to `SyntaxError` if ',' is not found.
;;
CheckComma:
		.checknext KWD_COMMA

;;
; Check for specific character in register A.
;
; Validates that the next character in the statement matches the character
; currently in register A. Advances the parser position if successful.
;
; \in A        Expected character to match.
; \in Y        Current parsing position in the statement.
; \out Y       Updated parsing position after consuming the matched character.
; \sideeffects Jumps to `SyntaxError` if character doesn't match.
;;
CheckNextA:
		.ccmp								; match ?
		bne 	CNAFail
		iny 								; skip character
		rts 								; and exit

;;
; Common failure handler for character checking functions.
;
; Called when an expected character is not found at the current parsing
; position. Transfers control to the main syntax error handler.
;
; \sideeffects Jumps to `SyntaxError` and does not return.
;;
CNAFail:
		jmp 	SyntaxError

		.send 	code
