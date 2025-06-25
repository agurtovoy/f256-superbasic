;;
; Token scanning utilities
;;

		.section code

;;
; Scan forward for matching tokens at the current block depth.
;
; Searches from the current position for one of two closing tokens,
; specified in registers `A` and `X`. Only matches tokens at the same
; nesting level as the current scope.
;
; \in A         First token to search for
; \in X         Second token to search for
; \in Y         Current position in tokenized program
; \out A        The token that was found (either original A or X value)
; \out Y        Position after the found token (backtracked if EOL match)
; \sideeffects  - Modifies registers `A`, `X`, and `Y`
;               - Modifies `zTemp0`, `zTemp0+1`, and `zTemp1`
;               - Advances through tokenized program consuming tokens
;               - May generate structure error if end of program reached
; \see          _adjust_block_depth, .cget, .cnextline, .error_struct
;;
ScanForward:
		stz 	zTemp1 						; zero the block depth - goes up with WHILE/FOR,
											; down with WEND/NEXT etc.
		stx 	zTemp0+1					; save `X` & `A` as the two possible matches in `zTemp0`
		sta 	zTemp0
		;
		; 		Main Scanning Loop
		;
	_scan_loop:
		.cget 								; get next token and consume it
		iny

		ldx 	zTemp1 						; if the count is > 0 cannot match as in sub-block
		bne 	_next
		;
		cmp 	zTemp0 						; see if either matches
		beq 	_scan_match
		cmp 	zTemp0+1
		bne 	_next

	_scan_match:							; if so, exit after skipping that token
		cmp 	#KWC_EOL 					; if asked for EOL, backtrack to point before it
		bne 	_exit
		dey

	_exit:
		rts

	_next:
		jsr  	CalcBlockDepth     			; adjust the block depth based on the current token
		bra 	_scan_loop

;;
; Adjust block depth based on the current token.
;
; Increments for block-opening tokens ([while], [for], etc.)
; and decrements for block-closing tokens ([wend], [next], etc.).
; Skips data blocks and correctly advances past multi-byte tokens.
;;
; \in A         Token to process (already consumed from program)
; \in Y         Current position in tokenized program
; \out Y        Updated position after processing token-specific data
; \sideeffects  - Modifies `zTemp1` block depth for block tracking
;               - May advance `Y` for multi-byte tokens (variables, keywords
;                 in extended sets)
;               - May skip data blocks for string/hex/decimal constants
;               - May generate structure error if end of program reached
; \see          ScanForward, .cnextline, .cskipdatablock, .error_struct
;;
CalcBlockDepth:
		cmp 	#KWC_EOL
		beq     _skip_one 					; if EOL, skip one token and exit

		cmp 	#KWC_LAST_PUNCTUATION + 1	; if punctuation characters, already done
		bcc 	_exit
		;
		cmp 	#KWC_LAST_USERDEFINED + 1	; if it's a user-defined token, skip one extra as
		bcc 	_skip_one	 				; these are 2-byte offsets into the identifier table
											; or shifts.
		;
		cmp 	#KWC_FIRST_DATA 			; skip data tokens (consts, strings etc.)
		bcs 	_skip_data
		;
		cmp 	#KWC_FIRST_BLOCK 			; block keyword ?
		bcc 	_exit 						; if not, ordinary keywords.
		cmp 	#KWC_LAST_BLOCK+1
		bcs 	_exit
		;
		;		Structure code - can go up and down.
		;
		dec 	zTemp1 						; decrement the sructure count
		cmp 	#KWC_FIRST_BLOCKEND 		; back if it is an end of block statement (e.g. WEND/NEXT)
		bcs 	_exit
		inc 	zTemp1 						; it's a beginning of block statement (e.g. WHILE/FOR)
		inc 	zTemp1 						; twice to undo the dec
		bra 	_exit

		;
		;		+2 ; for 40-7F (Variable) 80 (New line) and 81-82 (Shifts)
		;
	_skip_one:
		iny 								; consume the extra one.
		cmp 	#KWC_EOL 					; if not EOL loop back
		bne 	_exit
		;
		.cnextline 							; go to next line
		ldy 	#3 							; scan start position.
		.cget0 								; read the offset
		bne 	_exit 						; if not zero, more to scan
		.error_struct 						; couldn't find either token at level zero end of program.
		;
		;		Skip data structure
		;
	_skip_data:
		;
		dey 								; point at data token
		.cskipdatablock 					; skip block

	_exit:
		rts


;;
; Get the nesting depth of the current line.
;
; Scans through the current line from the beginning, counting the net effect
; of block structure tokens to determine the indentation level. Block-opening
; tokens ([while], [for], etc.) increase the depth, while block-closing tokens
; ([wend], [next], etc.) decrease it. Used by the [list] command to determine
; proper indentation.
;
; \in codePtr  Pointer to the start of the current line in tokenized program
; \out A        Net block depth change for the current line
; \sideeffects  - Modifies `A` and `Y` registers
;               - Modifies `zTemp1` (block depth counter)
;               - Scans from start of current line to EOL
; \see          AdjustBlockDepth, cget
GetCurrentLineDepth:
		stz 	zTemp1
		ldy 	#global.FIRST_TOKEN_OFFSET
	_loop:
		.cget 								; next and consume ?
		iny
		cmp 	#KWC_EOL	 				; if EOL exit
		beq 	_exit
		jsr 	CalcBlockDepth
		bra 	_loop
	_exit:
		lda 	zTemp1 						; return the adjustment
		rts

		.send code
