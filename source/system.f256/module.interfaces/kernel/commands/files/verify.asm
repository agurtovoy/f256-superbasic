;;
; [verify] statement implementation
;;

		.section code

;;
; Verify a program file against the current program in memory.
;
; Reads a program file from disk and compares it line-by-line with the
; currently loaded program in memory. Each line is tokenized and compared with
; the corresponding tokenized line in memory to ensure they match exactly.
; Reports an error if any differences are found.
;
; \sideeffects  - Opens and closes file stream for reading
;               - Tokenizes lines from file using temporary buffer
;               - May generate verify error if files don't match
;               - Returns to warm start on successful completion
; \see          EvaluateString, TKTokenizeLine, LoadReadLine, CLComplete
;;
Command_VERIFY: ;; [VERIFY]
		jsr 	EvaluateString 				; file name to verify

		ldx 	zTemp0+1					; zTemp0 -> XA
		lda 	zTemp0
		jsr 	KNLOpenFileRead 			; open file for reading
		bcs 	_CVErrorHandler 			; error, so fail.
		sta 	BasicFileStream 			; save the reading stream.
		jsr 	LoadReadByteInit			; Init reader with the stream
		stz 	LoadEOFFlag 				; clear EOF Flag.
		.cresetcodepointer 					; prepare to loop through code.

_CVLoop:
		jsr 	LoadReadLine 				; get next line.
		beq 	_CVExit 					; end, exit.

		jsr 	TKTokenizeLine 				; tokenize the line.

		lda 	tokenLineNumber 			; line number = 0
		ora 	tokenLineNumber+1
		beq 	_CVLoop 					; not legal code, blank line or maybe a comment.

		ldy 	#0 							; start compare
_CVCompareLoop:
		.cget 								; tokenized code
		cmp 	tokenOffset,y 				; compare against actual code.
		bne 	_CVCompareError
		iny
		cpy 	tokenOffset 				; until done whole line of code
		bne 	_CVCompareLoop

		.cnextline 							; go to next line.
		bra 	_CVLoop

_CVExit:
		lda 	BasicFileStream
		jsr 	KNLCloseFile
		jsr 	CLComplete
		jmp		WarmStart

_CVCompareError:
		jsr		ResetTokenBuffer
		.error_verify

_CVErrorHandler:
		jmp 	CLErrorHandler

		.send code
