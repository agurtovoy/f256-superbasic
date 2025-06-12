;;
; Macros used for accessing program code space
;
; `codePtr` always contains the address of current line, `Y` register
; contains the offset within the line.
;;

;;
; Get the current token.
;
; Returns the token at offset `Y` from `codePtr`. This is the primary method
; for reading tokens during parsing and execution.
;
; \in Y         Token offset
; \in codePtr   Current program line pointer
; \out A        Token/byte value at `(codePtr),Y`
; \see          codePtr, .cget0, .cset, .ccmp
;;
cget 	.macro
		lda 	(codePtr),y
		.endm

;;
; Get the offset to the next program line.
;
; Loads the first byte at `codePtr`, which gives the offset to the next line.
; See `tokenize.asm` for details on the in-memory structure of program lines.
;
; \out A        The offset to the next program line
; \see          codePtr, .cget, .cnextline
;;
cget0 	.macro
		lda 	(codePtr)
		.endm


;;
; Set the current token.
;
; Set the token at offset `Y` from `codePtr` to `A`. Used to modify tokens or
; data during program execution.
;
; \in Y         Token offset
; \in A         Token to store
; \in codePtr   Current program line pointer
; \see          codePtr, .cget, .cget0, .ccmp
cset 	.macro
		sta 	(codePtr),y
		.endm

cset0 	.macro 								; update offset
		sta 	(codePtr)
		.endm

ccmp 	.macro 								; compare against
		cmp 	(codePtr),y
		.endm


;;
; Resync `codePtr` to the current page, if necessary.
;
; Call after `codePtr` has been updated to adjust any physical selections
; (e.g. page selection).
;;
cresync .macro
		.endm

;;
; Advance `codePtr` to the next line.
;
; Reads the offset byte at the beginning of the line and adds it to `codePtr`
; to move to the start of the next line.
;
; \in codePtr   Must point to the start of a program line
; \out codePtr  Updated to point to the next program line
; \sideeffects  - Updates `codePtr` to point to next program line
;               - Modifies `A` register
;               - Preserves `Y` register (current position within line)
; \see          codePtr, BasicStart
;;
cnextline .macro
		clc
		lda		(codePtr)					; read the first byte of the current line (the offset)
		adc		codePtr						; add the offset to `codePtr`
		sta 	codePtr						; store the new low byte of `codePtr`
		bcc 	_exit
		inc 	codePtr+1 					; if carry, increment the high byte of `codePtr`
	_exit:
		.endm


;;
; Skip over a data block in the tokenized program.
;
; Data blocks consist of a length byte followed by that many data bytes.
; This macro advances the `Y` register past the entire block by reading the
; length byte and adding it (plus 1 for the length byte itself) to the
; current position.
;
; \in Y         Current position pointing at the data block token
; \out Y        Position after the data block (pointing at next token)
; \sideeffects  - Modifies `A` and `Y` registers
; \see          .cget, KWC_STRING, KWC_HEXCONST, KWC_DECIMAL, KWC_COMMENT
;;
cskipdatablock .macro
		iny 								; point to block size
		tya 								; save the current position in A
		sec 								; set carry flag to add size + 1
		adc 	(codePtr),y					; add the block size to the current position
		tay 								; transfer the current position back to Y
		.endm


;;
; Set `codePtr` to a specific 16-bit address.
;
; Sets the `codePtr` to point to a specific memory address. Performs any
; necessary page synchronization.
;
; \in \1        16-bit address to set `codePtr` to
; \sideeffects  - Updates `codePtr` with the specified address
;               - Modifies `A` register
;               - Calls `cresync` for any required page switching
; \see          codePtr, .cresync, BasicStart
;;
csetcodepointer 	.macro
		lda 	#<\1
		sta 	codePtr
		lda 	#>\1
		sta 	codePtr+1
		.cresync
		.endm


;;
; Reset `codePtr` to the start of the program.
;
; Resets the `codePtr` to point to the beginning of the program stored at
; `BasicStart`. This is typically used when starting program execution or
; returning to the beginning after an error or command completion.
;
; \out codePtr  Contains the `BasicStart` address
; \sideeffects  - Modifies `A` register
;               - Calls `cresync` for any required page switching
; \see          .codePtr, .csetcodepointer, BasicStart
;;
cresetcodepointer 	.macro
		.csetcodepointer BasicStart
		.endm
