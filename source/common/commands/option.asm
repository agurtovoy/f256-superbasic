;;
; [option] statement implementation.
;;

		.section code

;;
; Handle the [option] statement.
;
; Interprets the [option] statement that sets a configuration value
; at a specified address within the options storage area. The command expects
; two 8-bit integer arguments: an address (0-255) and a value (0-255).
;
; Syntax: `OPTION <address>, <value>`
; Example: `OPTION 5, 128    ; Sets option slot 5 to value 128`
;
; \in Y                 Current parsing position in the statement.
; \out Y                Updated parsing position after consuming both arguments.
; \out OptionsStorage   Updated with new value at specified address.
; \sideeffects         - Modifies registers `A`, `X` and `Y`.
;                      - Updates `OptionsStorage` array.
; \see                 Evaluate8BitInteger, CheckComma, OptionsStorage
;;
OptionCommand: ;; [option]
		ldx 	#0 							; bottom of stack level
		jsr		Evaluate8BitInteger 		; parse address (0-255)
		pha									; save address on stack
		jsr 	CheckComma					; ensure comma separator present
		inx									; increment stack level
		jsr		Evaluate8BitInteger 		; parse value (0-255)
		;
		plx									; restore address into X
		sta 	OptionsStorage,x			; store value at OptionsStorage[address]
		rts

		.send code
