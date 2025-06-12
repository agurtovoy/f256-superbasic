;;
; `tab` modifier for [print]/[input] statements
;;

		.section code

TabUnary: ;; [tab]
		jsr 	CheckLeftBracket
		plx 								; restore stack pos
		jsr 	TabModifierImpl
		jsr 	CheckRightBracket
		rts

TabModifierImpl:
		jsr		Evaluate8BitInteger         ; parse row into `A`
		cmp		EXTScreenWidth				; check if column is within valid range
		bcs		_range_error

		sta     EXTColumn					; save column into `EXTColumn`
		rts

	_range_error:
		jmp 	RangeError 					; branch to range error handler

		.send code

