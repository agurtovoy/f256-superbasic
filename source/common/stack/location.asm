; ************************************************************************************************
; ************************************************************************************************
;
;		Name:		location.asm
;		Purpose:	Store and retrieve the location from the TOS
;		Created:	1st October 2022
;		Reviewed: 	28th November 2022
;		Author:		Paul Robson (paul@robsons.org.uk)
;
; ************************************************************************************************
; ************************************************************************************************

		.section code

; ************************************************************************************************
;
;		Save the current code position and offset (in Y) on the stack. By convention, this is
;		stored in the first 5 bytes above the stack frame marker.
;
; ************************************************************************************************

;;
; Save current code position and offset on the stack.
;
; Stores the current execution position (code pointer and Y register offset)
; in the first 5 bytes above the current stack frame marker. This is used
; by control structures and function calls to preserve the return location.
;
; \in Y         Current offset within the code line
; \sideeffects  - Saves 4-byte code pointer (safePtr) to stack at offsets 1-4
;               - Saves Y register offset to stack at offset 5
;               - Preserves Y register across the operation
; \see          STKLoadCodePosition, safePtr, basicStack
;;

STKSaveCodePosition:
		phy
		tya 								; save Y
		ldy 	#5
		sta 	(basicStack),y
		dey 								; save Code Pointer
_STKSaveLoop:
		lda 	safePtr-1,y 				; allows us to access the pointer w/out issues.
		sta 	(basicStack),y
		dey
		bne 	_STKSaveLoop
		ply
		rts

; ************************************************************************************************
;
;							Load TOS into current code positions
;
; ************************************************************************************************

STKLoadCodePosition:
		ldy 	#1 							; load code pointer back
_STKLoadLoop:
		lda 	(basicStack),y
		sta 	safePtr-1,y
		iny
		cpy 	#5
		bne 	_STKLoadLoop
		lda 	(basicStack),y 				; get Y offset
		tay
		.cresync
		rts

		.send code

; ************************************************************************************************
;
;									Changes and Updates
;
; ************************************************************************************************
;
;		Date			Notes
;		==== 			=====
;
; ************************************************************************************************
