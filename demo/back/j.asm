	.text
	.global	_main
_main:

; ------ allocreg
; ALLOCREG - gpr6

; ------ allocreg
; ALLOCREG - gpr4

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr4
	[:gpr4+0] = #af	
	[:gpr4+1] = #be	
	[:gpr4+2] = #00	
	[:gpr4+3] = #00	

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr6
	[:gpr6+0] = #fe	
	[:gpr6+1] = #fe	
	[:gpr6+2] = #00	
	[:gpr6+3] = #00	

; ------ compare
; COMPARE START ======================================================
	; ORIGINAL ASM: 		cmp.l	gpr4,gpr6
	; BRANCH-TYPE-WILL-BE bne
	REGA=[:gpr4+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:gpr6+3] _S
	REGA=[:gpr4+2]
	NOOP = REGA A_MINUS_B           [:gpr6+2] _EQ_S
	REGA=[:gpr4+1]
	NOOP = REGA A_MINUS_B           [:gpr6+1] _EQ_S
	REGA=[:gpr4+0]
	NOOP = REGA A_MINUS_B           [:gpr6+0] _EQ_S
	; aggregate flags into register
	REGA=0
	REGA = REGA A_OR_B 1 _LT
	REGA = REGA A_OR_B 2 _GT
	REGA = REGA A_OR_B 4 _NE
	REGA = REGA A_OR_B 8 _EQ

; ------ bne
; BRANCH BLOCK ne
	PCHI = <:l4
	PCLO = >:l4 _NE
; BRANCH TO LABEL l4

; ------ label
l3:

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr4
	[:gpr4+0] = #aa	
	[:gpr4+1] = #00	
	[:gpr4+2] = #00	
	[:gpr4+3] = #00	

; ------ bra
	PCHI = <:l5
	PCLO = >:l5

; ------ label
l4:

; ------ allocreg
; ALLOCREG - gpr3

; ------ mul
OR AND SHIFT MOD 
	mullw.l	gpr4,gpr6,187

; ------ freereg
; FREEREG - gpr3

; ------ label
l5:

; ------ set-return
; SETRETURN - zreg = gpr3
	[:gpr3+0] = #63	
	[:gpr3+1] = #00	
	[:gpr3+2] = #00	
	[:gpr3+3] = #00	

; ------ label
l1:
# stacksize=0+??
