	.text
	.global	_sub
_sub:

; ------ allocreg
; ALLOCREG - gpr7

; ------ move
; ASSIGN/PUSH
; ASSIGN i gpr7
	[:gpr7+0] = #00	
	[:gpr7+1] = #00	
	[:gpr7+2] = #00	
	[:gpr7+3] = #00	

; ------ allocreg
; ALLOCREG - gpr6

; ------ call
; CALL
	call	_externalFn

; ------ allocreg
; ALLOCREG - gpr0

; ------ get-return
; GETRETURN
	REGA = [:gpr0]
	[:gpr6] = REGA

; ------ freereg
; FREEREG - gpr0

; ------ call
; CALL
	call	_externalFn2

; ------ allocreg
; ALLOCREG - gpr0

; ------ get-return
; GETRETURN
	REGA = [:gpr0]
	[:gpr6] = REGA

; ------ freereg
; FREEREG - gpr0

; ------ set-return
; SETRETURN - zreg = gpr0
	[:gpr0+0] = #00	
	[:gpr0+1] = #00	
	[:gpr0+2] = #00	
	[:gpr0+3] = #00	

; ------ label
l1:
# stacksize=0+??
	.global	_main
_main:

; ------ allocreg
; ALLOCREG - gpr8

; ------ allocreg
; ALLOCREG - gpr7

; ------ allocreg
; ALLOCREG - gpr6

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr6
	[:gpr6+0] = #af	
	[:gpr6+1] = #be	
	[:gpr6+2] = #00	
	[:gpr6+3] = #00	

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr7
	[:gpr7+0] = #fe	
	[:gpr7+1] = #fe	
	[:gpr7+2] = #00	
	[:gpr7+3] = #00	

; ------ compare
; COMPARE START ======================================================
	; ORIGINAL ASM: 		cmp.l	gpr6,gpr7
	; BRANCH-TYPE-WILL-BE bne
	REGA=[:gpr6+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:gpr7+3] _S
	REGA=[:gpr6+2]
	NOOP = REGA A_MINUS_B           [:gpr7+2] _EQ_S
	REGA=[:gpr6+1]
	NOOP = REGA A_MINUS_B           [:gpr7+1] _EQ_S
	REGA=[:gpr6+0]
	NOOP = REGA A_MINUS_B           [:gpr7+0] _EQ_S
	; aggregate flags into register
	REGA=0
	REGA = REGA A_OR_B 1 _LT
	REGA = REGA A_OR_B 2 _GT
	REGA = REGA A_OR_B 4 _NE
	REGA = REGA A_OR_B 8 _EQ

; ------ bne
; BRANCH BLOCK ne
	PCHI = <:l6
	PCLO = >:l6 _NE
; BRANCH TO LABEL l6

; ------ label
l5:

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr6
	[:gpr6+0] = #aa	
	[:gpr6+1] = #00	
	[:gpr6+2] = #00	
	[:gpr6+3] = #00	

; ------ bra
	PCHI = <:l7
	PCLO = >:l7

; ------ label
l6:

; ------ allocreg
; ALLOCREG - gpr0

; ------ add
; OR AND SHIFT MOD 
	; ORIG add.l	gpr6,gpr7,187

; ------ freereg
; FREEREG - gpr0

; ------ label
l7:

; ------ push
; ASSIGN/PUSH
; ASSIGN/PUSH = P
	; ORIGINAL mov.i	0(sp),123

; ------ call
; CALL
	call	_sub

; ------ allocreg
; ALLOCREG - gpr0

; ------ get-return
; GETRETURN
	REGA = [:gpr0]
	[:gpr8] = REGA

; ------ freereg
; FREEREG - gpr0

; ------ set-return
; SETRETURN - zreg = gpr0
	[:gpr0+0] = #63	
	[:gpr0+1] = #00	
	[:gpr0+2] = #00	
	[:gpr0+3] = #00	

; ------ label
l3:
# stacksize=0+??

;JL gen_var_head
	.globl	_static1
	.data
	.align	2
_static1:
	dc.i	1

;JL gen_var_head
	.globl	_static2
	.align	2
_static2:
	dc.i	2

;JL gen_var_head
	.globl	_externalFn

;JL gen_var_head
	.globl	_externalFn2
	noreg :	BYTES [0,0,0,0]
	gtmp1 :	BYTES [0,0,0,0]
	gtmp2 :	BYTES [0,0,0,0]
	ftmp1 :	BYTES [0,0,0,0]
	ftmp2 :	BYTES [0,0,0,0]
	gpr0  :	BYTES [0,0,0,0]
	gpr1  :	BYTES [0,0,0,0]
	gpr2  :	BYTES [0,0,0,0]
	gpr3  :	BYTES [0,0,0,0]
	gpr4  :	BYTES [0,0,0,0]
	gpr5  :	BYTES [0,0,0,0]
	gpr6  :	BYTES [0,0,0,0]
	gpr7  :	BYTES [0,0,0,0]
	gpr8  :	BYTES [0,0,0,0]
	gpr9  :	BYTES [0,0,0,0]
	gpr10 :	BYTES [0,0,0,0]
	gpr11 :	BYTES [0,0,0,0]
	gpr12 :	BYTES [0,0,0,0]
	gpr13 :	BYTES [0,0,0,0]
	gpr14 :	BYTES [0,0,0,0]
	gpr15 :	BYTES [0,0,0,0]
	fpr0  :	BYTES [0,0,0,0]
	fpr1  :	BYTES [0,0,0,0]
	fpr2  :	BYTES [0,0,0,0]
	fpr3  :	BYTES [0,0,0,0]
	fpr4  :	BYTES [0,0,0,0]
	fpr5  :	BYTES [0,0,0,0]
	fpr6  :	BYTES [0,0,0,0]
	fpr7  :	BYTES [0,0,0,0]
	fpr8  :	BYTES [0,0,0,0]
	fpr9  :	BYTES [0,0,0,0]
	fpr10 :	BYTES [0,0,0,0]
	fpr11 :	BYTES [0,0,0,0]
	fpr12 :	BYTES [0,0,0,0]
	fpr13 :	BYTES [0,0,0,0]
	fpr14 :	BYTES [0,0,0,0]
	fpr15 :	BYTES [0,0,0,0]
	fp    :	BYTES [0,0,0,0]
	sp    :	BYTES [0,0,0,0]
