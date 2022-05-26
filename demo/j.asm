	.text
	.global	_sub
_sub:

; ------ allocreg
; ALLOCREG - gpr6

; ------ move
; ASSIGN/PUSH
; ASSIGN i gpr6
	[:gpr6+0] = #00	
	[:gpr6+1] = #00	
	[:gpr6+2] = #00	
	[:gpr6+3] = #00	

; ------ allocreg
; ALLOCREG - gpr5

; ------ call
; CALL
	call	_externalFn

; ------ allocreg
; ALLOCREG - gpr3

; ------ get-return
; GETRETURN
	REGA = [:gpr3]
	[:gpr5] = REGA

; ------ freereg
; FREEREG - gpr3

; ------ call
; CALL
	call	_externalFn2

; ------ allocreg
; ALLOCREG - gpr3

; ------ get-return
; GETRETURN
	REGA = [:gpr3]
	[:gpr5] = REGA

; ------ freereg
; FREEREG - gpr3

; ------ set-return
; SETRETURN - zreg = gpr3
	[:gpr3+0] = #00	
	[:gpr3+1] = #00	
	[:gpr3+2] = #00	
	[:gpr3+3] = #00	

; ------ label
l1:
# stacksize=0+??
	.global	_main
_main:

; ------ allocreg
; ALLOCREG - gpr7

; ------ allocreg
; ALLOCREG - gpr6

; ------ allocreg
; ALLOCREG - gpr5

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr5
	[:gpr5+0] = #af	
	[:gpr5+1] = #be	
	[:gpr5+2] = #00	
	[:gpr5+3] = #00	

; ------ move
; ASSIGN/PUSH
; ASSIGN l gpr6
	[:gpr6+0] = #fe	
	[:gpr6+1] = #fe	
	[:gpr6+2] = #00	
	[:gpr6+3] = #00	

; ------ compare
; COMPARE START ======================================================
	; ORIGINAL ASM: 		cmp.l	gpr5,gpr6
	; BRANCH-TYPE-WILL-BE bne
	REGA=[:gpr5+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:gpr6+3] _S
	REGA=[:gpr5+2]
	NOOP = REGA A_MINUS_B           [:gpr6+2] _EQ_S
	REGA=[:gpr5+1]
	NOOP = REGA A_MINUS_B           [:gpr6+1] _EQ_S
	REGA=[:gpr5+0]
	NOOP = REGA A_MINUS_B           [:gpr6+0] _EQ_S
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
; ASSIGN l gpr5
	[:gpr5+0] = #aa	
	[:gpr5+1] = #00	
	[:gpr5+2] = #00	
	[:gpr5+3] = #00	

; ------ bra
	PCHI = <:l7
	PCLO = >:l7

; ------ label
l6:

; ------ allocreg
; ALLOCREG - gpr3

; ------ add
; OR AND SHIFT MOD 
	; ORIG add.l	gpr5,gpr6,187

; ------ freereg
; FREEREG - gpr3

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
; ALLOCREG - gpr3

; ------ get-return
; GETRETURN
	REGA = [:gpr3]
	[:gpr7] = REGA

; ------ freereg
; FREEREG - gpr3

; ------ set-return
; SETRETURN - zreg = gpr3
	[:gpr3+0] = #63	
	[:gpr3+1] = #00	
	[:gpr3+2] = #00	
	[:gpr3+3] = #00	

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
	(null):	BYTES [0,0,0,0]
	(null):	BYTES [0,0,0,0]
	(null):	BYTES [0,0,0,0]
	(null):	BYTES [0,0,0,0]
	(null):	BYTES [0,0,0,0]
	(null):	BYTES [0,0,0,0]
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
