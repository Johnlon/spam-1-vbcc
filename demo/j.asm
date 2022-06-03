	.text
	.global	_main
_main:
; ALLOCREG - gpr8
; ALLOCREG - gpr7
; ALLOCREG - gpr6
; ASSIGN type:s srcreg:noreg destreg:gpr7
; load_reg targ reg size:4    src data size:2
	[:gpr7+0] = $af	
	[:gpr7+1] = $be	
	[:gpr7+2] = $ff ; padding
	[:gpr7+3] = $ff ; padding
; save_result stored in gpr7
; save_result ISREG
; save_result : targ ISREG gpr7 and srcReg is same as targReg so nothing to do
; ASSIGN type:l srcreg:noreg destreg:gpr6
; load_reg targ reg size:4    src data size:4
	[:gpr6+0] = $fe	
	[:gpr6+1] = $fe	
	[:gpr6+2] = $00	
	[:gpr6+3] = $00	
; save_result stored in gpr6
; save_result ISREG
; save_result : targ ISREG gpr6 and srcReg is same as targReg so nothing to do
; COMPARE START ======================================================
	; ORIGINAL ASM: 		cmp.l	gpr6,gpr6
	; BRANCH-TYPE-WILL-BE bne
	REGA=[:gpr6+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:gpr6+3] _S
	REGA=[:gpr6+2]
	NOOP = REGA A_MINUS_B           [:gpr6+2] _EQ_S
	REGA=[:gpr6+1]
	NOOP = REGA A_MINUS_B           [:gpr6+1] _EQ_S
	REGA=[:gpr6+0]
	NOOP = REGA A_MINUS_B           [:gpr6+0] _EQ_S
	; aggregate flags into register
	REGA=0
	REGA = REGA A_OR_B 1 _LT
	REGA = REGA A_OR_B 2 _GT
	REGA = REGA A_OR_B 4 _NE
	REGA = REGA A_OR_B 8 _EQ
; BRANCH BLOCK ne
	PCHI = <:l4
	PCLO = >:l4 _NE
; BRANCH TO LABEL l4
l3:
; ASSIGN type:l srcreg:noreg destreg:gpr6
; load_reg targ reg size:4    src data size:4
	[:gpr6+0] = $aa	
	[:gpr6+1] = $00	
	[:gpr6+2] = $00	
	[:gpr6+3] = $00	
; save_result stored in gpr6
; save_result ISREG
; save_result : targ ISREG gpr6 and srcReg is same as targReg so nothing to do
	PCHI = <:l5
	PCLO = >:l5
l4:
; ALLOCREG - gpr0
; OR AND SHIFT MOD 
	; ORIG add.l	gpr6,gpr6,187
; save_result stored in gpr6
; save_result ISREG
; save_result : targ ISREG gpr6 and srcReg is same as targReg so nothing to do
; FREEREG - gpr0
l5:
; PUSH = P
	; ORIGINAL mov.i	0(sp),11
; PUSH = P
	; ORIGINAL mov.i	4(sp),22
; PUSH = P
	; ORIGINAL mov.i	8(sp),33
; PUSH = P
	; ORIGINAL mov.i	12(sp),44
; PUSH = P
	; ORIGINAL mov.i	16(sp),55
; PUSH = P
	; ORIGINAL mov.i	20(sp),66
; CALL
	; call	_sub
; ALLOCREG - gpr0
; GETRETURN
	REGA = [:gpr0]
	[:gpr8] = REGA
; FREEREG - gpr0
; SETRETURN - zreg = gpr0
; load_reg targ reg size:4    src data size:4
	[:gpr0+0] = $00	
	[:gpr0+1] = $00	
	[:gpr0+2] = $00	
	[:gpr0+3] = $00	
l1:
# stacksize=0+??
	.global	_sub
_sub:
; ALLOCREG - gpr14
; ALLOCREG - gpr13
; ASSIGN type:i srcreg:noreg destreg:gpr13
; load_reg targ reg size:4    src data size:4
	[:gpr13+0] = $00	
	[:gpr13+1] = $00	
	[:gpr13+2] = $00	
	[:gpr13+3] = $00	
; save_result stored in gpr13
; save_result ISREG
; save_result : targ ISREG gpr13 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr12
; ASSIGN type:i srcreg:noreg destreg:gpr12
; load_reg targ reg size:4    src data size:4
	[:gpr12+0] = $00	
	[:gpr12+1] = $00	
	[:gpr12+2] = $00	
	[:gpr12+3] = $00	
; save_result stored in gpr12
; save_result ISREG
; save_result : targ ISREG gpr12 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr11
; ASSIGN type:i srcreg:noreg destreg:gpr11
; load_reg targ reg size:4    src data size:4
	[:gpr11+0] = $00	
	[:gpr11+1] = $00	
	[:gpr11+2] = $00	
	[:gpr11+3] = $00	
; save_result stored in gpr11
; save_result ISREG
; save_result : targ ISREG gpr11 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr10
; ASSIGN type:i srcreg:noreg destreg:gpr10
; load_reg targ reg size:4    src data size:4
	[:gpr10+0] = $00	
	[:gpr10+1] = $00	
	[:gpr10+2] = $00	
	[:gpr10+3] = $00	
; save_result stored in gpr10
; save_result ISREG
; save_result : targ ISREG gpr10 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr9
; ASSIGN type:i srcreg:noreg destreg:gpr9
; load_reg targ reg size:4    src data size:4
	[:gpr9+0] = $00	
	[:gpr9+1] = $00	
	[:gpr9+2] = $00	
	[:gpr9+3] = $00	
; save_result stored in gpr9
; save_result ISREG
; save_result : targ ISREG gpr9 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr8
; ALLOCREG - gpr7
; ASSIGN type:i srcreg:noreg destreg:gpr7
; load_reg targ reg size:4    src data size:4
	[:gpr7+0] = $00	
	[:gpr7+1] = $00	
	[:gpr7+2] = $00	
	[:gpr7+3] = $00	
; save_result stored in gpr7
; save_result ISREG
; save_result : targ ISREG gpr7 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr6
; ASSIGN type:i srcreg:gpr9 destreg:gpr6
; load_reg targ reg size:4    src data size:4
	[:gpr6+0] = $00	
	[:gpr6+1] = $00	
	[:gpr6+2] = $00	
	[:gpr6+3] = $00	
; save_result stored in gpr6
; save_result ISREG
; save_result : targ ISREG gpr6 and srcReg is same as targReg so nothing to do
; ALLOCREG - gpr0
; OR AND SHIFT MOD 
	; ORIG add.i	gpr0,gpr10,gpr7
; save_result stored in gpr0
; save_result ISREG
; save_result : targ ISREG gpr0 and srcReg is same as targReg so nothing to do
; OR AND SHIFT MOD 
	; ORIG add.i	gpr0,gpr0,gpr11
; save_result stored in gpr0
; save_result ISREG
; save_result : targ ISREG gpr0 and srcReg is same as targReg so nothing to do
; OR AND SHIFT MOD 
	; ORIG add.i	gpr0,gpr0,gpr12
; save_result stored in gpr0
; save_result ISREG
; save_result : targ ISREG gpr0 and srcReg is same as targReg so nothing to do
; OR AND SHIFT MOD 
	; ORIG add.i	gpr8,gpr0,gpr13
; save_result stored in gpr8
; save_result ISREG
; save_result : targ ISREG gpr8 and srcReg is same as targReg so nothing to do
; FREEREG - gpr0
; ASSIGN type:i srcreg:gpr6 destreg:gpr14
; load_reg targ reg size:4    src data size:4
	[:gpr14+0] = $00	
	[:gpr14+1] = $00	
	[:gpr14+2] = $00	
	[:gpr14+3] = $00	
; save_result stored in gpr14
; save_result ISREG
; save_result : targ ISREG gpr14 and srcReg is same as targReg so nothing to do
; PUSH = P
	; ORIGINAL mov.i	0(sp),gpr6
; PUSH = P
	; ORIGINAL mov.i	4(sp),gpr8
; PUSH = P
	; ORIGINAL mov.i	8(sp),gpr7
; PUSH = P
	; ORIGINAL mov.i	12(sp),3
; PUSH = P
	; ORIGINAL mov.i	16(sp),3
; PUSH = P
	; ORIGINAL mov.i	20(sp),3
; CALL
	; call	_sub
; ALLOCREG - gpr0
; GETRETURN
	REGA = [:gpr0]
	[:gpr0] = REGA
; FREEREG - gpr0
; PUSH = P
	; ORIGINAL mov.i	0(sp),gpr6
; PUSH = P
	; ORIGINAL mov.i	4(sp),gpr8
; PUSH = P
	; ORIGINAL mov.i	8(sp),gpr7
; PUSH = P
	; ORIGINAL mov.i	12(sp),3
; PUSH = P
	; ORIGINAL mov.i	16(sp),3
; PUSH = P
	; ORIGINAL mov.i	20(sp),3
; CALL
	; call	_sub
; ALLOCREG - gpr0
; GETRETURN
	REGA = [:gpr0]
	[:gpr0] = REGA
; FREEREG - gpr0
; SETRETURN - zreg = gpr0
; load_reg targ reg size:4    src data size:4
	[:gpr0+0] = $00	
	[:gpr0+1] = $00	
	[:gpr0+2] = $00	
	[:gpr0+3] = $00	
l6:
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
