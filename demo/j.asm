	.text
	.global	_main
_main:
MARLO=0
MARHI=0
[:sp]=$ff
[:sp+1]=$ff
; ALLOCREG - gpr10
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr10 sp_stash 
; ALLOCREG - gpr9
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr9 gpr10 sp_stash 
; ALLOCREG - gpr8
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr8 gpr9 gpr10 sp_stash 
; ALLOCREG - gpr7
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ALLOCREG - gpr6
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr7
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr7+0] = $9a	
	[:gpr7+1] = $02	
	[:gpr7+2] = $00	
	[:gpr7+3] = $00	
; COMPARE START ========
	; ORIGINAL ASM: 		cmp.i	gpr7,666
	; BRANCH-TYPE-WILL-BE beq
	REGA=[:gpr7+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:noreg+3] _S
	REGA=[:gpr7+2]
	NOOP = REGA A_MINUS_B           [:noreg+2] _EQ_S
	REGA=[:gpr7+1]
	NOOP = REGA A_MINUS_B           [:noreg+1] _EQ_S
	REGA=[:gpr7+0]
	NOOP = REGA A_MINUS_B           [:noreg+0] _EQ_S
	; aggregate flags into register
	; NOT NEEDED REGA=0
	; NOT NEEDED REGA = REGA A_OR_B 1 _LT
	; NOT NEEDED REGA = REGA A_OR_B 2 _GT
	; NOT NEEDED REGA = REGA A_OR_B 4 _NE
	; NOT NEEDED REGA = REGA A_OR_B 8 _EQ
; BRANCH BLOCK eq
	PCHI = <:l4
	PCLO = >:l4 _EQ
; BRANCH TO LABEL l4
l3:
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr0
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr0+0] = $01	
	[:gpr0+1] = $00	
	[:gpr0+2] = $00	
	[:gpr0+3] = $00	
; CALL INLINE ASM : halt(..)
	HALT = [:gpr0]

; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
l4:
; COMPARE START ========
	; ORIGINAL ASM: 		cmp.i	gpr7,666
	; BRANCH-TYPE-WILL-BE bne
	REGA=[:gpr7+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:noreg+3] _S
	REGA=[:gpr7+2]
	NOOP = REGA A_MINUS_B           [:noreg+2] _EQ_S
	REGA=[:gpr7+1]
	NOOP = REGA A_MINUS_B           [:noreg+1] _EQ_S
	REGA=[:gpr7+0]
	NOOP = REGA A_MINUS_B           [:noreg+0] _EQ_S
	; aggregate flags into register
	; NOT NEEDED REGA=0
	; NOT NEEDED REGA = REGA A_OR_B 1 _LT
	; NOT NEEDED REGA = REGA A_OR_B 2 _GT
	; NOT NEEDED REGA = REGA A_OR_B 4 _NE
	; NOT NEEDED REGA = REGA A_OR_B 8 _EQ
; BRANCH BLOCK ne
	PCHI = <:l6
	PCLO = >:l6 _NE
; BRANCH TO LABEL l6
l5:
; ASSIGN type:i srcreg:noreg -> destreg:gpr7
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr7+0] = $e7	
	[:gpr7+1] = $03	
	[:gpr7+2] = $00	
	[:gpr7+3] = $00	
; BRA l7
	PCHI = <:l7
	PCLO = >:l7
l6:
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr0
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr0+0] = $02	
	[:gpr0+1] = $00	
	[:gpr0+2] = $00	
	[:gpr0+3] = $00	
; CALL INLINE ASM : halt(..)
	HALT = [:gpr0]

; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
l7:
; COMPARE START ========
	; ORIGINAL ASM: 		cmp.i	gpr7,999
	; BRANCH-TYPE-WILL-BE beq
	REGA=[:gpr7+3]
	NOOP = REGA A_MINUS_B_SIGNEDMAG [:noreg+3] _S
	REGA=[:gpr7+2]
	NOOP = REGA A_MINUS_B           [:noreg+2] _EQ_S
	REGA=[:gpr7+1]
	NOOP = REGA A_MINUS_B           [:noreg+1] _EQ_S
	REGA=[:gpr7+0]
	NOOP = REGA A_MINUS_B           [:noreg+0] _EQ_S
	; aggregate flags into register
	; NOT NEEDED REGA=0
	; NOT NEEDED REGA = REGA A_OR_B 1 _LT
	; NOT NEEDED REGA = REGA A_OR_B 2 _GT
	; NOT NEEDED REGA = REGA A_OR_B 4 _NE
	; NOT NEEDED REGA = REGA A_OR_B 8 _EQ
; BRANCH BLOCK eq
	PCHI = <:l9
	PCLO = >:l9 _EQ
; BRANCH TO LABEL l9
l8:
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr0
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr0+0] = $03	
	[:gpr0+1] = $00	
	[:gpr0+2] = $00	
	[:gpr0+3] = $00	
; CALL INLINE ASM : halt(..)
	HALT = [:gpr0]

; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
l9:
; ASSIGN type:s srcreg:noreg -> destreg:gpr9
	; load_reg targ from KONST - reg size:4    src data size:2
	[:gpr9+0] = $af	
	[:gpr9+1] = $be	
	[:gpr9+2] = $ff ; padding
	[:gpr9+3] = $ff ; padding
; ASSIGN type:l srcreg:noreg -> destreg:gpr6
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr6+0] = $fe	
	[:gpr6+1] = $fe	
	[:gpr6+2] = $00	
	[:gpr6+3] = $00	
; ASSIGN type:uc srcreg:noreg -> destreg:gpr8
	; load_reg targ from KONST - reg size:4    src data size:1
	[:gpr8+0] = $a1	
	[:gpr8+1] = $00 ; padding
	[:gpr8+2] = $00 ; padding
	[:gpr8+3] = $00 ; padding
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ADD gpr0 = gpr8 - 1
	REGA = 0 _S ; clear flags
	REGA = [:gpr8+0]
	[:gpr0+0] = REGA A_PLUS_B_PLUS_C $01 _S
	REGA = [:gpr8+1]
	[:gpr0+1] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr8+2]
	[:gpr0+2] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr8+3]
	[:gpr0+3] = REGA A_PLUS_B_PLUS_C $00 _S
; ASSIGN type:l srcreg:noreg -> destreg:gpr0
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr0+0] = $b2	
	[:gpr0+1] = $00	
	[:gpr0+2] = $00	
	[:gpr0+3] = $00	
; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; COMPARE START ========
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
	; NOT NEEDED REGA=0
	; NOT NEEDED REGA = REGA A_OR_B 1 _LT
	; NOT NEEDED REGA = REGA A_OR_B 2 _GT
	; NOT NEEDED REGA = REGA A_OR_B 4 _NE
	; NOT NEEDED REGA = REGA A_OR_B 8 _EQ
; BRANCH BLOCK ne
	PCHI = <:l11
	PCLO = >:l11 _NE
; BRANCH TO LABEL l11
l10:
; ASSIGN type:l srcreg:noreg -> destreg:gpr6
	; load_reg targ from KONST - reg size:4    src data size:4
	[:gpr6+0] = $aa	
	[:gpr6+1] = $00	
	[:gpr6+2] = $00	
	[:gpr6+3] = $00	
; BRA l12
	PCHI = <:l12
	PCLO = >:l12
l11:
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; ADD gpr6 = gpr6 - 187
	REGA = 0 _S ; clear flags
	REGA = [:gpr6+0]
	[:gpr6+0] = REGA A_PLUS_B_PLUS_C $bb _S
	REGA = [:gpr6+1]
	[:gpr6+1] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr6+2]
	[:gpr6+2] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr6+3]
	[:gpr6+3] = REGA A_PLUS_B_PLUS_C $00 _S
; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
l12:
; PUSH KONST 11
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $0b
; PUSH KONST 22
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $16
; PUSH KONST 33
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $21
; PUSH KONST 44
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $2c
; PUSH KONST 55
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $37
; PUSH KONST 66
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $42
; CALL sub(..) and return to l13
	; call	_sub
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = (<:l13)
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = (>:l13)
	PCHITMP = (>:_sub)
	PC      = (<:_sub)
	; return location l13
l13:
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; GETRETURN
	; memCopy : move 4 bytes  gpr10 <- gpr0  
	REGA = [:gpr0+0]
	[:gpr10+0] = REGA
	REGA = [:gpr0+1]
	[:gpr10+1] = REGA
	REGA = [:gpr0+2]
	[:gpr10+2] = REGA
	REGA = [:gpr0+3]
	[:gpr10+3] = REGA
; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 sp_stash 
; SETRETURN - zreg = gpr0
	; load_reg targ from REG - reg size:4    src data size:4
	; memCopy : move 4 bytes  gpr0 <- gpr6  
	REGA = [:gpr6+0]
	[:gpr0+0] = REGA
	REGA = [:gpr6+1]
	[:gpr0+1] = REGA
	REGA = [:gpr6+2]
	[:gpr0+2] = REGA
	REGA = [:gpr6+3]
	[:gpr0+3] = REGA
l1:
; FUNCTION BOTTOM
	; adjust SP to rewind over the variable stack by offset 20
	MARLO = MARLO + (> 20)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 20) ; add hi byte of offset plus any carry
	; return
	PCHITMP = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	REGA = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	PCHI = REGA
	ret
# stacksize=0+??
	.global	_sub
_sub:
; ALLOCREG - gpr14
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr14 sp_stash 
; ALLOCREG - gpr13
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr13
	; load_reg targ from VAR - reg size:4    src data size:4
	; stash SP
	[:SP_STASH]   = MARLO
	[:SP_STASH+1] = MARHI
	; adjust SP by offset 36
	MARLO = MARLO + (> 36)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 36) ; add hi byte of offset plus any carry
	; copy 4 bytes
	[:gpr13+3] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr13+2] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr13+1] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr13+0] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	; restore SP
	MARLO = [:SP_STASH]
	MARHI = [:SP_STASH+1]
; ALLOCREG - gpr12
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr12
	; load_reg targ from VAR - reg size:4    src data size:4
	; stash SP
	[:SP_STASH]   = MARLO
	[:SP_STASH+1] = MARHI
	; adjust SP by offset 32
	MARLO = MARLO + (> 32)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 32) ; add hi byte of offset plus any carry
	; copy 4 bytes
	[:gpr12+3] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr12+2] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr12+1] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr12+0] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	; restore SP
	MARLO = [:SP_STASH]
	MARHI = [:SP_STASH+1]
; ALLOCREG - gpr11
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr11
	; load_reg targ from VAR - reg size:4    src data size:4
	; stash SP
	[:SP_STASH]   = MARLO
	[:SP_STASH+1] = MARHI
	; adjust SP by offset 28
	MARLO = MARLO + (> 28)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 28) ; add hi byte of offset plus any carry
	; copy 4 bytes
	[:gpr11+3] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr11+2] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr11+1] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr11+0] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	; restore SP
	MARLO = [:SP_STASH]
	MARHI = [:SP_STASH+1]
; ALLOCREG - gpr10
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr10
	; load_reg targ from VAR - reg size:4    src data size:4
	; stash SP
	[:SP_STASH]   = MARLO
	[:SP_STASH+1] = MARHI
	; adjust SP by offset 20
	MARLO = MARLO + (> 20)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 20) ; add hi byte of offset plus any carry
	; copy 4 bytes
	[:gpr10+3] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr10+2] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr10+1] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr10+0] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	; restore SP
	MARLO = [:SP_STASH]
	MARHI = [:SP_STASH+1]
; ALLOCREG - gpr9
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr9
	; load_reg targ from VAR - reg size:4    src data size:4
	; stash SP
	[:SP_STASH]   = MARLO
	[:SP_STASH+1] = MARHI
	; adjust SP by offset 16
	MARLO = MARLO + (> 16)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 16) ; add hi byte of offset plus any carry
	; copy 4 bytes
	[:gpr9+3] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr9+2] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr9+1] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr9+0] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	; restore SP
	MARLO = [:SP_STASH]
	MARHI = [:SP_STASH+1]
; ALLOCREG - gpr8
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ALLOCREG - gpr7
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr7 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:noreg -> destreg:gpr7
	; load_reg targ from VAR - reg size:4    src data size:4
	; stash SP
	[:SP_STASH]   = MARLO
	[:SP_STASH+1] = MARHI
	; adjust SP by offset 24
	MARLO = MARLO + (> 24)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 24) ; add hi byte of offset plus any carry
	; copy 4 bytes
	[:gpr7+3] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr7+2] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr7+1] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	[:gpr7+0] = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	; restore SP
	MARLO = [:SP_STASH]
	MARHI = [:SP_STASH+1]
; ALLOCREG - gpr6
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:gpr9 -> destreg:gpr6
	; load_reg targ from REG - reg size:4    src data size:4
	; memCopy : move 4 bytes  gpr6 <- gpr9  
	REGA = [:gpr9+0]
	[:gpr6+0] = REGA
	REGA = [:gpr9+1]
	[:gpr6+1] = REGA
	REGA = [:gpr9+2]
	[:gpr6+2] = REGA
	REGA = [:gpr9+3]
	[:gpr6+3] = REGA
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ADD gpr0 = gpr10 - gpr7
	REGA = 0 _S ; clear flags
	REGA = [:gpr10+0]
	[:gpr0+0] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr10+1]
	[:gpr0+1] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr10+2]
	[:gpr0+2] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr10+3]
	[:gpr0+3] = REGA A_PLUS_B_PLUS_C $00 _S
; ADD gpr0 = gpr0 - gpr11
	REGA = 0 _S ; clear flags
	REGA = [:gpr0+0]
	[:gpr0+0] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+1]
	[:gpr0+1] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+2]
	[:gpr0+2] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+3]
	[:gpr0+3] = REGA A_PLUS_B_PLUS_C $00 _S
; ADD gpr0 = gpr0 - gpr12
	REGA = 0 _S ; clear flags
	REGA = [:gpr0+0]
	[:gpr0+0] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+1]
	[:gpr0+1] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+2]
	[:gpr0+2] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+3]
	[:gpr0+3] = REGA A_PLUS_B_PLUS_C $00 _S
; ADD gpr8 = gpr0 - gpr13
	REGA = 0 _S ; clear flags
	REGA = [:gpr0+0]
	[:gpr8+0] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+1]
	[:gpr8+1] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+2]
	[:gpr8+2] = REGA A_PLUS_B_PLUS_C $00 _S
	REGA = [:gpr0+3]
	[:gpr8+3] = REGA A_PLUS_B_PLUS_C $00 _S
; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; ASSIGN type:i srcreg:gpr6 -> destreg:gpr14
	; load_reg targ from REG - reg size:4    src data size:4
	; memCopy : move 4 bytes  gpr14 <- gpr6  
	REGA = [:gpr6+0]
	[:gpr14+0] = REGA
	REGA = [:gpr6+1]
	[:gpr14+1] = REGA
	REGA = [:gpr6+2]
	[:gpr14+2] = REGA
	REGA = [:gpr6+3]
	[:gpr14+3] = REGA
; PUSH REG gpr6
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr6+4]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr6+3]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr6+2]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr6+1]
; PUSH REG gpr8
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr8+4]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr8+3]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr8+2]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr8+1]
; PUSH REG gpr7
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr7+4]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr7+3]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr7+2]
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = [gpr7+1]
; PUSH KONST 5
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $05
; PUSH KONST 6
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $06
; PUSH KONST 7
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $00
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = $07
; CALL sub(..) and return to l16
	; call	_sub
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = (<:l16)
	MARLO = MARLO - 1 _S
	MARHI = MARHI A_MINUS_B_MINUS_C 0
	RAM = (>:l16)
	PCHITMP = (>:_sub)
	PC      = (<:_sub)
	; return location l16
l16:
; ALLOCREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr0 gpr6 gpr7 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; GETRETURN
	; targ gpr0 and src gpr0 are same - so nothing to do
; FREEREG - gpr0
	; allocated: gtmp1 gtmp2 ftmp1 ftmp2 gpr6 gpr7 gpr8 gpr9 gpr10 gpr11 gpr12 gpr13 gpr14 sp_stash 
; SETRETURN - zreg = gpr0
	; load_reg targ from REG - reg size:4    src data size:4
	; memCopy : move 4 bytes  gpr0 <- gpr6  
	REGA = [:gpr6+0]
	[:gpr0+0] = REGA
	REGA = [:gpr6+1]
	[:gpr0+1] = REGA
	REGA = [:gpr6+2]
	[:gpr0+2] = REGA
	REGA = [:gpr6+3]
	[:gpr0+3] = REGA
l14:
; FUNCTION BOTTOM
	; adjust SP to rewind over the variable stack by offset 12
	MARLO = MARLO + (> 12)               ; add lo byte of offset
	MARHI = MARHI A_PLUS_B_PLUS_C (< 12) ; add hi byte of offset plus any carry
	; return
	PCHITMP = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	REGA = RAM
	MARLO = MARLO + 1 _S
	MARHI = MARHI A_PLUS_B_PLUS_C 0
	PCHI = REGA
	ret
# stacksize=0+??
	;JL gen_var_head
	.globl	_static1
	.data
_static1:
	;JL gen_dc
	dc.i	111
	;JL gen_var_head
	.globl	_static2
_static2:
	;JL gen_dc
	dc.i	222
	noreg :	BYTES [0]
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
	fpr0  :	BYTES [0,0,0,0,0,0,0,0]
	fpr1  :	BYTES [0,0,0,0,0,0,0,0]
	fpr2  :	BYTES [0,0,0,0,0,0,0,0]
	fpr3  :	BYTES [0,0,0,0,0,0,0,0]
	fpr4  :	BYTES [0,0,0,0,0,0,0,0]
	fpr5  :	BYTES [0,0,0,0,0,0,0,0]
	fpr6  :	BYTES [0,0,0,0,0,0,0,0]
	fpr7  :	BYTES [0,0,0,0,0,0,0,0]
	fpr8  :	BYTES [0,0,0,0,0,0,0,0]
	fpr9  :	BYTES [0,0,0,0,0,0,0,0]
	fpr10 :	BYTES [0,0,0,0,0,0,0,0]
	fpr11 :	BYTES [0,0,0,0,0,0,0,0]
	fpr12 :	BYTES [0,0,0,0,0,0,0,0]
	fpr13 :	BYTES [0,0,0,0,0,0,0,0]
	fpr14 :	BYTES [0,0,0,0,0,0,0,0]
	fpr15 :	BYTES [0,0,0,0,0,0,0,0]
	sp_stash:	BYTES [0,0,0,0]
	sp    :	BYTES [0,0,0,0]
