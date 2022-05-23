[ _bar xp
	r0
	r1
	r2
	r3
	r4
	r5
;
	@sub xp 0+2 -> xp;
	@loadw _statOut 0+0 -> sp;
	@pull r0;
.L1;
	@ret r0;
]
;
[ __init_vars_;
];
