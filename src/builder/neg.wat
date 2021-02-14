;; # Mathematical Negation Operator for Integers
;; ```
;; function neg(x) {
;; 	return (x XOR -1) + 1
;; }
;; ```
(func $neg (param $p0 i32) (result i32)
	(i32.add
		(i32.xor
			(local.get $p0)
			(i32.const -1)
		)
		(i32.const 1)
	)
)
