;; # Exponent Operator for Integers
;; ```
;; x ^ y
;; 	:=     (x^2) ^ (y / 2)       if y is even
;; 	:= x * (x^2) ^ ((y - 1) / 2) if y is odd
;; ```
;; ```
;; function exp(base, exponent) {
;; 	return (
;; 		(exponent <   0) ? 0 :
;; 		(exponent === 0) ? 1 :
;; 		(exponent === 1) ? base :
;; 		(exponent === 2) ? base * base :
;; 		(base === 0) ? 0 :
;; 		(base === 1) ? 1 :
;; 		(exponent % 2 === 0)
;; 			?        exp(base * base,  exponent      / 2)
;; 			: base * exp(base * base, (exponent - 1) / 2)
;; 	)
;; }
;; ```
(func $exp (param $p0 i32) (param $p1 i32) (result i32)
	(if (result i32) (i32.lt_s (local.get $p1) (i32.const 0)) ;; if $p1 < 0
		(then (i32.const 0)) ;; return 0
	(else (if (result i32) (i32.eqz (local.get $p1)) ;; else if $p1 === 0
		(then (i32.const 1)) ;; return 1
	(else (if (result i32) (i32.eq (local.get $p1) (i32.const 1)) ;; else if $p1 === 1
		(then (local.get $p0)) ;; return $p0
	(else (if (result i32) (i32.eq (local.get $p1) (i32.const 2)) ;; else if $p1 === 2
		(then (i32.mul (local.get $p0) (local.get $p0))) ;; return $p0 * $p0
	(else (if (result i32) (i32.eqz (local.get $p0)) ;; else if $p0 === 0
		(then (i32.const 0)) ;; return 0
	(else (if (result i32) (i32.eq (local.get $p0) (i32.const 1)) ;; else if $p0 === 1
		(then (i32.const 1)) ;; return 1
	(else (if (result i32) (i32.ctz (local.get $p1)) ;; else if $p1 % 2 === 0
		(then (call $exp ;; return $exp($p0 * $p0, $p1 / 2)
			(i32.mul (local.get $p0) (local.get $p0))
			(i32.div_s (local.get $p1) (i32.const 2))
		))
	(else ;; else (assert $p1 % 2 === 1)
		(i32.mul ;; return $p0 * $exp($p0 * $p0, ($p1 - 1) / 2)
			(local.get $p0)
			(call $exp
				(i32.mul (local.get $p0) (local.get $p0))
				(i32.div_s (i32.sub (local.get $p1) (i32.const 1)) (i32.const 2))
			)
		)
	)) )) )) )) )) )) ))
)
