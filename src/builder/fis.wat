(func $sign (param $p0 f64) (result i32)
	(i32.trunc_f64_s (f64.copysign (f64.const 1.0) (local.get $p0)))
)

;; # Identity Operator for Floats
(func $fis (param $p0 f64) (param $p1 f64) (result i32)
	(i32.and
		(f64.eq (local.get $p0) (local.get $p1))
		(i32.eq
			(call $sign (local.get $p0))
			(call $sign (local.get $p1))
		)
	)
)

;; # Identity Operator for Int/Float
;; Returns false, as an Int is never identical to a Float
(func $i_f_is (param $p0 i32) (param $p1 f64) (result i32)
	(i32.const 0)
)

;; # Identity Operator for Float/Int
;; Returns false, as a Float is never identical to an Int
(func $f_i_is (param $p0 f64) (param $p1 i32) (result i32)
	(i32.const 0)
)
