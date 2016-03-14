open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3

let t0e = evaluate (BoolC true) = Bool true

let t0f = let env1 = bind "x" (Bool false) empty
          in interp env1 (BoolC true) = Bool true

let t0g = desugar (BoolS true) = BoolC true

let t0h = evaluate (desugar (BoolS true)) = Bool true 

let t0i = evaluate (IfC (BoolC true, NumC 1.0, NumC 2.0)) = Num 1.0

let t0j = evaluate (IfC (BoolC false, NumC 1.0, NumC 2.0)) = Num 2.0

let t0k = desugar (IfS (BoolS false, NumS 1.0, NumS 2.0)) = IfC (BoolC false, NumC 1.0, NumC 2.0)
