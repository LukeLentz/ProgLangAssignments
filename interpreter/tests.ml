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

let t1a = evaluate (BoolC true) = Bool true

let t1b = let env1 = bind "x" (Bool false) empty
          in interp env1 (BoolC true) = Bool true

let t1c = desugar (BoolS true) = BoolC true

let t1d = evaluate (desugar (BoolS true)) = Bool true 

let t2a = evaluate (IfC (BoolC true, NumC 1.0, NumC 2.0)) = Num 1.0

let t2b = evaluate (IfC (BoolC false, NumC 1.0, NumC 2.0)) = Num 2.0

let t2c = desugar (IfS (BoolS false, NumS 1.0, NumS 2.0)) = IfC (BoolC false, NumC 1.0, NumC 2.0)

let t3a = evaluate (desugar ((NotS (BoolS false)))) = Bool true

let t3b = evaluate (desugar ((AndS (BoolS true, BoolS true)))) = Bool true

let t3c = evaluate (desugar ((OrS (BoolS true, BoolS false)))) = Bool true

let t4a = evaluate (desugar (ArithS ("+", NumS 2.0, NumS 2.0))) = Num 4.0

let t4b = evaluate (desugar (ArithS ("-", NumS 1.0, NumS 2.0))) = Num (-1.0)

let t4c = evaluate (desugar (ArithS ("*", NumS 2.0, NumS 3.0))) = Num 6.0

let t4d = evaluate (desugar (ArithS ("*", NumS 0.0, NumS 4.0))) = Num 0.0

let t4e = evaluate (desugar (ArithS ("*", NumS 5.0, NumS (-1.0)))) = Num (-5.0)

let t4f = evaluate (desugar (ArithS ("/", NumS 6.0, NumS 2.0))) = Num 3.0

let t4g = try evaluate (ArithC ("/", NumC 3.0, NumC 0.0)) with
                        | Failure "Interp" -> Bool true
                        | _ -> Bool false

let t4h = try evaluate (ArithC ("a", NumC 3.0, NumC 1.0)) with
                    | Failure "Interp" -> Bool true
                    | _ -> Bool false

let t5a = evaluate (desugar (CompS ("<", NumS 1.0, NumS 2.0))) = Bool true

let t5b = evaluate (desugar (CompS ("<", NumS 1.0, NumS 1.0))) = Bool false

let t5c = evaluate (desugar (CompS ("<=", NumS 1.0, NumS 1.0))) = Bool true

let t5d = evaluate (desugar (CompS (">=", NumS 1.0, NumS 2.0))) = Bool false

let t5e = evaluate (desugar (CompS (">", NumS 2.0, NumS 1.0))) = Bool true

let t5f = try evaluate (desugar (CompS ("a", NumS 1.0, NumS 1.0))) with
                    | Failure "Interp" -> Bool true
                    | _ -> Bool false

let t6a = evaluate (EqC (NumC 1.0, NumC 1.0)) = Bool true

let t6b = evaluate (EqC (NumC 1.0, NumC 3.0)) = Bool false

let t6c = evaluate (desugar (NeqS (NumS 1.0, NumS 3.0))) = Bool true