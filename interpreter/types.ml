exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float | BoolS of bool | IfS of (exprS * exprS * exprS) | AndS of (exprS * exprS) | OrS of (exprS * exprS) | NotS of exprS | ArithS of (string * exprS * exprS) | CompS of (string * exprS * exprS) | EqS of (exprS * exprS) | NeqS of (exprS * exprS)
(* You will need to add more cases here. *)
type exprC = NumC of float | BoolC of bool | IfC of (exprC * exprC * exprC) | ArithC of (string * exprC * exprC) | CompC of (string * exprC * exprC) | EqC of (exprC * exprC)


(* You will need to add more cases here. *)
type value = Num of float | Bool of bool

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS b       -> BoolC b
  | IfS (a, b, c) -> IfC (desugar a, desugar b, desugar c)
  | AndS (a, b)   -> desugar (IfS (a, IfS (b, BoolS true, BoolS false), BoolS false))
  | OrS (a, b)  -> desugar (IfS (a, BoolS true, (IfS (b, BoolS true, BoolS false))))
  | NotS a       -> desugar (IfS (a, BoolS false, BoolS true))
  | ArithS (op, x, y)  -> ArithC (op, desugar x, desugar y)
  | CompS (op, x, y) -> CompC (op, desugar x, desugar y)
  | EqS (x, y) -> EqC (desugar x, desugar y)
  | NeqS (x, y) -> desugar (NotS (EqS (x, y)))

(* You will need to add cases here. *)

let arithEval op v1 v2 =
    match (op, v1, v2) with
    | ("+", Num v1, Num v2) -> Num (v1 +. v2)
    | ("-", Num v1, Num v2) -> Num (v1 -. v2)
    | ("*", Num v1, Num v2) -> Num (v1 *. v2)
    | ("/", Num v1, Num v2) -> if (v2 = 0.0)
                                                then raise (Failure "Interp")
                                                else Num (v1 /. v2)
    | _ -> raise (Failure "Interp")                        

let compEval op v1 v2 =
    match (op, v1, v2) with
    | ("<", Num v1, Num v2) -> Bool (v1 < v2)
    | ("<=", Num v1, Num v2) -> Bool (v1 <= v2)
    | (">=", Num v1, Num v2) -> Bool (v1 >= v2)
    | (">", Num v1, Num v2) -> Bool (v1 > v2)
    | _ -> raise (Failure "Interp")

let eqEval v1 v2 =
    match (v1, v2) with
    | (Num x, Num y) -> Bool (x = y)
    | (Bool x, Bool y) -> Bool (x = y)
    | _ -> Bool false

(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i         -> Num i
  | BoolC b        -> Bool b
  | ArithC (op, x, y) -> (match (x, y) with
                                    | (NumC v, NumC n) -> arithEval op (Num v) (Num n)
                                    | (NumC v, ArithC n) -> arithEval op (Num v) (interp [] (ArithC n))
                                    | (ArithC v, NumC n) -> arithEval op (interp [] (ArithC v)) (Num n)
                                    | (ArithC v, ArithC n) -> arithEval op (interp [] (ArithC v)) (interp [] (ArithC n))
                                    | _ -> raise (Failure "Interp") )
  | CompC (op, x, y) -> (match (x, y) with
                                      | (NumC x, NumC y) -> compEval op (Num x) (Num y)
                                      | _ -> raise (Failure "Interp"))
  | EqC (x, y) -> eqEval (interp [] x) (interp [] y)
  | IfC (a, b, c)    -> match a with
                                | BoolC a -> (match a with
                                                    | true -> interp [] b
                                                    | false -> interp []c)
                                | EqC e -> (match interp [] (EqC e) with
                                                  | Bool true -> interp [] b
                                                  | Bool false -> interp [] c)
                                | _ -> raise (Failure "Interp")


(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b
