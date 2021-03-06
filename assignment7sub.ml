(* Programming Languages, Assignment 7 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you.

   The instructions for this assignment reside in an auxiliary file, assignment7doc.md
   You should start by reading that file.
*)
(* ---------------------------------
              HELPERS
   ---------------------------------

   Place your "helpers" implementations here.
*)
let rec range a b = if a > b then [] else a :: range (a + 1) b
let range1 a = range 1 a
let tabulate f n = f range1 n



(* ---------------------------------
              PICTURES
   ---------------------------------

   Place our Pictures implementations here after the type declarations and
   sword definition.
*)
type pixel = D | H
type row = pixel list
type pic = row list

exception IncompatibleDims

let sword = [
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;D;H;H;H;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;H;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;H;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;D;H;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H]]

(*
   You need to fix this.
*)
let doodad =[
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;H;H;H;H;H;D;D;D;D;D;D];
[D;D;D;D;H;H;H;H;H;H;H;D;D;D;D;D];
[D;D;D;H;H;H;H;H;H;H;H;H;D;D;D;D];
[D;D;D;D;H;H;H;H;H;H;H;D;D;D;D;D];
[D;D;D;D;D;H;H;H;H;H;D;D;D;D;D;D];
[D;D;D;D;D;D;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D]]

(*
   These two functions provided to you. Study how they work before continuing!
*)
let valid_pic pic =
   match List.map List.length pic with
   | [] -> true
   | x :: xs -> List.for_all ((=) x) xs

let dims_pic pic =
   match pic with
   | [] -> (0, 0)
   | row :: _ -> (List.length pic, List.length row)

let string_of_pxl pxl =
   match pxl with
   | D -> "."
   | H -> "#"

let string_of_row r =
   (List.fold_left (^) "" (List.map (string_of_pxl) r)) ^ "\n"

let string_of_pic p =
   List.fold_left (^) "" (List.map (string_of_row) p)

let flip_vertical p =
   List.rev p

let flip_horizontal p =
   List.map (List.rev) p

let flip_both p =
   flip_vertical (flip_horizontal p)

let mirror_vertical p =
   p @ (flip_vertical p)

let mirror_horizontal p =
    List.map (fun l -> l @ (List.rev l)) p

let mirror_both p =
   mirror_vertical (mirror_horizontal p)

let stack_vertical p1 p2 =
    let (x, y) = dims_pic p1 and (m, n) = dims_pic p2 in
    if y <> n then raise (Failure "IncompatibleDims") else p1 @ p2

let stack_horizontal p1 p2 =
    let (x, y) = dims_pic p1 and (m, n) = dims_pic p2 in
    if x <> m
    then raise (Failure "IncompatibleDims")
    else List.map2 (fun a b -> a @ b) p1 p2

let invert p =
    List.map (List.map (fun pxl -> (match pxl with
                                                    | D -> H
                                                    | H -> D))) p


let rec transpose_r p =
    match p with
    | []                 -> []
    | []   :: xss      -> transpose_r xss
    | (x::xs) :: xss ->
        (x :: List.map List.hd xss) :: transpose_r (xs :: List.map List.tl xss)
