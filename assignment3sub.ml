(* Programming Languages, Assignment 3 *)
(*
   You should write your functions in this file.
   You should specify the types of your functions, both for arguments and for
   return values.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)

(* ----------------------------------------
            ROCK PAPER SCISSORS
   ---------------------------------------- *)

(*
   In the first set of exercises we will be describing in OCAML
   the game rock-paper-scissors. If somehow you are not familiar with the
   game, read it in wikipedia.
   We will specify some custom types for the game:
   - A "shape" is one of the possible hand shapes that a player can make.
   - A "check" is a pair of the two shapes that the two players are supposed
         to have made.
   - The "result" of a check is whether it was a tie, or whether the first
         or the second player won. For instance the result of (Rock, Paper)
         should be that the second player won.
   - A "game" is a list of "checks" that are supposed to happen in order.
   - A "valid game" is a game that could actually occur: Players only continue
         to perform "checks" if they run in a tie. For instance a valid game
         would be: [(Rock, Rock), (Rock, Paper)]
         But this is not valid: [(Rock, Rock), (Rock, Paper), (Paper, Scissors)]
         because the 3rd check should not be happening.
         Also this is not valid: [(Rock, Rock), (Paper, Paper)]
         because the game cannot end with ties.
         In other words a "valid game" would consist of a sequence of tied checks
         followed by one non-tied check, and nothing after it.
   - A game, even a non-valid one, can be "played" as follows:
      - Look at each check in the list in order. Return the first result that is
            not a Tie.
      - If all checks in the game are Ties, return a Tie.
   - A "play" is a list of shapes. They represent the intended "plays" of the player.
         For example [Rock, Rock, Paper] means that the player will play Rock on
         the first check, Rock on the second check and Paper on the third check.
*)
type shape = Rock | Paper | Scissors
type check = shape * shape
type result = Tie | FstWin | SndWin
type game = check list
type play = shape list

(*
   Write a function `result` that takes as input a check and
   returns the result of that check.
   Type: check -> result
*)
let result check =
   match check with
   | (Rock, Rock)
   | (Paper, Paper)
   | (Scissors, Scissors) -> Tie
   | (Rock, y) -> if y = Paper then SndWin else FstWin
   | (Paper, y) -> if y = Scissors then SndWin else FstWin
   | (Scissors, y) -> if y = Rock then SndWin else FstWin



(*
   Write a function `is_tie` that takes as input a check and returns
   whether the check's result is a tie.
   Type: check -> bool
*)
let is_tie check =
	result check = Tie


(*
   Write a function `game_from_plays` that takes as input two plays (correspoding
   to the intended plays of the two players) and creates a game by combining them,
   representing how they would have been played. If one play is longer than the
   other, stop at the shortest one.
   Type: play * play -> game
*)
let rec game_from_plays (plays1, plays2) : game =
	match plays1 with
	| [] -> [] (* if plays1 is finished, end the list constuctor *)
	| hd :: rest -> match plays2 with
					| [] -> [] (* same, but for plays2 *)
					| hd' :: rest' -> (hd, hd') :: game_from_plays (rest, rest') 
					(* otherwise, recursively call the fun. to create the list *)
	



(*
   Write a function `valid_game` that takes as input a game and determines if it is
   a valid game as described above.
   Type: game -> bool
*)
let rec valid_game game =
	match game with
	| [] -> true (* returns true for empty games *)
	| hd :: [] -> if result hd = Tie (* returns false if ending in a tie *)
				  then false
				  else true (* otherwise, we have gone through the entire game and can return true *)
	| hd :: rest -> if result hd != Tie (* returns false if continuing after a win *)
					then false
					else valid_game rest (* if game ties, continue validating *)


(*
   Write a function `play_game` that plays the game as described above.
   Type: game -> result
*)
let rec play_game game =
	match game with
	| [] -> Tie (* No game was played/inconclusive, so it is a tie *)
	| hd :: rest -> if result hd = Tie
					then play_game rest
				    else result hd

(* --------------------------------------
            TEMPERATURES
   -------------------------------------- *)

(*
   In this section we write functions to work flexibly with temperatures.
   A value of type "temp" is effectively a number 'tagged' with a C or F
   depending on if it is meant to be Celsius or Fahrenheit.
   The conversion between the two is the familiar formula: F = 1.8 * C + 32
*)
type temp = C of float | F of float

(*
   Write a function `to_f` that takes as input a value of type "temp" and
   returns the temperature measured in Fahrenheit.
   Note: The operators for floating point arithmetic have a dot following
   them to distinguish from the integer ones. For example "2.1 +. 5.2"
   Type: temp -> float
*)
let to_f temp =
	match temp with
	| F f -> f
	| C f -> 1.8 *. f +. 32.0

(*
   Write a function `temp_compare` that takes as input a pair of temperatures and
   "compares" them, returning 1 if the first temperature is higher, 0 if they are
   equal and -1 if the second temperature is higher.
   Type: temp * temp -> int
*)
let temp_compare (temp1, temp2) =
	(* first convert to fahrenheit *)
	let pr = (to_f temp1, to_f temp2) in
	if fst pr > snd pr
	then 1
	else if fst pr = snd pr
	then 0
	else -1


(*
   Write a function `string_of_temp` that takes as input a temperature and
   returns a string representing that temperature. For instance 23.2 Fahrenheit
   should print as "23.2F" while 23.2 Celcius as "23.2C". Look in the Pervasives
   module in the string conversions section for a function converting floats
   to strings.
   Type: temp -> string
*)
let string_of_temp temp =
	match temp with
	| C f -> (string_of_float f) ^ " C"
	| F f -> (string_of_float f) ^ " F"



(*
   Write a function `max_temp` that takes as input a list of temperatures and
   returns the largest one. It should raise an exception `Failure "max_temp"`
   if the list is empty.
   Type: temp list -> temp
*)
let rec max_temp temps =
	match temps with
	| [] -> raise (Failure "max_temp")
	| hd :: rest -> match rest with
					| [] -> hd (* if only one element, simply return it *)
					| hd' :: rest' -> if to_f hd' > to_f hd (* convert for accurate comparison *)
									  then max_temp rest (* if the second el is bigger, call max_temps on everything after first el *)
									  else max_temp (hd :: rest') (* otherwise, remove the second el and call max_temps again *)
(*
   Write a function `max_temp2` that behaves like `max_temp` but where all the
   recursive calls are tail calls. You will likely need to define an auxiliary
   function and use state recursion.
*)
(* max_temp is already like this *)