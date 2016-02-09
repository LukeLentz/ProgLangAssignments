let t1a = result (Rock, Rock) = Tie
let t1b = result (Paper, Paper) = Tie
let t1c = result (Scissors, Scissors) = Tie
let t1d = result (Paper, Rock) = FstWin
let t1e = result (Rock, Paper) = SndWin
let t1f = result (Rock, Scissors) = FstWin
let t1g = result (Scissors, Rock) = SndWin
let t1h = result (Scissors, Paper) = FstWin
let t1i = result (Paper, Scissors) = SndWin

let t2a = is_tie (Rock, Paper) = false
let t2b = is_tie (Rock, Rock) = true

let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) = [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3b = game_from_plays ([Rock; Paper], [Rock; Scissors; Scissors]) = [(Rock, Rock); (Paper, Scissors)]

let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [(Rock, Rock)] = false
let t4c = valid_game [(Rock, Scissors); (Paper, Rock)] = false
let t4d = valid_game [(Paper, Paper); (Scissors, Scissors)] = false
let t4e = valid_game [(Rock, Rock); (Rock, Paper)] = true
let t4f = valid_game [] = true

let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5b = play_game [(Rock, Rock)] = Tie
let t5c = play_game [] = Tie
let t5d = play_game [(Scissors, Paper); (Paper, Paper); (Rock, Paper)] = FstWin

let t6a = to_f (F 2.3) = 2.3
let t6b = to_f (C 0.0) = 32.0
let t6c = to_f (C 100.0) = 212.0

let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7b = temp_compare (F 2.3, F 2.3) = 0
let t7c = temp_compare (F 3.0, F 2.0) = 1

let t8a = string_of_temp (C 2.3) = "2.3 C"
let t8b = string_of_temp (F 5.0) = "5. F"

let t9a = max_temp [F 2.1; C 2.1] = C 2.1
let t9b = try max_temp [] with
		  | Failure "max_temp" -> true
		  | _ -> false

let t9c = max_temp [F 4.2] = F 4.2
let t9d = max_temp [C 0; F 40; C 100] = C 100

let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
