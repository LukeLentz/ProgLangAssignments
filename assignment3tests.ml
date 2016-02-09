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

let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin

let t6a = to_f (F 2.3) = 2.3

let t7a = temp_compare (F 2.3, F 4.5) = -1

let t8a = string_of_temp (C 2.3) = "2.3C"

let t9a = max_temp [F 2.1; C 2.1] = C 2.1

let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
