let t1a = getnth (3, ["hi"; "there"; "you"]) = "you"
let t1b = try (getnth (3, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1c = try (getnth (0, ["hi"; "there"; "you"]); false) with
			| Failure "getnth" -> true
			| _ -> false
let t1e = try (getnth (0, []); false) with
			| Failure "getnth" -> true
			| _ -> false

let t2a = lookup ("you", []) = None
let t2b = lookup ("you", [("him", 2); ("you", 3)]) = Some 3
let t2c = lookup ("you", [("you", 2); ("you", 4)]) = Some 2
let t2d = lookup ("", [("a", 1); ("", 2)]) = Some 2

let t3a = inPairs [1; 2; 3; 4; 5] = [(1, 2); (3, 4)]
let t3b = inPairs [] = []
let t3c = inPairs [1] = []

let t4a = flatten [[1; 2; 3]; []; [4; 5]; [6]] = [1; 2; 3; 4; 5; 6]
let t4b = flatten [] = []
let t4c = flatten [[1;2;3]] = [1;2;3]

let t5a = remove (3, [3; 4; 3; 1]) = [4; 1]
let t5b = remove (1, [2;3;4]) = [2;3;4]
let t5c = remove (1, [1;1]) = []
let t5d = remove (2, []) = []

let t6a = removeDups [4; 1; 2; 1; 4; 5; 20] = [4; 1; 2; 5; 20]
let t6b = removeDups [1;2;3;4] = [1;2;3;4]
let t6c = removeDups [] = []
let t6d = removeDups [1] = [1]
let t6e = removeDups [1;2;1;3;4;1] = [1;2;3;4]

let t7a = collateSome [Some 1; None; Some 2; Some 1; None; Some 3] = [1; 2; 1; 3]

let t8a = unzip2 [(1, 2); (3, 4); (5, 6)] = ([1; 3; 5], [2; 4; 6])
let t8b = unzip2 [(1, 1)] = ([1], [1])
let t8c = unzip2 [] = ([], [])

let t9a = makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
let t9b = makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
let t9c = makeChange (20, [13; 11]) = None
