let t1a = valid_pic sword  = true

let t2a = dims_pic sword = (16, 16)

let t3a = string_of_pxl D = "."
let t3b = string_of_pxl H = "#"

let t4a = string_of_row [D;D;D;H;D] = "...#.\n"

let t5a = string_of_pic [[D;D;D];[D;H;D];[D;D;D]] = "...\n.#.\n...\n"

let t6a = flip_vertical [[D;D;H];[H;D;D]] = [[H;D;D];[D;D;H]]

let t7a = flip_horizontal [[H;D;D];[H;D;D]] = [[D;D;H];[D;D;H]]

let t8a = flip_both [[H;D;D];[D;D;H]] = [[H;D;D];[D;D;H]]

let t9a = mirror_vertical [[D;D;H];[H;D;D]]  = [[D;D;H];[H;D;D];[H;D;D];[D;D;H]] 

let t10a = mirror_horizontal [[D;D;H]] = [[D;D;H;H;D;D]]

let t11a = mirror_both [[D;D;H];[H;H;D]] = [[D;D;H;H;D;D];[H;H;D;D;H;H];[H;H;D;D;H;H];[D;D;H;H;D;D]]

let t12a = stack_vertical [[D;D;D]] [[H;H;H]] = [[D;D;D];[H;H;H]] 
let t12b = try (stack_vertical [[D;D]] [[H]]; false) with
                                                | Failure "IncompatibleDims" -> true
                                                | _ -> false
let t13a = stack_horizontal [[D;D;D]] [[H;H;H]] = [[D;D;D;H;H;H]]
let t13b = try (stack_horizontal [[D;D;D];[D;D;D]] [[H;H]]; false) with
                                                    | Failure "IncompatibleDims" -> true
                                                    | _ -> false

let t14a = invert [D;D;H] = [H;H;D]