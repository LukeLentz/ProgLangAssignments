let t1a = valid_pic sword  = true

let t2a = dims_pic sword = (16, 16)

let t3a = string_of_pxl D = "."
let t3b = string_of_pxl H = "#"

let t4a = string_of_row [D;D;D;H;D] = "...#.\n"

let t5a = string_of_pic [[D;D;D];[D;H;D];[D;D;D]] = "...\n.#.\n...\n"
