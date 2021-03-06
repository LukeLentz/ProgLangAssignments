{
  open Parser
  exception Eof
  exception Unrecognized
}

let any = _
let digit = ['0'-'9']
let sign = ['+' '-']
let frac = '.' digit+
let exp = ['e' 'E'] sign? digit+
let white = [' ' '\t' '\n' '\r']+ | "//" ([^ '\n' '\r'])*
let newline = '\n' | '\r' | "\r\n"
let dblsemi = ";;"
let float = (digit+ '.'? | digit* frac) exp?
let true = "true" | "#t"
let false = "false" | "#f"
let comp = ">" | ">=" | "<" | "<="

rule token = parse
  | "if"                { IF }
  | "then"           { THEN }
  | "else"         { ELSE }
  | "or"                { OR }
  | "and"               { AND }
  | "not"               { NOT }
  | true            { TRUE }
  | false          { FALSE }
  | white          { token lexbuf }
  | newline      { token lexbuf }
  | dblsemi      { DBLSEMI }
  | float as x    { FLOAT (float_of_string x) }
  | comp as s       { COMPOP s }
  | "+"                { PLUS }
  | "-"                 { MINUS }
  | "*"                 { TIMES }
  | "/"                     { DIVIDE }
  | eof         { raise Eof }
  | any         { raise Unrecognized }
  | "=="        { EQ }
  | "!="            { NEQ }
