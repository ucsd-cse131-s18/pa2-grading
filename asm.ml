open Printf

type reg =
  | EAX
  | ESP
  | EBP

type arg =
  | Const of int
  | Reg of reg
  | RegOffset of int * reg

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | IShr of arg * arg
  | IShl of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  | ILabel of string
  | IPush of arg
  | IPop of arg
  | ICall of string
  | IRet
  | ICmp of arg * arg
  | IJne of string
  | IJe of string
  | IJmp of string
  | IJno of string
  | IJo of string

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let r_to_asm (r : reg) : string =
  match r with
  | EAX -> "eax"
  | ESP -> "esp"
  | EBP -> "ebp"

let arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%d" n
  | Reg(r) -> r_to_asm r
  | RegOffset(n, r) ->
    if n >= 0 then
      sprintf "[%s+%d]" (r_to_asm r) n
    else
      sprintf "[%s-%d]" (r_to_asm r) (-1 * n)

let i_to_asm (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
    sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd(dest, to_add) ->
    sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | ISub(dest, to_sub) ->
    sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
  | IMul(dest, to_mul) ->
    sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
  | ICmp(left, right) ->
    sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
  | ILabel(name) ->
    sprintf "%s:" name
  | IJne(label) ->
    sprintf "  jne %s" label
  | IJmp(label) ->
    sprintf "  jmp %s" label
  | IRet ->
    "       ret"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is
