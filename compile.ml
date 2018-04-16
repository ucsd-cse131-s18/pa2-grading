open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let stackloc si = RegOffset(-4 * si, ESP)
let throw_err code = failwith "TODO: throw_err"
let check_overflow = IJo("overflow_check")
let error_non_int = "error_non_int"
let error_non_bool = "error_non_bool"

let check_num = failwith "TODO: check_num"

let rec check (e : expr) : string list = failwith "TODO: check"

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | ELet(binds, body) ->
    let fold_fn (nenv,si,instrs) (nm,expr) =
      let _ = match find nenv nm with
        | Some _ -> failwith "Duplicate binding"
        | None -> () in
      let newbinding = (nm, si) in
      let expr_instrs = compile_expr expr si (nenv @ env) in
      let bind_instr = IMov((stackloc si), Reg(EAX)) in
      let si = si + 1 in
      (newbinding::nenv,si,instrs@expr_instrs@[bind_instr]) in
    let nenv,si,prelude = List.fold_left fold_fn ([],si,[]) binds in
    let postlude = compile_expr body si (nenv @ env) in
    prelude @ postlude
  | EPrim1(op, e) -> compile_prim1 op e si env
  | EPrim2(op, e1, e2) -> compile_prim2 op e1 e2 si env
  | ENumber(n) -> [IMov(Reg(EAX),Const(n))]
  | EId(nm) ->
    match find env nm with
    | Some(sloc) -> [IMov(Reg(EAX),(stackloc sloc))]
    | None ->
      let err_str = Printf.sprintf "Unbound variable identifier %s" nm in
      failwith err_str

and compile_prim1 op e si env =
  let prelude = compile_expr e si env in
  let instr = match op with
    | Add1 -> IAdd(Reg(EAX),Const(1))
    | Sub1 -> ISub(Reg(EAX),Const(1)) in
  prelude @ [instr]

and compile_prim2 op e1 e2 si env =
  let second_op = compile_expr e2 si env in
  let first_op = compile_expr e1 (si + 1) env in
  let instr = match op with
    | Plus -> IAdd(Reg(EAX),(stackloc si))
    | Minus -> ISub(Reg(EAX),(stackloc si))
    | Times -> IMul(Reg(EAX),(stackloc si)) in
  second_op @ [IMov((stackloc si),Reg(EAX))] @ first_op @ [instr]

let compile_to_string prog =
  let static_errors = check prog in
  let stackjump = 0 in
  let prelude = "section .text
extern error
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, " ^ (string_of_int stackjump) ^ "\n" in
  let postlude = [
    IMov(Reg(ESP), Reg(EBP));
    IPop(Reg(EBP));
    IRet;
    ILabel("overflow_check")
  ]
  @ (throw_err 3)
  @ [ILabel(error_non_int)] @ (throw_err 1)
  @ [ILabel(error_non_bool)] @ (throw_err 2) in
  let compiled = (compile_expr prog 1 []) in
  let as_assembly_string = (to_asm (compiled @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string
