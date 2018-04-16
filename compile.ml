open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let stackloc si = RegOffset(-4 * si, ESP)
let throw_err code = [IMov(Reg(EAX), Const(code));
                      ICall("error");]
let check_overflow = IJo("overflow_check")
let error_non_int = "error_non_int"
let error_non_bool = "error_non_bool"

let true_const = HexConst(0xFFFFFFFE)
let false_const = HexConst(0x7FFFFFFE)

let check_num = [IAnd(Reg(EAX), Const(1));
                 ICmp(Reg(EAX), Const(1));
                 IJne(error_non_int);]


(* Assume arg1 is a register *)
let check_nums arg1 arg2 =
  [IAnd(arg1, arg2);
   IAnd(arg1, Const(1));
   ICmp(arg1, Const(1));
   IJne(error_non_int);]

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
  | ENumber(n) -> [IMov(Reg(EAX),Const((n * 2) lor 1))]
  | EId(nm) ->
    match find env nm with
    | Some(sloc) -> [IMov(Reg(EAX),(stackloc sloc))]
    | None ->
      let err_str = Printf.sprintf "Unbound variable identifier %s" nm in
      failwith err_str

and compile_prim1 op e si env =
  let prelude = compile_expr e si env in
  let instr = match op with
    | Add1 -> IAdd(Reg(EAX),Const(2))
    | Sub1 -> ISub(Reg(EAX),Const(2)) in
  prelude @ [instr]

and compile_prim2 op e1 e2 si env =
  let first_op = compile_expr e1 si env in
  let second_op = compile_expr e2 (si + 1) env in
  (* first arg is in eax, second is in stackloc si + 1 *)
  let instrs,numr = match op with
    | Plus ->
      [IAnd(Reg(EAX), true_const);
       IAdd(Reg(EAX), stackloc si); check_overflow],true
    | Minus ->
      [IAnd(Reg(EAX), true_const);
       IMov(stackloc (si + 1), Reg(EAX));
       IMov(Reg(EAX), stackloc si);
       ISub(Reg(EAX), stackloc (si + 1)); check_overflow],true
    | Times ->
      [IAnd(Reg(EAX), true_const);
       IMov(stackloc (si + 1), Reg(EAX));
       IMov(Reg(EAX), stackloc si);
       IShr(Reg(EAX), Const(1));
       IMul(Reg(EAX), stackloc (si + 1));
       check_overflow;
       IAdd(Reg(EAX), Const(1));],true
    | Less ->
      let not_less = gen_temp "not_less" in
      let end_lbl = gen_temp "end" in
      [ICmp(Reg(EAX),(stackloc si));
       IJge(not_less);
       IMov(Reg(EAX), true_const);
       IJmp(end_lbl);
       ILabel(not_less);
       IMov(Reg(EAX), false_const);
       ILabel(end_lbl);],true
    | Greater ->
      let not_greater = gen_temp "not_greater" in
      let end_lbl = gen_temp "end" in
      [ICmp(Reg(EAX),(stackloc si));
       IJle(not_greater);
       IMov(Reg(EAX), true_const);
       IJmp(end_lbl);
       ILabel(not_greater);
       IMov(Reg(EAX), false_const);
       ILabel(end_lbl);],true
    | Equal ->
      let not_equal = gen_temp "not_equal" in
      let end_lbl = gen_temp "end" in
      [ICmp(Reg(EAX),(stackloc si));
       IJne(not_equal);
       IMov(Reg(EAX), true_const);
       IJmp(end_lbl);
       ILabel(not_equal);
       IMov(Reg(EAX), false_const);
       ILabel(end_lbl);],false in
  if numr then
    first_op @ [IMov((stackloc si),Reg(EAX))] @ second_op @
    (IMov(stackloc (si + 1), Reg(EAX))::
     (check_nums (Reg(EAX)) (stackloc si))) @
     (IMov(Reg(EAX), stackloc(si + 1))::instrs)
  else
    let valid = gen_temp "valid_eq" in
    first_op @ [IMov((stackloc si),Reg(EAX))] @ second_op @
    ([IMov(stackloc (si + 1), Reg(EAX));
      (* If the lowest bit after the xor is 1, the two types were different *)
      IXor(Reg(EAX), (stackloc si));
      IAnd(Reg(EAX), Const(1));
      ICmp(Reg(EAX), Const(1));
      IMov(Reg(EAX), stackloc (si + 1));
      IJne(valid);
      IAnd(Reg(EAX), Const(1));
      ICmp(Reg(EAX), Const(0));
      IJe(error_non_bool);
      IJmp(error_non_int);
      ILabel(valid);] @
      instrs)

let compile_to_string prog =
  (*let static_errors = check prog in*)
  let stackjump = 0 in
  let prelude =
    "section .text\n" ^
    "extern error\n" ^
    "extern print\n" ^
    "extern input\n" ^
    "global our_code_starts_here\n" ^
    "our_code_starts_here:\n" ^
    "push ebp\n" ^
    "mov ebp, esp\n" ^
    "sub esp, " ^ (string_of_int stackjump) ^ "\n" in
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
