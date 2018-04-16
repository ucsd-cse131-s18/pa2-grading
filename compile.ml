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
                      IPush(Reg(EAX));
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

let rec well_formed_e (e : expr) (env : (string * int) list) : string list =
  match e with
  | ENumber(_)
  | EBool(_) -> []
  | EId(x) ->
    begin match find env x with
    | None -> ["Variable identifier "^ x ^ " unbounded "]
    | Some(_) -> []
    end
  | EPrim1(_, e) ->
    well_formed_e e env
  | EPrim2(_, left, right) ->
    (well_formed_e left env)@(well_formed_e right env)
  | EIf(cond, thn, els) ->
    (well_formed_e cond env)@(well_formed_e thn env)@(well_formed_e els env)
  | ELet(binds, body) ->
    (* Assume the parser will never give an empty binding list *)
      match binds with
      | [] ->
        failwith ("A let expression must contain one or more bindings, " ^
                  "(this shouldn't be happening)")
      | _  ->
        let rec extBinds rbs nameAcc errAcc envAcc =
          (match rbs with
           | [] -> (errAcc, envAcc)
           | (name, value)::t ->
             if List.mem name nameAcc then
               extBinds t nameAcc
                 (errAcc@["Multiple bindings for variable identifier " ^ name]@
                  (well_formed_e value env)) envAcc
             else
               extBinds t (name::nameAcc)
                 (errAcc@(well_formed_e value envAcc)) ((name,0)::envAcc))
        in
        let (errAcc', envAcc') = (extBinds binds [] [] env) in
        errAcc'@(well_formed_e body envAcc')

let check (e : expr) : string list =
  match well_formed_e e [("input", -1)] with
  | [] -> []
  | errs -> failwith (String.concat "\n" errs)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list)
  : instruction list =
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
  | EIf(ifexpr,thenexpr,elseexpr) ->
    let test_bool =
      [ IMov(stackloc si, Reg(EAX));
        IAnd(Reg(EAX), Const(1));
        ICmp(Reg(EAX), Const(0));
        IJne(error_non_bool);
        IMov(Reg(EAX), stackloc si);] in
    let ifexpr = compile_expr ifexpr si env in
    let thenexpr = compile_expr thenexpr si env in
    let elseexpr = compile_expr elseexpr si env in
    let else_lbl = gen_temp "else" in
    let end_lbl = gen_temp "end_if" in
    ifexpr @
    test_bool @
    [ ICmp(Reg(EAX), true_const);
      IJne(else_lbl);] @
    thenexpr @
    [ IJmp(end_lbl);
      ILabel(else_lbl);] @
    elseexpr @
    [ILabel(end_lbl)]
  | ENumber(n) -> [IMov(Reg(EAX),Const((n * 2) lor 1))]
  | EBool(b) ->
    let b = if b then true_const else false_const in
    [IMov(Reg(EAX),b)]
  | EId(nm) ->
    match find env nm with
    | Some(sloc) -> [IMov(Reg(EAX),(stackloc sloc))]
    | None ->
      let err_str = Printf.sprintf "Unbound variable identifier %s" nm in
      failwith err_str

and compile_prim1 op e si env =
  let prelude = compile_expr e si env in
  let instrs = match op with
    | Add1 ->
      IMov(stackloc si, Reg(EAX))::
      check_num @
      [IMov(Reg(EAX), stackloc si);
       IAdd(Reg(EAX),Const(2));
       check_overflow]
    | Sub1 ->
      IMov(stackloc si, Reg(EAX))::
      check_num @
      [IMov(Reg(EAX), stackloc si);
       ISub(Reg(EAX),Const(2));
       check_overflow]
    | IsNum ->
      [IAnd(Reg(EAX), Const(1));
       IShl(Reg(EAX), Const(31));
       IOr(Reg(EAX), false_const);]
    | IsBool ->
      [IXor(Reg(EAX), Const(1));
       IShl(Reg(EAX), Const(31));
       IOr(Reg(EAX), false_const);]
  in
  prelude @ instrs

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
       ISar(Reg(EAX), Const(1));
       IMul(Reg(EAX), stackloc (si + 1));
       check_overflow;
       IAdd(Reg(EAX), Const(1));],true
    | Less ->
      let less = gen_temp "less" in
      let end_lbl = gen_temp "end" in
      [ICmp(Reg(EAX),(stackloc si));
       IJg(less);
       IMov(Reg(EAX), false_const);
       IJmp(end_lbl);
       ILabel(less);
       IMov(Reg(EAX), true_const);
       ILabel(end_lbl);],true
    | Greater ->
      let greater = gen_temp "greater" in
      let end_lbl = gen_temp "end" in
      [ICmp(Reg(EAX),(stackloc si));
       IJl(greater);
       IMov(Reg(EAX), false_const);
       IJmp(end_lbl);
       ILabel(greater);
       IMov(Reg(EAX), true_const);
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
      IJe(error_non_int);
      IJmp(error_non_bool);
      ILabel(valid);] @
     instrs)

let compile_to_string prog =
  let _ = check prog in
  let prelude = "  section .text\n" ^
                "  extern error\n" ^
                "  global our_code_starts_here\n" ^
                "our_code_starts_here:\n" in
  let postlude = [IRet]
                 @ [ILabel("overflow_check")] @ (throw_err 3)
                 @ [ILabel(error_non_int)] @ (throw_err 1)
                 @ [ILabel(error_non_bool)] @ (throw_err 2) in
  let compiled = (compile_expr prog 1 [("input", -1)]) in
  let as_assembly_string = (to_asm (compiled @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string
