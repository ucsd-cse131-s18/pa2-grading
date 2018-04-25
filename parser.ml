open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr

let boa_max = int_of_float(2.**30.) - 1;;
let boa_min = -int_of_float(2.**30.);;
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^[+-]?[0-9]+"
let reserved_words = ["let"; "add1"; "sub1"; "isNum"; "isBool"; "if"]
let reserved_constants = ["true"; "false"; ]
let int_of_string_opt s =
  try Some(int_of_string s) with
  | _ -> None

let rec parse (sexp : Sexp.t) =
  match sexp with
  | List((Atom s)::t) ->
    (match s with
     | "let" ->
       (match t with
        | (List(bindings))::body::[] ->
          let binds = List.map parse_binding bindings in
          let body = parse body in
          ELet(binds, body)
        | _ -> failwith "Error: Invalid let syntax")
     | "if" ->
       (match t with
        | ifexpr::thenexpr::elseexpr::[] ->
          let ifexpr = parse ifexpr in
          let thenexpr = parse thenexpr in
          let elseexpr = parse elseexpr in
          EIf(ifexpr, thenexpr, elseexpr)
        | _ -> failwith "Error: Invalid let syntax")
     |  "add1" ->
       (match t with
        | t::[] ->
          EPrim1(Add1, (parse t))
        | _ -> failwith "Error: Invalid number of arguments to add1")
     | "sub1" ->
       (match t with
        | t::[] ->
          EPrim1(Sub1, (parse t))
        | _ -> failwith "Error: Invalid number of arguments to sub1")
     | "isNum" ->
       (match t with
        | v1::[] ->
          EPrim1(IsNum, (parse v1))
        | _ -> failwith "Error: Invalid isNum syntax")
     | "isBool" ->
       (match t with
        | v1::[] ->
          EPrim1(IsBool, (parse v1))
        | _ -> failwith "Error: Invalid isNum syntax")
     |  "+" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Plus, (parse v1), (parse v2))
        | _ -> failwith "Error: Invalid + syntax")
     | "-" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Minus, (parse v1), (parse v2))
        | _ -> failwith "Error: Invalid - syntax")
     | "*" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Times, (parse v1), (parse v2))
        | _ -> failwith "Error: Invalid * syntax")
     | "<" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Less, (parse v1), (parse v2))
        | _ -> failwith "Error: Invalid < syntax")
     | ">" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Greater, (parse v1), (parse v2))
        | _ -> failwith "Error: Invalid > syntax")
     | "==" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Equal, (parse v1), (parse v2))
        | _ -> failwith "Error: Invalid = syntax")
     | _ ->
       let s = Printf.sprintf "Error: unknown sexp %s" (to_string_hum sexp) in
       failwith s)
  | Atom s ->
    (if Str.string_match number_regex s 0 then
     (match int_of_string_opt s with
     | Some n -> (if n > boa_max || n < boa_min then
            failwith ("Error: Non-representable number "^ s)
         else
            ENumber(n))
     | None -> failwith ("Error: Non-representable number " ^s))
     else if (not (List.mem s reserved_words)) &&
          Str.string_match valid_id_regex s 0 then
         (match s with
          | "true" -> EBool(true)
          | "false" -> EBool(false)
          | _ -> EId(s))
       else
         failwith ("Invalid or unexpected id name " ^ s))
  | _ ->
    let s = Printf.sprintf "Error: unknown sexp %s" (to_string_hum sexp) in
    failwith s

and parse_binding binding =
  match binding with
  | List((Atom s)::t::[]) ->
    if (not (List.mem s reserved_words)) &&
       (not (List.mem s reserved_constants)) &&
       Str.string_match valid_id_regex s 0
    then
      (s,parse t)
    else
      failwith ("Invalid or unexpected name " ^ s)
  | _ -> failwith "Error: Invalid binding structure"
