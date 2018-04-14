open Sexplib.Sexp
open Expr

let reserved_words = ["let"; "add1"; "sub1"]
let int_of_string_opt s =
  try Some(int_of_string s) with
  | _ -> None

let rec parse (sexp : Sexplib.Sexp.t) =
  match sexp with
  | List((Atom s)::t) ->
    (match s with
     | "let" ->
       (match t with
        | (List(bindings))::body::[] ->
          let binds = List.map parse_bindings bindings in
          let body = parse body in
          ELet(binds, body)
        | _ -> failwith "Error: invalid let syntax")
     |  "add1" ->
       (match t with
        | t::[] ->
          EPrim1(Add1, (parse t))
        | _ -> failwith "Error: Too many arguments to add1")
     | "sub1" ->
       (match t with
        | t::[] ->
          EPrim1(Sub1, (parse t))
        | _ -> failwith "Error: Too many arguments to sub1")
     |  "+" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Plus, (parse v1), (parse v2))
        | _ -> failwith "Error: invalid + syntax")
     | "-" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Minus, (parse v1), (parse v2))
        | _ -> failwith "Error: invalid - syntax")
     | "*" ->
       (match t with
        | v1::v2::[] ->
          EPrim2(Times, (parse v1), (parse v2))
        | _ -> failwith "Error: invalid * syntax")
     | _ ->
       let s = Printf.sprintf "Error: unknown sexp %s" (to_string_hum sexp) in
       failwith s)
  | Atom s ->
    (match int_of_string_opt s with
     | Some n -> ENumber(n)
     | None ->
       if (not (List.mem s reserved_words)) then
         EId(s)
       else
         failwith ("Invalid or unexpected id name " ^ s))
  | _ ->
    let s = Printf.sprintf "Error: unknown sexp %s" (to_string_hum sexp) in
    failwith s

and parse_bindings binding =
  match binding with
  | List((Atom s)::t::[]) ->
    (s,parse t)
  | _ -> failwith "Error: invalid binding structure"
