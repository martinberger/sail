open Ast
open Ast_util
open PPrint
open Type_check

type ctx =
  { register_inits : tannot exp list;
    externs : id Bindings.t
  }

let empty_ctx =
  { register_inits = [];
    externs = Bindings.empty
  }

let zchar c =
  let zc c = "z" ^ String.make 1 c in
  if Char.code c <= 41 then zc (Char.chr (Char.code c + 16))
  else if Char.code c <= 47 then zc (Char.chr (Char.code c + 23))
  else if Char.code c <= 57 then String.make 1 c
  else if Char.code c <= 64 then zc (Char.chr (Char.code c + 13))
  else if Char.code c <= 90 then String.make 1 c
  else if Char.code c <= 94 then zc (Char.chr (Char.code c - 13))
  else if Char.code c <= 95 then "_"
  else if Char.code c <= 96 then zc (Char.chr (Char.code c - 13))
  else if Char.code c <= 121 then String.make 1 c
  else if Char.code c <= 122 then "zz"
  else if Char.code c <= 126 then zc (Char.chr (Char.code c - 39))
  else raise (Invalid_argument "zchar")

let zencode_string str = "z" ^ List.fold_left (fun s1 s2 -> s1 ^ s2) "" (List.map zchar (Util.string_to_list str))

let zencode_upper_string str = "Z" ^ List.fold_left (fun s1 s2 -> s1 ^ s2) "" (List.map zchar (Util.string_to_list str))

let zencode ctx id =
  try string (string_of_id (Bindings.find id ctx.externs)) with
  | Not_found -> string (zencode_string (string_of_id id))

let zencode_upper ctx id =
  try string (string_of_id (Bindings.find id ctx.externs)) with
  | Not_found -> string (zencode_upper_string (string_of_id id))

let zencode_kid kid = string ("'" ^ zencode_string (string_of_id (id_of_kid kid)))

let ocaml_typ_id ctx = function
  | id when Id.compare id (mk_id "string") = 0 -> string "string"
  | id when Id.compare id (mk_id "list") = 0 -> string "list"
  | id when Id.compare id (mk_id "bit") = 0 -> string "bit"
  | id when Id.compare id (mk_id "int") = 0 -> string "big_int"
  | id when Id.compare id (mk_id "bool") = 0 -> string "bool"
  | id -> zencode ctx id

let rec ocaml_typ ctx (Typ_aux (typ_aux, _)) =
  match typ_aux with
  | Typ_id id -> ocaml_typ_id ctx id
  | Typ_app (id, []) -> ocaml_typ_id ctx id
  | Typ_app (id, typs) -> parens (separate_map (string " * ") (ocaml_typ_arg ctx) typs) ^^ space ^^ ocaml_typ_id ctx id
  | Typ_tup typs -> parens (separate_map (string " * ") (ocaml_typ ctx) typs)
  | Typ_fn (typ1, typ2, _) -> separate space [ocaml_typ ctx typ1; string "->"; ocaml_typ ctx typ2]
  | Typ_var kid -> zencode_kid kid
  | Typ_exist _ | Typ_wild -> assert false
and ocaml_typ_arg ctx (Typ_arg_aux (typ_arg_aux, _) as typ_arg) =
  match typ_arg_aux with
  | Typ_arg_typ typ -> ocaml_typ ctx typ
  | _ -> failwith ("OCaml: unexpected type argument " ^ string_of_typ_arg typ_arg)

let ocaml_typquant typq =
  let ocaml_qi = function
    | QI_aux (QI_id kopt, _) -> zencode_kid (kopt_kid kopt)
    | QI_aux (QI_const _, _) -> failwith "Ocaml type quantifiers should no longer contain constraints"
  in
  match quant_items typq with
  | [] -> empty
  | [qi] -> ocaml_qi qi
  | qis -> parens (separate_map (string " * ") ocaml_qi qis)

let ocaml_lit (L_aux (lit_aux, _)) =
  match lit_aux with
  | L_unit -> string "()"
  | L_zero -> string "B0"
  | L_one -> string "B1"
  | L_true -> string "true"
  | L_false -> string "false"
  | L_num n -> parens (string "big_int_of_int" ^^ space ^^ string (string_of_int n))
  | L_undef -> failwith "undefined should have been re-written prior to ocaml backend"
  | L_string str -> dquotes (string (String.escaped str))
  | _ -> string "LIT"

let rec ocaml_pat ctx (P_aux (pat_aux, _) as pat) =
  match pat_aux with
  | P_id id ->
     begin
       match Env.lookup_id id (pat_env_of pat) with
       | Local (Immutable, _) | Unbound -> zencode ctx id
       | Enum _ -> zencode_upper ctx id
       | _ -> failwith "Ocaml: Cannot pattern match on mutable variable or register"
     end
  | P_lit lit -> ocaml_lit lit
  | P_typ (_, pat) -> ocaml_pat ctx pat
  | P_tup pats -> parens (separate_map (comma ^^ space) (ocaml_pat ctx) pats)
  | P_list pats -> brackets (separate_map (semi ^^ space) (ocaml_pat ctx) pats)
  | P_wild -> string "_"
  | P_as (pat, id) -> separate space [ocaml_pat ctx pat; string "as"; zencode ctx id]
  | _ -> string ("PAT<" ^ string_of_pat pat ^ ">")

let begin_end doc = group (string "begin" ^^ nest 2 (break 1 ^^ doc) ^/^ string "end")

let rec ocaml_exp ctx (E_aux (exp_aux, _) as exp) =
  match exp_aux with
  | E_app (f, [x]) -> zencode ctx f ^^ space ^^ ocaml_atomic_exp ctx x
  | E_app (f, xs) -> zencode ctx f ^^ space ^^ parens (separate_map (comma ^^ space) (ocaml_exp ctx) xs)
  | E_return exp -> separate space [string "r.return"; ocaml_atomic_exp ctx exp]
  | E_assert (exp, _) -> separate space [string "assert"; ocaml_atomic_exp ctx exp]
  | E_cast (_, exp) -> ocaml_exp ctx exp
  | E_block [exp] -> ocaml_exp ctx exp
  | E_block [] -> string "()"
  | E_block exps -> begin_end (ocaml_block ctx exps)
  | E_field (exp, id) -> ocaml_atomic_exp ctx exp ^^ dot ^^ zencode ctx id
  | E_exit exp -> string "exit 0"
  | E_case (exp, pexps) ->
     begin_end (separate space [string "match"; ocaml_atomic_exp ctx exp; string "with"]
                ^/^ ocaml_pexps ctx pexps)
  | E_assign (lexp, exp) -> separate space [ocaml_lexp ctx lexp; string ":="; ocaml_exp ctx exp]
  | E_if (c, t, e) -> separate space [string "if"; ocaml_atomic_exp ctx c;
                                      string "then"; ocaml_atomic_exp ctx t;
                                      string "else"; ocaml_atomic_exp ctx e]
  | E_record (FES_aux (FES_Fexps (fexps, _), _)) ->
     enclose lbrace rbrace (group (separate_map (semi ^^ break 1) (ocaml_fexp ctx) fexps))
  | E_record_update (exp, FES_aux (FES_Fexps (fexps, _), _)) ->
     enclose lbrace rbrace (separate space [ocaml_atomic_exp ctx exp;
                                            string "with";
                                            separate_map (semi ^^ space) (ocaml_fexp ctx) fexps])
  | E_let (lb, exp) ->
     separate space [string "let"; ocaml_letbind ctx lb; string "in"]
     ^/^ ocaml_exp ctx exp
  | E_internal_let (lexp, exp1, exp2) ->
     separate space [string "let"; ocaml_atomic_lexp ctx lexp;
                     equals; string "ref"; ocaml_atomic_exp ctx exp1; string "in"]
     ^/^ ocaml_exp ctx exp2
  | E_lit _ | E_list _ | E_id _ | E_tuple _ -> ocaml_atomic_exp ctx exp
  | _ -> string ("EXP(" ^ string_of_exp exp ^ ")")
and ocaml_letbind ctx (LB_aux (lb_aux, _)) =
  match lb_aux with
  | LB_val_implicit (pat, exp) -> separate space [ocaml_pat ctx pat; equals; ocaml_atomic_exp ctx exp]
  | _ -> failwith "Ocaml: Explicit letbind found"
and ocaml_pexps ctx = function
  | [pexp] -> ocaml_pexp ctx pexp
  | pexp :: pexps -> ocaml_pexp ctx pexp ^/^ ocaml_pexps ctx pexps
  | [] -> empty
and ocaml_pexp ctx = function
  | Pat_aux (Pat_exp (pat, exp), _) ->
     separate space [bar; ocaml_pat ctx pat; string "->"]
     ^//^ group (ocaml_exp ctx exp)
  | Pat_aux (Pat_when (pat, wh, exp), _) ->
     separate space [bar; ocaml_pat ctx pat; string "when"; ocaml_atomic_exp ctx exp; string "->"]
     ^//^ group (ocaml_exp ctx exp)
and ocaml_block ctx = function
  | [exp] -> ocaml_exp ctx exp
  | exp :: exps -> ocaml_exp ctx exp ^^ semi ^/^ ocaml_block ctx exps
  | _ -> assert false
and ocaml_fexp ctx (FE_aux (FE_Fexp (id, exp), _)) =
  separate space [zencode ctx id; equals; ocaml_exp ctx exp]
and ocaml_atomic_exp ctx (E_aux (exp_aux, _) as exp) =
  match exp_aux with
  | E_lit lit -> ocaml_lit lit
  | E_id id ->
     begin
       match Env.lookup_id id (env_of exp) with
       | Local (Immutable, _) | Unbound -> zencode ctx id
       | Enum _ -> zencode_upper ctx id
       | Register _ | Local (Mutable, _) -> bang ^^ zencode ctx id
       | _ -> failwith ("Union constructor: " ^ zencode_string (string_of_id id))
     end
  | E_list exps -> enclose lbracket rbracket (separate_map (semi ^^ space) (ocaml_exp ctx) exps)
  | E_tuple exps -> parens (separate_map (comma ^^ space) (ocaml_exp ctx) exps)
  | _ -> parens (ocaml_exp ctx exp)
and ocaml_lexp ctx (LEXP_aux (lexp_aux, _) as lexp) =
  match lexp_aux with
  | LEXP_cast _ | LEXP_id _ -> ocaml_atomic_lexp ctx lexp
  | _ -> string ("LEXP<" ^ string_of_lexp lexp ^ ">")
and ocaml_atomic_lexp ctx (LEXP_aux (lexp_aux, _) as lexp) =
  match lexp_aux with
  | LEXP_cast (_, id) -> zencode ctx id
  | LEXP_id id -> zencode ctx id
  | _ -> parens (ocaml_lexp ctx lexp)

let rec get_initialize_registers = function
  | DEF_fundef (FD_aux (FD_function (_, _, _, [FCL_aux (FCL_Funcl (id, _, E_aux (E_block inits, _)), _)]), _)) :: defs
       when Id.compare id (mk_id "initialize_registers") = 0 ->
     inits
  | _ :: defs -> get_initialize_registers defs
  | [] -> []

let initial_value_for id inits =
  let find_reg = function
    | E_aux (E_assign (LEXP_aux (LEXP_cast (_, reg_id), _), init), _) when Id.compare id reg_id = 0 -> Some init
    | _ -> None
  in
  match Util.option_first find_reg inits with
  | Some init -> init
  | None -> failwith ("No assignment to register ^ " ^ string_of_id id ^ " in initialize_registers")

let ocaml_dec_spec ctx (DEC_aux (reg, _)) =
  match reg with
  | DEC_reg (typ, id) ->
     separate space [string "let"; zencode ctx id; colon;
                     parens (ocaml_typ ctx typ); string "ref"; equals;
                     string "ref"; parens (ocaml_exp ctx (initial_value_for id ctx.register_inits))]
  | _ -> failwith "Unsupported register declaration"

let funcls_id = function
  | [] -> failwith "Ocaml: empty function"
  | FCL_aux (FCL_Funcl (id, pat, exp),_) :: _ -> id

let ocaml_funcl_match ctx (FCL_aux (FCL_Funcl (id, pat, exp), _)) =
  separate space [bar; ocaml_pat ctx pat; string "->"]
  ^//^ group (string "with_return (fun r ->" ^//^ ocaml_exp ctx exp ^^ rparen)

let rec ocaml_funcl_matches ctx = function
  | [] -> failwith "Ocaml: empty function"
  | [clause] -> ocaml_funcl_match ctx clause
  | (clause :: clauses) -> ocaml_funcl_match ctx clause ^/^ ocaml_funcl_matches ctx clauses

let ocaml_funcls ctx = function
  | [] -> failwith "Ocaml: empty function"
  | [FCL_aux (FCL_Funcl (id, pat, exp),_)] ->
     separate space [string "let rec"; zencode ctx id; ocaml_pat ctx pat; equals; string "with_return (fun r ->"]
     ^//^ ocaml_exp ctx exp
     ^^ rparen
  | funcls ->
     let id = funcls_id funcls in
     separate space [string "let rec"; zencode ctx id; equals; string "function"]
     ^//^ ocaml_funcl_matches ctx funcls

let ocaml_fundef ctx (FD_aux (FD_function (_, _, _, funcls), _)) =
  ocaml_funcls ctx funcls

let rec ocaml_fields ctx =
  let ocaml_field typ id =
    separate space [zencode ctx id; colon; ocaml_typ ctx typ]
  in
  function
  | [(typ, id)] -> ocaml_field typ id
  | (typ, id) :: fields -> ocaml_field typ id ^^ semi ^/^ ocaml_fields ctx fields
  | [] -> empty

let rec ocaml_cases ctx =
  let ocaml_case = function
    | Tu_aux (Tu_id id, _) -> separate space [bar; zencode ctx id]
    | Tu_aux (Tu_ty_id (typ, id), _) -> separate space [bar; zencode ctx id; string "of"; ocaml_typ ctx typ]
  in
  function
  | [tu] -> ocaml_case tu
  | tu :: tus -> ocaml_case tu ^/^ ocaml_cases ctx tus
  | [] -> empty

let rec ocaml_enum ctx = function
  | [id] -> zencode_upper ctx id
  | id :: ids -> zencode_upper ctx id ^/^ (bar ^^ space ^^ ocaml_enum ctx ids)
  | [] -> empty

let ocaml_typedef ctx (TD_aux (td_aux, _)) =
  match td_aux with
  | TD_record (id, _, typq, fields, _) ->
     (separate space [string "type"; ocaml_typquant typq; zencode ctx id; equals; lbrace]
      ^//^ ocaml_fields ctx fields)
     ^/^ rbrace
  | TD_variant (id, _, typq, cases, _) ->
     separate space [string "type"; ocaml_typquant typq; zencode ctx id; equals]
     ^//^ ocaml_cases ctx cases
  | TD_enum (id, _, ids, _) ->
     separate space [string "type"; zencode ctx id; equals]
     ^//^ (bar ^^ space ^^ ocaml_enum ctx ids)
  | TD_abbrev (id, _, TypSchm_aux (TypSchm_ts (typq, typ), _)) ->
     separate space [string "type"; ocaml_typquant typq; zencode ctx id; equals; ocaml_typ ctx typ]
  | _ -> failwith "Unsupported typedef"

let get_externs (Defs defs) =
  let extern_id (VS_aux (vs_aux, _)) =
    match vs_aux with
    | VS_val_spec (typschm, id) -> []
    | VS_extern_no_rename (typschm, id) -> [(id, id)]
    | VS_extern_spec (typschm, id, name) -> [(id, mk_id name)]
    | VS_cast_spec (typschm, id) -> []
  in
  let rec extern_ids = function
    | DEF_spec vs :: defs -> extern_id vs :: extern_ids defs
    | def :: defs -> extern_ids defs
    | [] -> []
  in
  List.fold_left (fun exts (id, name) -> Bindings.add id name exts) Bindings.empty (List.concat (extern_ids defs))

let ocaml_def_end = string ";;" ^^ twice hardline

let ocaml_def ctx def = match def with
  | DEF_reg_dec ds -> group (ocaml_dec_spec ctx ds) ^^ ocaml_def_end
  | DEF_fundef fd -> group (ocaml_fundef ctx fd) ^^ ocaml_def_end
  | DEF_type td -> group (ocaml_typedef ctx td) ^^ ocaml_def_end
  | _ -> empty

let ocaml_defs (Defs defs) =
  let ctx = { register_inits = get_initialize_registers defs;
              externs = get_externs (Defs defs)
            }
  in
  let empty_reg_init =
    if ctx.register_inits = []
    then
      separate space [string "let"; string "initialize_registers"; string "()"; equals; string "()"]
      ^^ ocaml_def_end
    else empty
  in
  (string "open Sail_lib;;" ^^ hardline)
  ^^ (string "open Big_int" ^^ ocaml_def_end)
  ^^ concat (List.map (ocaml_def ctx) defs)
  ^^ empty_reg_init

let ocaml_main spec =
  concat [separate space [string "open"; string (String.capitalize spec)] ^^ ocaml_def_end;
          separate space [string "open"; string "Elf_loader"] ^^ ocaml_def_end;
          separate space [string "let"; string "()"; equals]
          ^//^ (string "Random.self_init ();"
                ^/^ string "load_elf ();"
                ^/^ string "initialize_registers ();"
                ^/^ string "zmain ()")
         ]

let ocaml_pp_defs f defs =
  ToChannel.pretty 1. 80 f (ocaml_defs defs)

let ocaml_compile spec defs =
  let sail_lib_dir =
    try Sys.getenv "SAILLIBDIR" with
    | Not_found -> failwith "Environment variable SAILLIBDIR needs to be set"
  in
  if Sys.file_exists "_sbuild" then () else Unix.mkdir "_sbuild" 0o775;
  let cwd = Unix.getcwd () in
  Unix.chdir "_sbuild";
  let _ = Unix.system ("cp -r " ^ sail_lib_dir ^ "/ocaml_rts/. .") in
  let out_chan = open_out (spec ^ ".ml") in
  ocaml_pp_defs out_chan defs;
  close_out out_chan;
  if IdSet.mem (mk_id "main") (Initial_check.val_spec_ids defs)
  then
    begin
      print_endline "Generating main";
      let out_chan = open_out "main.ml" in
      ToChannel.pretty 1. 80 out_chan (ocaml_main spec);
      close_out out_chan;
      let _ = Unix.system "ocamlbuild -pkg zarith -pkg uint main.native" in
      let _ = Unix.system ("cp main.native " ^ cwd ^ "/" ^ spec) in
      ()
    end
  else
    let _ = Unix.system ("ocamlbuild -pkg zarith -pkg uint " ^ spec ^ ".cmo") in
    ();
  Unix.chdir cwd
