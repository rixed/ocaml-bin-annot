(*
 * Some misc. helper functions
 *)

let debug = false
let log fmt =
  if debug then Printf.eprintf (fmt ^^ "\n")
  else Printf.ifprintf stderr fmt

external reraise : exn -> 'a = "%reraise"

let list_find_map f l =
  let rec loop = function
    | [] -> raise Not_found
    | x :: l ->
        (match f x with
        | Some v -> v
        | None -> loop l) in
  loop l

(* We will compare only the basename with no extension when comparing
 * locations: *)
let simplified_filename f =
  Filename.(basename f |> remove_extension)

(*
 * Some printer to help with debugging:
 *)

let print_lexing_position oc l =
  Printf.fprintf oc "%s:%d.%d"
    (simplified_filename l.Lexing.pos_fname)
    l.pos_lnum
    (l.pos_cnum - l.pos_bol)

let print_location oc l =
  Printf.fprintf oc "%a...%a"
    print_lexing_position l.Location.loc_start
    print_lexing_position l.loc_end

let print_loc printer oc loc =
  Printf.fprintf oc "%a@%a"
    printer loc.Location.txt
    print_location loc.loc

let print_type_signature oc ts =
  Format.asprintf "%a" Printtyp.signature ts |>
  output_string oc

let print_type_expr oc te =
  Format.asprintf "%a" Printtyp.type_expr te |>
  output_string oc

let print_module_expr_desc oc = function
  | Typedtree.Tmod_ident _ -> output_string oc "Tmod_ident"
  | Tmod_structure _ -> output_string oc "Tmod_structure"
  | Tmod_functor _ -> output_string oc "Tmod_functor"
  | Tmod_apply _ -> output_string oc "Tmod_apply"
  | Tmod_constraint _ -> output_string oc "Tmod_constraint"
  | Tmod_unpack _ -> output_string oc "Tmod_unpack"

let print_structure_item_desc oc = function
  | Typedtree.Tstr_eval _ -> output_string oc "Tstr_eval"
  | Tstr_value _ -> output_string oc "Tstr_value"
  | Tstr_primitive _ -> output_string oc "Tstr_primitive"
  | Tstr_type _ -> output_string oc "Tstr_type"
  | Tstr_typext _ -> output_string oc "Tstr_typext"
  | Tstr_exception _ -> output_string oc "Tstr_exception"
  | Tstr_module _ -> output_string oc "Tstr_module"
  | Tstr_recmodule _ -> output_string oc "Tstr_recmodule"
  | Tstr_modtype _ -> output_string oc "Tstr_modtype"
  | Tstr_open _ -> output_string oc "Tstr_open"
  | Tstr_class _ -> output_string oc "Tstr_class"
  | Tstr_class_type _ -> output_string oc "Tstr_class_type"
  | Tstr_include _ -> output_string oc "Tstr_include"
  | Tstr_attribute _ -> output_string oc "Tstr_attribute"

let print_expression_desc oc = function
  | Typedtree.Texp_ident _ -> output_string oc "Texp_ident"
  | Texp_constant _ -> output_string oc "Texp_constant"
  | Texp_let _ -> output_string oc "Texp_let"
  | Texp_function _ -> output_string oc "Texp_function"
  | Texp_apply _ -> output_string oc "Texp_apply"
  | Texp_match _ -> output_string oc "Texp_match"
  | Texp_try _ -> output_string oc "Texp_try"
  | Texp_tuple _ -> output_string oc "Texp_tuple"
  | Texp_construct _ -> output_string oc "Texp_construct"
  | Texp_variant _ -> output_string oc "Texp_variant"
  | Texp_record _ -> output_string oc "Texp_record"
  | Texp_field _ -> output_string oc "Texp_field"
  | Texp_setfield _ -> output_string oc "Texp_setfield"
  | Texp_array _ -> output_string oc "Texp_array"
  | Texp_ifthenelse _ -> output_string oc "Texp_ifthenelse"
  | Texp_sequence _ -> output_string oc "Texp_sequence"
  | Texp_while _ -> output_string oc "Texp_while"
  | Texp_for _ -> output_string oc "Texp_for"
  | Texp_send _ -> output_string oc "Texp_send"
  | Texp_new _ -> output_string oc "Texp_new"
  | Texp_instvar _ -> output_string oc "Texp_instvar"
  | Texp_setinstvar _ -> output_string oc "Texp_setinstvar"
  | Texp_override _ -> output_string oc "Texp_override"
  | Texp_letmodule _ -> output_string oc "Texp_letmodule"
  | Texp_letexception _ -> output_string oc "Texp_letexception"
  | Texp_assert _ -> output_string oc "Texp_assert"
  | Texp_lazy _ -> output_string oc "Texp_lazy"
  | Texp_object _ -> output_string oc "Texp_object"
  | Texp_pack _ -> output_string oc "Texp_pack"
  (*| Texp_letop _ -> output_string oc "Texp_letop"*)
  | Texp_unreachable -> output_string oc "Texp_unreachable"
  | Texp_extension_constructor _ -> output_string oc "Texp_extension_constructor"
  (*| Texp_open _ -> output_string oc "Texp_open"*)

let print_pattern_desc oc = function
  | Typedtree.Tpat_any -> output_string oc "Tpat_any"
  | Tpat_var _ -> output_string oc "Tpat_var"
  | Tpat_alias _ -> output_string oc "Tpat_alias"
  | Tpat_constant _ -> output_string oc "Tpat_constant"
  | Tpat_tuple _ -> output_string oc "Tpat_tuple"
  | Tpat_construct _ -> output_string oc "Tpat_construct"
  | Tpat_variant _ -> output_string oc "Tpat_variant"
  | Tpat_record _ -> output_string oc "Tpat_record"
  | Tpat_array _ -> output_string oc "Tpat_array"
  | Tpat_lazy _ -> output_string oc "Tpat_lazy"
  (*| Tpat_value _ -> output_string oc "Tpat_value"*)
  (*| Tpat_exception _ -> output_string oc "Tpat_exception"*)
  | Tpat_or _ -> output_string oc "Tpat_or"

(*
 * The location which type we are looking for:
 *)

let line = ref (-1)
let col = ref (-1)
let file = ref ""

(* Is the searched location above within the passed [loc]? *)
let within loc =
  let between a b x = a <= x && x <= b
  and cnum l = l.Lexing.pos_cnum - l.pos_bol in
  between loc.Location.loc_start.Lexing.pos_lnum loc.loc_end.pos_lnum !line &&
  (
    if !line = loc.loc_start.pos_lnum then
      cnum loc.loc_start <= !col
    else true
  ) && (
    if !line = loc.loc_end.pos_lnum then
      !col <= cnum loc.loc_end
    else true
  ) &&
  simplified_filename loc.loc_start.pos_fname = !file

let if_within what loc f =
  if within loc then
    f ()
  else (
    log "%s: out of %a" what print_location loc ;
    raise Not_found
  )

(*
 * Searching functions
 *)

let search_list what f l =
  try
    list_find_map (fun i ->
      try Some (f i)
      with Not_found -> None
    ) l
  with Not_found as e ->
    log "No matching %s" what ;
    reraise e

let rec search_module_expr_desc = function
  | Typedtree.Tmod_structure s ->
      search_structure s
  | Tmod_constraint (me, _, _, _) ->
      search_module_expr me
  | x ->
      log "search_module_expr_desc(%a)" print_module_expr_desc x ;
      raise Not_found

and search_module_expr me =
  if_within "search_module_expr" me.Typedtree.mod_loc (fun () ->
    search_module_expr_desc me.mod_desc)

and search_module_binding b =
  if_within "search_module_binding" b.Typedtree.mb_loc (fun () ->
    search_module_expr b.mb_expr)

and search_case c =
  try search_pattern c.Typedtree.c_lhs
  with Not_found ->
    try
      match c.c_guard with
      | None -> raise Not_found
      | Some e -> search_expression e
    with Not_found ->
      search_expression c.c_rhs

and search_apply_args = function
  | _label, None -> raise Not_found
  | _label, Some e -> search_expression e

and search_expression_desc = function
  | Typedtree.Texp_ident (_path, _loc, value_desc) ->
      value_desc.Types.val_type
  | Texp_function { cases ; _ } ->
      search_list "Texp_function" search_case cases
  | Texp_apply (e, args) ->
      (try search_expression e
      with Not_found -> search_list "Texp_apply" search_apply_args args)
  | Texp_let (_, vbs, e) ->
      (try search_list "Texp_let" search_value_binding vbs
      with Not_found -> search_expression e)
  | x ->
    log "search_expression_desc(%a)" print_expression_desc x ;
    raise Not_found

and search_expression ?(skip_location_check=false) e =
  let f () =
    try search_expression_desc e.Typedtree.exp_desc
    with Not_found -> e.exp_type in
  if skip_location_check then f () else
  if_within "search_expression" e.Typedtree.exp_loc f

and search_pattern_desc = function
  | x ->
      log "search_pattern_desc(%a)" print_pattern_desc x ;
      raise Not_found

and search_pattern p =
  if_within "search_pattern" p.Typedtree.pat_loc (fun () ->
    (* Have a closer look at the pattern before defaulting to the overall type: *)
    try search_pattern_desc p.pat_desc
    with Not_found -> p.pat_type)

and search_value_binding b =
  if_within "search_value_binding" b.Typedtree.vb_loc (fun () ->
    (* We are in a pattern, either we have some type info in there for a
     * deconstructed element, or we just output the type of the expression
     * that's being pattern matched: *)
    try search_pattern b.vb_pat
    with Not_found ->
      search_expression ~skip_location_check:true b.vb_expr)

and search_structure_item_desc = function
  | Typedtree.Tstr_module b ->
      search_module_binding b
  | Tstr_recmodule bs ->
      search_list "Tstr_recmodule" search_module_binding bs
  | Tstr_value (_, bs) ->
      search_list "Tstr_value" search_value_binding bs
  | x ->
      log "search_structure_item_desc(%a)" print_structure_item_desc x ;
      raise Not_found

and print_env oc _env =
  Printf.fprintf oc "…"

and search_structure_item si =
  if_within "search_structure_item" si.Typedtree.str_loc (fun () ->
    search_structure_item_desc si.str_desc)

and search_structure s =
  search_list "search_structure_item" search_structure_item s.Typedtree.str_items

(* Print a map of all encountered location->type *)
let search_annots = function
  | Ocaml_common.Cmt_format.Packed _ ->
      log "search_annots: Packed" ;
      raise Not_found
  | Implementation s ->
      search_structure s
  | Interface signature ->
      log "search_annots: Interface" ;
      raise Not_found
  | Partial_implementation binary_parts ->
      log "search_annots: Partial_implementation" ;
      raise Not_found
  | Partial_interface binary_parts ->
      log "search_annots: Partial_interface" ;
      raise Not_found

let start_search cmt_file =
  let cmt = Ocaml_common.Cmt_format.read_cmt cmt_file in
  search_annots cmt.cmt_annots

(*
 * Entry point
 *)

let () =
  let usage_msg = "bin-annot [-n] [-help] [-type line col] file.cmt" in
  let path = ref "" in
  let skip_newline_at_end = ref false in
  let opts =
    Arg.[ "-n", Set skip_newline_at_end, "suppress newline after last line" ;
          "-type", Tuple [ Set_int line ; Set_int col ],
          "lookup type annotation for position" ] in
  let anon_arg a =
    if !path <> "" then raise (Arg.Bad a) else path := a in
  Arg.parse opts anon_arg usage_msg ;
  if !path = "" || !line < 0 || !col < 0 then (
    Arg.usage opts usage_msg
  ) else if !line = 0 || !col = 0 then (
    Printf.eprintf "Line and column numbers starts at 1.\n" ;
    exit 1
  ) else (
    (* Convert command line arguments from column numbers to column offset
     * (as in [Lexing.position]): *)
    col := !col - 1 ;
    file := simplified_filename !path ;
    log "Looking for the type at %s:%d.%d" !file !line !col ;
    let te = start_search !path in
    print_type_expr stdout te ;
    if not !skip_newline_at_end then print_newline ()
  )
