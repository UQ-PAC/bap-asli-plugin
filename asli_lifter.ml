open Bap_core_theory
open Bap.Std
open LibASL

open KB.Syntax

include Self()

module Variables = Map.Make(String)

module Ignored = Set.Make(String)

type KB.Conflict.t +=
  | Unknown_primop of string
  | Unhandled_stmt of string
  | Unhandled_expr of string
  | Unhandled_lexpr of string
  | Unhandled_type of string
  | Failed_conversion of string
  | Unknown_variable of string

exception Error of string

let emptyKB = (KB.return (Theory.Effect.empty Theory.Effect.Sort.bot))

type 'a comp_state = {
  vars: unit Theory.Var.t Variables.t;
  ign: Ignored.t;
  kb: 'a KB.t;
  throwErrors: bool;
}

let updateKB (state: 'a comp_state) (kb: 'a KB.t): 'a comp_state =
  {
    vars = state.vars;
    ign = state.ign;
    kb;
    throwErrors = state.throwErrors;
  }

let updateVars (state: 'a comp_state) (vars: unit Theory.Var.t Variables.t): 'a comp_state =
  {
    vars;
    ign = state.ign;
    kb = state.kb;
    throwErrors = state.throwErrors;
  }

let const_width = 256

(** Delete all characters matching 'c' from string 'x' *)
let drop_chars (x: string) (c: char): string =
  (* First calculate final length *)
  let len = ref 0 in
  String.iter (fun t -> if t <> c then len := !len + 1) x;

  (* search for next character not matching c *)
  let i = ref 0 in
  let rec next_char (_: int): char =
      let r = String.get x !i in
      i := !i + 1;
      if r = c then next_char 0 else r
  in

  (* create result *)
  String.init !len next_char

let fail (errorType: KB.conflict) (throwErrors: bool) = 
  if throwErrors then
    let typeString = match errorType with
      | Unknown_primop(m) -> "Unknown primitive operation: " ^ m
      | Unhandled_stmt(m) -> "Unhandled statement type: " ^ m
      | Unhandled_expr(m) -> "Unhandled expression: " ^ m
      | Unhandled_lexpr(m) -> "Unhandled Lexpression: " ^ m
      | Unhandled_type(m) -> "Unhandled type: " ^ m
      | Failed_conversion(m) -> "Failed conversion: " ^ m
      | Unknown_variable(m) -> "Unknown variable: " ^ m
      | _ -> "Unknown error"
    in 
    raise (Error typeString)
  else
    KB.fail errorType

let to_bits throwErrors e = e >>= fun ue -> 
  match Theory.Value.resort (fun x -> (Theory.Bitv.refine x)) ue with
  | Some x -> KB.return x
  | None -> fail (Failed_conversion "Unable to convert value to bits") throwErrors

let defined throwErrors e =
  match e with
  | Some x -> KB.return x
  | None -> fail (Failed_conversion "Unable to get option") throwErrors

let to_bool throwErrors e = e >>= fun ue ->
  match Theory.Value.resort (fun x -> (Theory.Bool.refine x)) ue with
  | Some x -> KB.return x
  | None -> fail (Failed_conversion "Unable to convert value to bool") throwErrors

let to_mem throwErrors e = e >>= fun ue ->
  match Theory.Value.resort (fun x -> (Theory.Mem.refine x)) ue with
  | Some x -> KB.return x
  | None -> fail (Failed_conversion "Unable to convert value to mem") throwErrors
  
let null = KB.Object.null Theory.Program.cls

module Make(CT : Theory.Core) = struct
  let get_var s (state: 'a comp_state) = 
    match Variables.find_opt s state.vars with
    | Some var -> CT.var var
    | None -> fail (Unknown_variable s) state.throwErrors

  let ign_var s (state: 'a comp_state) = 
    Ignored.mem s state.ign

  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let ctrl eff =
    CT.blk null (seq []) eff

  let data (eff: Theory.data Theory.eff): unit Theory.eff =
    CT.blk null eff (seq [])

  let nop: unit Theory.eff =
    CT.blk null (seq []) (seq [])

  let compile_int (e: Asl_ast.expr): int option =
    match e with
    | Expr_LitBits(s) -> 
      let x' = drop_chars s ' ' in 
      Some (Z.to_int (Z.of_string_base 2 x'))
    | Expr_LitInt(s) ->
      Some (Z.to_int (Z.of_string_base 10 s))
    | _ -> None

  let rec compile_var (t: Asl_ast.ty) (ident: string) (value: Asl_ast.expr option) (state: 'a comp_state): 'a comp_state =
    let add_state t' = 
      let var = Theory.Var.forget (Theory.Var.define t' ident) in
      updateKB (updateVars state (Variables.add ident var state.vars)) (match value with
      | Some v -> CT.seq state.kb (data (CT.set var (compile_expr v state)))
      | None -> state.kb)
    in
    match t with
    | Type_Register (n, _)
    | Type_Bits (Expr_LitInt n) ->
      add_state (Theory.Value.Sort.forget (Theory.Bitv.define (int_of_string n)))
    | Type_Constructor (Ident "boolean") ->
      add_state (Theory.Value.Sort.forget Theory.Bool.t)
    | _ ->
      updateKB state (fail (Unhandled_type (Asl_utils.pp_type t)) state.throwErrors)

  and compile_expr (e: Asl_ast.expr) (state: 'a comp_state): unit Theory.Value.t KB.t =
    match e with
    | Expr_Var(Ident ("_PC")) ->
      fail (Unhandled_expr (Asl_utils.pp_expr e)) state.throwErrors
    | Expr_Var(Ident ("FALSE")) -> CT.b0 >>| Theory.Value.forget
    | Expr_Var(Ident ("TRUE")) -> CT.b1 >>| Theory.Value.forget
    | Expr_Var(id) ->
        let i = (Asl_ast.pprint_ident id) in
        if ign_var i state then
          fail (Unknown_variable (Asl_utils.pp_expr e)) state.throwErrors
        else get_var i state
    (* TODO: remove this and just use general case? *)
    | Expr_Slices(e, [Slice_LoWd(Expr_LitInt lo, Expr_LitInt wd)]) ->
      let e' = compile_expr e state in
      let lo' = int_of_string lo in
      let wd' = int_of_string wd in
      let hi = lo' + wd' - 1 in
      let s = Theory.Bitv.define wd' in
      let (module MX) = Bitvec.modular const_width in
      let const x = CT.int (Theory.Bitv.define const_width) (MX.int x) in
      (CT.extract s (const hi) (const lo') (to_bits state.throwErrors e')) >>| Theory.Value.forget
    | Expr_Slices(e, [Slice_LoWd(lo, wd)]) ->
      let e' = compile_expr e state in
      let lo' = compile_expr lo state |> to_bits state.throwErrors in
      let wd' = compile_expr wd state |> to_bits state.throwErrors in
      let (module MX) = Bitvec.modular const_width in
      let const x = CT.int (Theory.Bitv.define const_width) (MX.int x) in
      let hi = CT.add (CT.add lo' wd') (const (-1)) in
      let* s = wd' >>| Theory.Value.sort in
      (CT.extract s hi lo' (to_bits state.throwErrors e')) >>| Theory.Value.forget
    | Expr_Array(Expr_Var(Ident "_R"), Expr_LitInt(s)) ->
      get_var ("R" ^ s) state
    | Expr_Array(Expr_Var(Ident "_Z"), Expr_LitInt(s)) ->
      get_var ("V" ^ s) state
    | Expr_TApply(FIdent("replicate_bits", _), _, [e; Expr_LitInt(n)]) ->
      let e' = compile_expr e state |> to_bits state.throwErrors in
      let n' = int_of_string n in
      let* v = e' in
      let en = Theory.Bitv.size (Theory.Value.sort v) in
      List.fold_left (fun acc i -> 
        CT.append (Theory.Bitv.define (en * i)) acc (KB.return v)
      ) (KB.return v) (List.init (n' - 1) (fun x -> x + 2)) >>| Theory.Value.forget
    | Expr_TApply(FIdent("eq_enum", _), _, [e1; e2]) ->
      let e1' = compile_expr e1 state |> to_bool state.throwErrors in
      let e2' = compile_expr e2 state |> to_bool state.throwErrors in
      (* TODO: figure out a less hacky way to do eq *)
      CT.or_ (CT.and_ e1' e2') (CT.and_ (CT.inv e1') (CT.inv e2')) >>| Theory.Value.forget
    | Expr_TApply(f, [], [e1; e2]) ->
      let e1' = compile_expr e1 state |> to_bool state.throwErrors in
      let e2' = compile_expr e2 state |> to_bool state.throwErrors in
      (match Asl_ast.name_of_FIdent f with
      | "or_bool" ->
        CT.or_ e1' e2' >>| Theory.Value.forget
      | "and_bool" ->
        CT.and_ e1' e2' >>| Theory.Value.forget
      | n ->
        fail (Unknown_primop n) state.throwErrors
      ) 
    | Expr_TApply(f, targs, [e1; e2]) ->
      let e1' = compile_expr e1 state |> to_bits state.throwErrors in
      let e2' = compile_expr e2 state |> to_bits state.throwErrors in
      let* v1 = e1' in
      let* v2 = e2' in
      let n1 = Theory.Bitv.size (Theory.Value.sort v1) in
      let n2 = Theory.Bitv.size (Theory.Value.sort v2) in
      let e1' = KB.return v1 in
      let e2' = KB.return v2 in
      (match Asl_ast.name_of_FIdent f with
      | "or_bits" ->
        CT.logor e1' e2' >>| Theory.Value.forget
      | "and_bits" ->
        CT.logand e1' e2' >>| Theory.Value.forget
      | "eor_bits" ->
        CT.logxor e1' e2' >>| Theory.Value.forget
      | "eq_bits" ->
        CT.eq e1' e2' >>| Theory.Value.forget
      | "add_bits" ->
        CT.add e1' e2' >>| Theory.Value.forget
      | "sub_bits" ->
        CT.sub e1' e2' >>| Theory.Value.forget
      | "mul_bits" ->
        CT.mul e1' e2' >>| Theory.Value.forget
      | "sdiv_bits" ->
        CT.sdiv e1' e2' >>| Theory.Value.forget
      | "lsl_bits" ->
        CT.lshift e1' e2' >>| Theory.Value.forget
      | "lsr_bits" ->
        CT.rshift e1' e2' >>| Theory.Value.forget
      | "asr_bits" ->
        CT.arshift e1' e2' >>| Theory.Value.forget
      | "slt_bits" ->
        CT.slt e1' e2' >>| Theory.Value.forget
      | "sle_bits" ->
        CT.sle e1' e2' >>| Theory.Value.forget
      | "append_bits" ->
       CT.append (Theory.Bitv.define (n1 + n2)) (KB.return v1) (KB.return v2) >>| Theory.Value.forget
      | "ZeroExtend" ->
          (match targs with
          | [_;  Expr_LitInt(n)] ->
              let n' = int_of_string n in
              CT.unsigned (Theory.Bitv.define n') e1' >>| Theory.Value.forget
          | _ -> fail (Unknown_primop "ZeroExtend") state.throwErrors)
      | "SignExtend" ->
          (match targs with
          | [_;  Expr_LitInt(n)] ->
              let n' = int_of_string n in
              CT.signed (Theory.Bitv.define n') e1' >>| Theory.Value.forget
          | _ -> fail (Unknown_primop "SignExtend") state.throwErrors)
      (* TODO: implement round_tozero_real for aarch64_integer_arithmetic_div *)
      | n ->
        fail (Unknown_primop n) state.throwErrors
      )
    | Expr_TApply(f, _, [e]) ->
      let eUnsorted = compile_expr e state in
      let e' = eUnsorted |> to_bits  state.throwErrors in
      (match Asl_ast.name_of_FIdent f with
      | "not_bits" ->
        CT.not e' >>| Theory.Value.forget
      | "not_bool" ->
        let eBool = eUnsorted |> to_bool state.throwErrors in
        CT.inv eBool >>| Theory.Value.forget
      | "cvt_bool_bv" ->
        let s = Theory.Bitv.define 1 in
        let (module MX) = Bitvec.modular 1 in
        let const x = CT.int (s) (MX.int x) in

        let eBool = eUnsorted |> to_bool state.throwErrors in
        (CT.ite eBool (const 1) (const 0)) >>| Theory.Value.forget
      | "ZeroExtend" ->
        (* TODO: at the moment relying on implicit extension, may need to do it explicitly *)
        e' >>| Theory.Value.forget
      | n ->
        fail (Unknown_primop n) state.throwErrors
      )
    | Expr_TApply(FIdent("Mem.read", 0), _, [e1; e2; e3]) ->
      let e1' = compile_expr e1 state |> to_bits state.throwErrors in
      let* s = defined state.throwErrors (compile_int e2) in
      let mem = get_var "mem" state |> to_mem state.throwErrors in
      CT.loadw (Theory.Bitv.define (s * 8)) CT.b0 mem e1' >>| Theory.Value.forget
    | Expr_Parens(e) ->
      compile_expr e state
    | Expr_Field(Expr_Var(Ident "PSTATE"), Ident f) -> 
      get_var (f ^ "F") state
    | Expr_LitBits(s) -> 
      let x' = drop_chars s ' ' in 
      CT.int (Theory.Bitv.define (String.length x')) (Bitvec.bigint_unsafe (Z.of_string_base 2 x')) >>| Theory.Value.forget
    | Expr_LitInt(s) ->
      CT.int (Theory.Bitv.define 64) (Bitvec.bigint_unsafe (Z.of_string_base 10 s)) >>| Theory.Value.forget
    | _ ->
      fail (Unhandled_expr (Asl_utils.pp_expr e)) state.throwErrors

  and compile_stmt (s: Asl_ast.stmt) (state: 'a comp_state): ('a comp_state) =
    match s with
    | Stmt_VarDeclsNoInit(ty, vs, loc) ->
      List.fold_left (fun state' v -> compile_var ty (Asl_ast.pprint_ident v) None state') state vs
    | Stmt_VarDecl(ty, v, i, loc) ->
      compile_var ty (Asl_ast.pprint_ident v) (Some i) state
    | Stmt_ConstDecl(ty, v, i, loc) ->
      compile_var ty (Asl_ast.pprint_ident v) (Some i) state
    | Stmt_Assign(LExpr_Var(Ident("_PC")), r, _) ->
        let kb =  (match r with
        | Expr_LitBits(s) -> 
            let x' = drop_chars s ' ' in 
            let d = Bitvec.bigint_unsafe (Z.of_string_base 2 x') in
            ctrl @@ (Theory.Label.for_addr d >>= fun dst -> CT.goto dst)
        | Expr_LitInt(s) -> 
            let d = Bitvec.bigint_unsafe (Z.of_string_base 10 s) in
            ctrl @@ (Theory.Label.for_addr d >>= fun dst -> CT.goto dst)
        | _ -> 
          let r' = compile_expr r state in
          let dst = r' |> to_bits state.throwErrors in
          ctrl @@ CT.jmp dst) in
      updateKB state (CT.seq state.kb kb)
    | Stmt_Assign(l, r, _) ->
      let r' = compile_expr r state in
      let kb = (match l with
      | LExpr_Write(FIdent(i, n), tes, es) ->
        data @@ CT.set (Variables.find ("R" ^ string_of_int n) state.vars) r'
      | LExpr_Array(LExpr_Var(Ident "_R"), Expr_LitInt(s)) ->
        data @@ CT.set (Variables.find ("R" ^ s) state.vars) r'
      | LExpr_Array(LExpr_Var(Ident "_Z"), Expr_LitInt(s)) ->
        data @@ CT.set (Variables.find ("V" ^ s) state.vars) r'
      | LExpr_Var(Ident(i)) ->
        (match Variables.find_opt i state.vars with
        | Some var -> data @@ CT.set var r'
        | None -> 
            if ign_var i state then nop
            else fail (Unknown_variable i) state.throwErrors
        )
      | LExpr_Field(LExpr_Var(Ident "PSTATE"), Ident f) ->
          let i = f ^ "F" in
          if ign_var i state then nop
          else data @@ CT.set (Variables.find (f ^ "F") state.vars) r'
      | _ ->
        fail (Unhandled_lexpr (Asl_utils.pp_lexpr l)) state.throwErrors
      ) in
      updateKB state (CT.seq state.kb kb)
    | Stmt_If(c, t, els, e, loc) ->
      let rec do_if xs e = (match xs with
        | [] -> compile_stmts e (updateKB state emptyKB)
        | Asl_ast.S_Elsif_Cond (cond, b)::xs' -> 
          let cond' = compile_expr cond state in
          let cond'' = to_bool state.throwErrors cond' in
          let b' = compile_stmts b (updateKB state emptyKB) in
          let rest = do_if xs' e in
          CT.branch cond'' b' rest
      ) in
      updateKB state (CT.seq state.kb (do_if (Asl_ast.S_Elsif_Cond(c, t)::els) e))
    | Stmt_TCall(FIdent("Mem.set", 0), _, [addr; size; _; value], _) ->
      let addr' = compile_expr addr state |> to_bits state.throwErrors in
      let value' = compile_expr value state |> to_bits state.throwErrors in
      let memVar = Variables.find "mem" state.vars in
      let mem = get_var "mem" state |> to_mem state.throwErrors in

      (*let newMem' = CT.store mem addr' value' >>| Theory.Value.forget in*)
      let newMem = CT.storew CT.b0 mem addr' value' >>| Theory.Value.forget in

      let kb = data @@ CT.set memVar newMem in
      updateKB state (CT.seq state.kb kb)
    | Stmt_Assert(_, _) ->
      (* TODO: perform some kind of intrinsic call *)
      state
    | Stmt_Throw(_, _) ->
      (* TODO: perform some kind of intrinsic call *)
      state
    | _ ->
      updateKB state (CT.seq state.kb (fail (Unhandled_stmt (Asl_utils.pp_stmt s)) state.throwErrors))
  
  and compile_stmts (stmts: Asl_ast.stmt list) (state: 'a comp_state) =
    (List.fold_left (fun state' stmt -> compile_stmt stmt state') state stmts).kb
    
  let initialize_vars =
    (* 64-bit registers *)
    List.fold_left (fun vars var -> 
      Variables.add var (Theory.Var.forget (Theory.Var.define (Theory.Bitv.define 64) var)) vars
    ) Variables.empty (List.init 32 (fun i -> "R" ^ string_of_int i)) |>
    (* 128-bit registers *)
    (fun prevVars -> List.fold_left (fun vars var -> 
      Variables.add var (Theory.Var.forget (Theory.Var.define (Theory.Bitv.define 128) var)) vars
    ) prevVars (List.init 32 (fun i -> "V" ^ string_of_int i))) |>
    (* Flags *)
    Variables.add "NF" (Theory.Var.forget (Theory.Var.define (Theory.Bitv.define 1) "NF")) |>
    Variables.add "ZF" (Theory.Var.forget (Theory.Var.define (Theory.Bitv.define 1) "ZF")) |>
    Variables.add "CF" (Theory.Var.forget (Theory.Var.define (Theory.Bitv.define 1) "CF")) |>
    Variables.add "VF" (Theory.Var.forget (Theory.Var.define (Theory.Bitv.define 1) "VF")) |>

    (* Special registers *)
    (fun v -> Variables.add "SP_EL0" (Variables.find "R31" v) v) |>

    Variables.add "mem" (Theory.Var.forget (Theory.Var.define (Theory.Mem.define (Theory.Bitv.define 64) (Theory.Bitv.define 8)) "mem"))

  let initialize_ign =
    Ignored.add "_PC" Ignored.empty |>
    Ignored.add "SPF" |>
    Ignored.add "ELF" |>
    Ignored.add "nRWF" |>
    Ignored.add "InGuardedPage" |>
    Ignored.add "BTypeCompatible" |>
    Ignored.add "BTypeNext"

end

let lifter env throwErrors label =
  let* mem = label-->?Memory.slot in
  let* addr = label-->?Theory.Label.addr in
  let* (module CT) = Theory.current in
  let module Cmplr = Make(CT) in
  let vars = Cmplr.initialize_vars in
  let ign = Cmplr.initialize_ign in
  let (d, opcount) = Memory.fold ~word_size:Size.r32 mem ~init:(emptyKB, 0) ~f:(fun w (kb, opcount) ->
    let str = Bitvec.to_string (Bitvector.to_bitvec w) in
    let address = Some (Bitvec.to_string addr) in
    try (let stmts = (Dis.retrieveDisassembly ?address env str) in (Cmplr.compile_stmts stmts { vars; ign; kb; throwErrors }), opcount + 1)
    with 
    | Error(s) -> Printf.printf "%s had BAP error: %s\n" str s; (kb, opcount + 1)
    | exn -> 
      let error = String.trim (Printexc.to_string exn) in
      let backtrace = Printexc.get_backtrace () in
      let firstbacktrace = List.hd (String.split_on_char '\n' backtrace) in
      Printf.printf "%s had ASL error: %s, %s\n" str error firstbacktrace; (kb, opcount + 1)
  ) in
  if throwErrors then Printf.eprintf "%d\n" opcount;
  let* i = d in
  let* m = (CT.blk null (CT.perform Theory.Effect.Sort.bot) (CT.perform Theory.Effect.Sort.bot)) in
  KB.collect Disasm_expert.Basic.Insn.slot label >>| function
  | Some basic when Insn.(i <> empty) ->
      Insn.with_basic i basic
  | Some basic ->
      Insn.with_basic m basic
  | _ -> i

let load prelude specs throwErrors =
  Dis.debug_show_trace := true;
  let prelude = LoadASL.read_file prelude true false in
  let mra = List.map (fun tool -> LoadASL.read_file tool false false) specs in
  let env = Eval.build_evaluation_environment (List.concat (prelude::mra)) in
  KB.promise Theory.Semantics.slot (lifter env throwErrors)
