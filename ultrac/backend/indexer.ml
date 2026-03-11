open Shared.Isa
open Shared.Transformer

let index_cunit (unit: CUnit.t): unit =
  let open Shared.Utils.UString in

  let current_addr = ref Platform.red_zone_size in

  let advance (offset: int): unit =
    current_addr := !current_addr + offset
  in

  let register_symbol (symbol_name: string) (symbol_addr: int) =
    HashMap.add unit.symbols symbol_name symbol_addr
  in

  CUnit.foreach_block unit (fun b ->
    b.start_addr <- Some !current_addr;
    match b.kind with
    | Reserved ->
      begin match b.seq with
      | [(Meta (Label symbol))] -> 
        register_symbol symbol !current_addr;
        advance @@ Option.get b.size
      | _ -> Shared.Errors.should_not_reach_here "Reserved block should contain only label"
      end
    | Function ->
      Block.foreach_elem b (fun elem ->
        match elem with
        | Meta (Label symbol) -> register_symbol symbol !current_addr
        | Instruction instr   -> advance @@ Instruction.size instr
      );
    let size = !current_addr - (Option.get b.start_addr) in
    b.size <- Some size
  )

(** Computes blocks placement inside image and fills symbol table *)
let index: ir_transformer = fun input ->
  match input with
  | LLIR ir -> index_cunit ir; LLIR ir
  | _ -> invalid_ir_kind "LLIR" "LLIR"