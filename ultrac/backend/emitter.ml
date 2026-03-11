open Shared
open Shared.Isa
open Shared.Transformer

let emit_cunit (image: bytes) (unit: CUnit.t): unit =

  let emit_value (width: Instruction.width) (addr: int) (v: int) = match width with
    | W8  -> Bytes.set_int8     image addr v
    | W16 -> Bytes.set_int16_be image addr v
  in

  let emit_instruction (start_addr: int) (instr: Instruction.t): unit =
    let encoded_uop = Instruction.opcode_to_enum instr.uop in
    Bytes.set_int8 image start_addr encoded_uop;
    ignore (List.fold_left (fun addr operand ->
      match operand with
      | Instruction.Label label -> 
        emit_value W16 addr @@ CUnit.lookup_symbol unit label;
        addr + Utils.word_size
      | Instruction.W8 hword -> emit_value W8 addr hword; addr + Utils.hword_size
      | Instruction.W16 word -> emit_value W16 addr word; addr + Utils.word_size
    ) 
    (start_addr + Instruction.opcode_size) instr.opnds)
  in

  CUnit.foreach_block unit (fun block ->
    let current_addr = ref @@ Option.get block.start_addr in
    Block.foreach_elem block (fun elem ->
        match elem with
        | Meta _ -> () (* Simply ignore *)
        | Instruction instr -> 
          emit_instruction !current_addr instr;
          current_addr := !current_addr + Instruction.size instr
    )
  )


(** Create ucorn image by LLIR *)
let emit_image: ir_transformer = fun input ->
  let image = Bytes.make Platform.image_size '\000' in
  match input with
  | LLIR ir -> 
    emit_cunit image ir;
    Image image
  | _ -> invalid_ir_kind "LLIR" "Image"