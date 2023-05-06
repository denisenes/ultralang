open Interpreter
let _main = 
  let stream = { buffer=[]; line_num=0; in_chan=stdin} in
  repl stream 