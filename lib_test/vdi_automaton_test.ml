open OUnit

let tests =
  "vdi_automaton" >:::
    [
      "Test VDI automaton" >:: Vdi_automaton.test
    ]
