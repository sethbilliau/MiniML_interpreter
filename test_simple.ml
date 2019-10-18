(*
	      CS 51 Really Simple Unit Testing Framework
 *)

(* unit_test test msg -- Returns unit, with side effect of printing a
   report identified by msg on whether the unit test passed (returned
   true) or failed (returned false) *)
let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else
    Printf.printf "%s FAILED\n" msg ;;

(* unit_test_within tolerance value1 value2 msg -- Tests that value1
   is within tolerance of value2. Identifies test using msg. *)
let unit_test_within (tolerance : float)
                     (value1 : float)
                     (value2 : float)
                     (msg : string)
                   : unit =
  unit_test (abs_float (value1 -. value2) < tolerance) msg ;;
