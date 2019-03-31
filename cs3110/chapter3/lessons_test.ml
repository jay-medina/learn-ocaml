open OUnit2
(* open Lessons *)

type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

let next_weekday d = 
   match d with 
   | Mon -> Tue
   | Tue -> Wed
   | Wed -> Thu
   | Thu -> Fri
   | Fri -> Mon
   | Sat -> Mon
   | Sun -> Mon

let make_next_weekday_test name expected_output input= 
  name >:: (fun _ -> assert_equal expected_output (next_weekday input))

let tests = "test suite for next_weekday" >::: [
  make_next_weekday_test "tue_after_mon" Tue Mon;
  make_next_weekday_test "wed_after_tue" Wed Tue;
  make_next_weekday_test "thu_after_wed" Thu Wed;
  make_next_weekday_test "fri_after_thu" Fri Thu;
  make_next_weekday_test "mon_after_fri" Mon Fri;
  make_next_weekday_test "mon_after_sat" Mon Sat;
  make_next_weekday_test "mon_after_sun" Mon Sun;
]


let _ = run_test_tt_main tests
