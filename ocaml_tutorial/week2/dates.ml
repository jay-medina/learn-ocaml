(* 

   On planet Shadokus, a year has 5 months, each month has 4 days, each day has 3 hours and each hour has 2 minutes. A calendar date is therefore defined as the record type date of the given prelude. 

   1) A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and its minute index is >= 0 and <= 1.
   
   The start of year 12 would be:

    { year = 12; month = 1; day = 1; hour = 0; minute = 0 }

   The end of year 12 would be:

    { year = 12; month = 5; day = 4; hour = 2; minute = 1 }

   Write a function wellformed : date -> bool which checks that the input date is well formed. 


   2) On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a magical computer that evaluates the infinite lambda-term of time. It is defined by value the_origin_of_time of the given prelude.

   Write a function next : date -> date which computes the date which comes one minute after the input date. 


   3) In this computer, the time is represented by an integer that counts the number of minutes since 1/1/1 0:0 (the origin of time).
    
    Write a function of_int : int -> date that converts such an integer into a date. 
*)

type date = { year : int; 
    month : int; 
    day : int;
    hour : int; 
    minute : int
  } ;;

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 } ;;

let wellformed {year; month; day; hour; minute} =
  year >= 1 && 
  month >= 1 && month <= 5 && 
  day >= 1 && day <= 4 && 
  hour >= 0 && hour <= 2 && 
  minute >= 0 && minute <= 1 ;;

let next {year; month; day; hour; minute} =
  if minute = 0 then {year; month; day; hour; minute = 1} 
  else if hour < 2 then {year; month; day; hour= hour + 1; minute = 0}
  else if day < 4 then {year; month; day=day+1; hour=0; minute=0}
  else if month < 5 then {year; month=month+1; day=1; hour=0; minute=0}
  else {year=year+1; month=1; day=1; hour=0; minute=0} ;;

let of_int minutes =
  let rec find_date rest_of_minutes date =
    if rest_of_minutes <= 0 then date 
    else find_date (rest_of_minutes - 1) (next date)
  in
    find_date minutes the_origin_of_time ;;

let print_bool bool_value = 
  print_endline (string_of_bool bool_value);;

let string_of_date date = 
  "Year: " ^ (string_of_int date.year) ^ 
  ", Month: " ^ (string_of_int date.month) ^ 
  ", Day: " ^ (string_of_int date.day) ^ 
  ", Hour: " ^ (string_of_int date.hour) ^ 
  ", Minute: " ^ (string_of_int date.minute) ;;

let my_date = { year=1; month=5; day=4;hour=2; minute=1};;

print_endline (string_of_date my_date) ;;

print_endline (string_of_date (next my_date)) ;;

(* print_bool (wellformed my_date) ;; *)
