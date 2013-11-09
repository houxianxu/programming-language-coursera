(*Homework1
https://class.coursera.org/proglang-002/assignment/view?assignment_id=2*)

(*1*)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
		let
		    val year1 = #1 date1
		    val month1 = #2 date1
		    val day1 = #3 date1
		    val year2 = #1 date2
		    val month2 = #2 date2
		    val day2 = #3 date2
		in
		    (*find all the true case, then use 'orelse' and 'andalso' keyword to include all the cases in one expression*)
		    (year1 < year2) orelse 
		    (year1 = year2 andalso month1 < month2) orelse 
		    (year1 = year2 andalso month1 = month2 andalso day1 < day2)
		end

(*2*)
fun number_in_month (dates : (int*int*int) list, month : int) =
	if null dates 
	then 0
	else 
		let
		    val tl_ans = number_in_month(tl dates, month)
		in
		    if #2 (hd dates) = month then 1 + tl_ans else tl_ans
		end

(*3*)
fun number_in_months (dates : (int*int*int) list, months : int list) =
	if null months then 0 else 
	number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*4*)
fun dates_in_month (dates : (int*int*int) list, month : int) =
	if null dates then []
	else
		let
		    val tl_ans = dates_in_month(tl dates, month)
		    val date = hd dates
		in
		    if #2 date = month then date :: tl_ans else tl_ans
		end

(*5*)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
	if null months then []
	else
		let
		    val month = hd months
		    val tl_ans = dates_in_months(dates, tl months)
		in
		    dates_in_month(dates, month) @ tl_ans
		end

(*6*)
fun get_nth (str_list : string list, n : int) =
	if n = 1 then hd str_list
	else get_nth(tl str_list, n-1)

(*7*)
fun date_to_string (date : (int*int*int)) =
	let
	    val months = ["January", "February", "March", "April", 
	                  "May", "June", "July", "August", 
	                  "September", "October", "November", "December"]
	    val year = #1 date
	    val month = #2 date
	    val day = #3 date
	in
	    get_nth(months, month) ^ " " ^ Int.toString(day) ^ ","  ^ " " ^ Int.toString(year)
	end

(*8*)
fun number_before_reaching_sum (sum : int, number_list : int list) =
	if hd number_list >= sum then 0
	else 1 + number_before_reaching_sum(sum - hd number_list, tl number_list)

	
(*9*)
fun what_month (day_of_year : int) =
	let
	    val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
	    1 + number_before_reaching_sum(day_of_year, days_in_months)
	end

(*10*)
fun month_range (day1 :int, day2 : int) =
	if day1 > day2 then []
	else what_month(day1) :: month_range(day1 + 1, day2)

(*11*)
fun oldest (dates : (int*int*int) list) =
	if null dates then NONE 
	else
		let
		     fun oldest_none_empty (dates : (int*int*int) list) =
		     	if null (tl dates) then hd dates 
		     	else
		     		let
		     		    val tl_ans = oldest_none_empty(tl dates)
		     		in
		     		    if is_older(hd dates, tl_ans)
		     		    then hd dates
		     		    else tl_ans
		     		end
		 in
		     SOME(oldest_none_empty(dates))
		 end			 

(*12*)
(*help function
----------------------------------------------------*)
(*determine if a int is in a int list -> O(n)*)
fun is_exist (num : int, nums : int list) =
	if null nums then false 
	else 
		if hd nums = num
		then true
		else is_exist(num, tl nums)

(* remove duplicates in a int list -> O(n**2)*)
fun remove_duplicates (nums : int list) =
	if null nums 
	then nums
	else 
		if is_exist(hd nums, tl nums)
		then remove_duplicates(tl nums)
		else hd nums :: remove_duplicates(tl nums)
(*help function end
--------------------------------------------------*)



fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
	number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
	dates_in_months (dates, remove_duplicates(months))


(*13*)
fun reasonable_date (date : int*int*int) =
	let
	    val year = #1 date
	    val month = #2 date
	    val day = #3 date
	    val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    val leap_days_in_months =[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	    fun get_nth (str_list : int list, n : int) =
			if n = 1 then hd str_list
			else get_nth(tl str_list, n-1)

	    fun is_reasonable_year (year : int) =
	    	if year > 0 then true else false

	    fun is_leap_year (year : int) =
	    	if year > 0 andalso ((year mod 400 = 0) orelse (year mod 4 = 0) andalso (year mod 100 <> 0))
	    	then true
	    	else false

	    fun is_reasonable_month (month : int) =
	    	if month >= 1 andalso month <=12 then true else	false

	in
		if is_reasonable_year(year) andalso is_reasonable_month(month) andalso day > 0
		then
		    if is_leap_year(year)
		    then
		    	day <= get_nth(leap_days_in_months, month)
		    else
		    	day <= get_nth(days_in_months, month)
		else false

	end


(* additional test by houxianxu*)

(*test 12*)
(*val test12_a = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = 3
val test12_a = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]*)

(*test 13*) 
(*val test13_a = reasonable_date(2004, 2, 29) leap year
val test13_b = reasonable_date(1990, 3, 23)
val test13_c = reasonable_date(2004, 2, 29)*)
