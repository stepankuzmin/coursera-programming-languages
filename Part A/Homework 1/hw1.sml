(* check if date1 is older than date2  *)
fun is_older(date1 : (int * int * int), date2 : (int * int * int)) =
  let
    val year1 = #1 date1
    val month1 = #2 date1
    val day1 = #3 date1
    val year2 = #1 date2
    val month2 = #2 date2
    val day2 = #3 date2
  in
    (year1 < year2) orelse
    (year1 = year2 andalso month1 < month2) orelse
    (year1 = year2 andalso month1 = month2 andalso day1 < day2)
  end

(* returns number dates in month *)
fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates then
    0
  else if month = #2 (hd dates) then
    1 + number_in_month(tl dates, month)
  else
    number_in_month(tl dates, month)

(* returns number dates in months *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months then
    0
  else
    number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* filter dates by month *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates then
    []
  else if month = #2 (hd dates) then
    (hd dates)::dates_in_month(tl dates, month)
  else
    dates_in_month(tl dates, month)

(* filter dates by months *)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months then
    []
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* get nth string in list *)
fun get_nth(strings : string list, n : int) =
  let
    fun get_nth_index(strings : string list, current_index : int) =
      if current_index = n then
        hd strings
      else
        get_nth_index(tl strings, current_index + 1)
  in
    get_nth_index(strings, 1)
  end

(* convert date to string *)
fun date_to_string(date : (int * int * int)) =
  let
    val months = ["January", "February", "March", "April", "May",
      "June", "July", "August", "September", "October", "November", "December"]
    val month = get_nth(months, #2 date)
  in
    month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, numbers : int list) =
  let
    fun calculate(numbers : int list, previous_sum : int, current_index : int) =
      let
        val current_sum = previous_sum + (hd numbers)
      in
        if current_sum >= sum then
          current_index
        else
          calculate(tl numbers, current_sum, current_index + 1)
      end
  in
    calculate(numbers, 0, 0)
  end

fun what_month(day : int) =
  let
    val dayCounts = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, dayCounts) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2 then []
  else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  let
    fun get_oldest(oldest_date : (int * int * int), dates : (int * int * int) list) =
      if null dates then
        oldest_date
      else
        let
          val current_date = hd dates
        in
          if is_older(current_date, oldest_date) then
            get_oldest(current_date, tl dates)
          else
            get_oldest(oldest_date, tl dates)
        end
  in
    if null dates then NONE
    else SOME (get_oldest(hd dates, tl dates))
  end

fun contains(elements : int list, element : int) =
  if null elements then false
  else if element = hd elements then true
  else contains(tl elements, element)

fun uniq(elements : int list) =
  if null elements then []
  else
    let
      val element = hd elements
      val elements = tl elements
    in
      if contains(elements, element) then uniq(elements)
      else element::uniq(elements)
    end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
  number_in_months(dates, uniq(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
  dates_in_months(dates, uniq(months))
