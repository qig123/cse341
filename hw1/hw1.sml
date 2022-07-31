fun is_older (a:int*int*int, b:int*int*int)=
    let val ay = (#1 a) val am = (#2 a) val ad=(#3 a) val by=(#1 b) val bm=(#2 b) val bd=(#3 b)
    in
      if ay < by
      then true
      else if ay = by andalso am < bm
      then true
      else if ay = by andalso am = bm  andalso ad < bd
      then true
      else false
    end
fun number_in_month(xs: (int*int*int) list, month:int)=
    if null xs orelse month<=0
    then 0
    else
        let val tl_ans=number_in_month( (tl xs), month)
        in
            if (#2 (hd xs))=month
            then 1+tl_ans
            else tl_ans    
        end
fun number_in_months(xs: (int*int*int) list,ms:int list)=
    if null xs orelse  null ms
    then 0
    else number_in_month(xs,(hd ms))+number_in_months(xs,(tl ms))
fun dates_in_month(xs:(int*int*int)list,month:int)=
    if null xs orelse month<=0
    then []
    else
        let val tl_ans=dates_in_month( (tl xs), month)
        in
            if (#2 (hd xs))=month
            then (hd xs)::tl_ans
            else tl_ans    
        end
fun dates_in_months(xs: (int*int*int) list,ms:int list)=
    if null xs orelse null ms
    then []
    else dates_in_month(xs,(hd ms))@dates_in_months(xs,(tl ms))
fun get_nth(xs:string list, n:int)=
    if  n = 1
    then hd xs
    else get_nth(tl xs, n-1)
fun date_to_string(t:int*int*int)=
    let
      val xs =["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
      get_nth(xs,(#2 t))^" "^Int.toString(#3 t)^", "^Int.toString(#1 t)
    end
fun number_before_reaching_sum(sum:int,xs:int list)= 
    if sum <= 0
    then ~1
    else 1+number_before_reaching_sum(sum- (hd xs), tl xs)
fun what_month(day:int)=
    number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31])+1
fun month_range(day1:int,day2:int)=
    if day1 > day2 
    then []
    else what_month(day1)::month_range(day1+1,day2)
fun oldest(xs: (int*int*int) list)=
    if null xs
    then NONE
    else
        let
          val tl_ans= oldest(tl xs)
        in
          if isSome tl_ans andalso is_older(valOf tl_ans,hd xs)
          then tl_ans
          else SOME (hd xs)
        end
fun number_in_months_challenge(xs: (int*int*int) list,ms:int list)=
    if null xs orelse  null ms
    then 0
    else 
        let
            fun remove_one(m:int,xs:int list)=
            if null xs
            then []
            else
            let val helper=remove_one(m,tl xs)
            in
                if   m = (hd xs)
                then helper
                else (hd xs)::helper
            end
            fun remove(xs:int list)=
                if null xs
                then []
                else  (hd xs)::remove(remove_one(hd xs,tl xs))
            val tl_r = remove(ms);
        in
           number_in_months(xs,tl_r)
        end
fun dates_in_months_challenge(xs: (int*int*int) list,ms:int list)=
    if null xs orelse  null ms
    then []
    else 
        let
            fun remove_one(m:int,xs:int list)=
            if null xs
            then []
            else
            let val helper=remove_one(m,tl xs)
            in
                if   m = (hd xs)
                then helper
                else (hd xs)::helper
            end
            fun remove(xs:int list)=
                if null xs
                then []
                else  (hd xs)::remove(remove_one(hd xs,tl xs))
            val tl_r = remove(ms);
        in
           dates_in_month(xs,(hd tl_r))@dates_in_months(xs,(tl tl_r))
        end
fun reasonable_date(date:int*int*int)=
    let val year= (#1 date)
        val month= (#2 date)
        val day= (#3 date)
        val is_day31= (day=1 orelse day=3 orelse day =5 orelse day =7 orelse day= 10 orelse day =12)
        fun is_leap()=
            if  (year mod 400 = 0) 
            then true
            else if (year mod 4 = 0) andalso  (year mod 100 <> 0)
            then true 
            else false
        fun is_year_month_legal()=
            if (year > 0)  andalso  (month >0 andalso month <=12)
            then true
            else false
        fun is_month2_day_legal()=
            if is_leap() andalso (day>0 andalso day<=29)
            then true
            else if not (is_leap()) andalso (day>0 andalso day<=28)
            then true
            else false
        (*一年中阳历的一，三，五，七，八，十，十二月都是31天的，除二月的其它月均为30天，二月正常情况下是28天*)
        fun is_month_not2_day_legal()=
            if is_day31 andalso (day>0 andalso day <= 31)
            then true
            else if (not is_day31 ) andalso day<>2 andalso (day>0 andalso day <= 31)
            then true
            else false
    in
        if is_year_month_legal() andalso is_month_not2_day_legal() andalso is_month2_day_legal()
        then true
        else false
    end








      













    

