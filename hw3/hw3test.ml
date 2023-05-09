(* function composition operator from lecture *)
let (%) f g x = f (g x)
exception NoAnswer

type pattern =
| WildcardP
| VariableP of string
| UnitP
| ConstantP of int
| ConstructorP of string * pattern
| TupleP of pattern list

type valu =
| Constant of int
| Unit
| Constructor of string * valu
| Tuple of valu list

let rec g f1 f2 p =
  let r = g f1 f2 in
  match p with
  | WildcardP         -> f1 ()
  | VariableP x       -> f2 x
  | ConstructorP(_,p) -> r p
  | TupleP ps         -> List.fold_left (fun i p -> (r p) + i) 0 ps
  | _                 -> 0


(*1*)
let only_lowercase =
  List.filter (fun x-> x.[0]=Char.lowercase_ascii x.[0])
(*2*)
let longest_string1 =
  List.fold_left (fun acc x -> if String.length x > String.length acc then x else acc) ""
(*3*)
let longest_string2 =
  List.fold_left (fun acc x -> if String.length x >= String.length acc then x else acc) ""
(*4*)
let longest_string_helper f =
  List.fold_left (fun acc x -> if  (f (String.length x)  (String.length acc)) then x else acc) ""
let longest_string3 =
  longest_string_helper (fun x y -> x>y)
let longest_string4 =
  longest_string_helper (fun x y -> x>=y)
(* #5 *)
let longest_lowercase =
  longest_string2 % only_lowercase
(* #6 *)
let caps_no_X_string =
  String.concat "" % String.split_on_char 'X' % String.uppercase_ascii
(* #7 *)
let rec first_answer f xs = 
  match xs with
  | [] -> raise NoAnswer
  |x::xs'->
          match f x with
          | Some x -> x
          | None ->first_answer f xs'
let t17 = first_answer ((fun x -> if x = "C" then Some x else None)) ["A";"B";"C"]
(* #8  key point:If it returns None for "Any" element*)
let all_answers f xs = 
  let rec aux acc xs  = 
      match xs with
      | [] -> Some acc
      |x::xs'->  
              match f x with
              | Some x -> aux (acc @ x) xs'
              | None-> None
  in
  aux [] xs
(* #9-b *)
let count_wildcards =
  g (fun (x) -> 1) (fun (x) -> 0)
(* #9-c *)
let count_wild_and_variable_lengths =
  g (fun (x) -> 1)(fun (x) -> String.length x)
(* #9-d *)
let count_a_var s = 
  g (fun (x) -> 0)(fun (x) -> if s = x then 1 else 0)
(* #10 *)
let check_pat pat = 
  let rec aux f p = 
  let r = aux f  in
    match p with
    | VariableP x       -> f x
    | ConstructorP(_,p) ->(r p)
    | TupleP ps         -> List.fold_left (fun i p -> (r p) @ i) [] ps
    | _                 -> [] 
  in
  let strs = aux (fun (x) -> [x]) pat in
  let rec check_repeat xs = 
    match xs with
    | [] -> true
    | x::xs'->  if (List.mem x xs') then false else check_repeat xs' 
  in
    check_repeat strs
(* #11  *)
let rec matches v pat = 
  match (pat,v) with
  | WildcardP,_ -> Some []
  | VariableP s,v-> Some [(s,v)]
  | UnitP,Unit ->  Some[]
  | ConstantP i,Constant j -> if i=j then Some [] else None
  | ConstructorP(s1,p),Constructor(s2,v)-> if s1=s2  then matches v p else None
  | TupleP ps,Tuple vs -> if List.length ps = List.length vs then 
                          let r = List.combine  vs ps in
                          all_answers (fun (x,y) -> matches x y )  r
                          else None
  | _,_-> None
(* #12 *)
let first_match v pats = 
  try Some (first_answer (matches v)   pats) with
  NoAnswer ->   None
(*Test1*)
let t1 = ["aBc"]= only_lowercase ["aBc";"AA";"Bc"]
let t2 = []= only_lowercase ["BBc";"AA";"Bc"]
(*Test2*)
let t2 = "abcde" = longest_string1 ["abcd";"abcde";"a";"gbde"] 
let t3 = ""=longest_string1 [] 
let t4 = "abcde" = longest_string1 ["abcd";"abcde";"a";"gbde";"aaaaa"]
(*Test3*)
let t5 = "abcde" = longest_string2 ["abcd";"abcde";"a";"gbde"] 
let t6 = ""=longest_string2 [] 
let t7 = "aaaaa" = longest_string2 ["abcd";"abcde";"a";"gbde";"aaaaa"] 
(*Test4*)
let t8 = longest_string3 ["abcd";"abcde";"a";"gbde"] = longest_string1 ["abcd";"abcde";"a";"gbde"] 
let t9 = longest_string3 []=longest_string1 [] 
let t10 =longest_string3 ["abcd";"abcde";"a";"gbde";"aaaaa"] = longest_string1 ["abcd";"abcde";"a";"gbde";"aaaaa"]
let t11 = longest_string4 ["abcd";"abcde";"a";"gbde"]  = longest_string2 ["abcd";"abcde";"a";"gbde"] 
let t12 = longest_string4 []=longest_string2 []
let t13 = longest_string4 ["abcd";"abcde";"a";"gbde";"aaaaa"]  = longest_string2 ["abcd";"abcde";"a";"gbde";"aaaaa"]  
(*Test5*)
let t14= "abcde"=longest_lowercase ["abcd";"abcde";"a";"gbde";"Aaaaabbb"]
(*Test6*)
let t15= "ABDD"=caps_no_X_string "aBxXXxDdx"
let t16= "ABTTDD"=caps_no_X_string "aBxxTXtxDdx"
(*Test7*)
let t18=all_answers (fun x -> if x = 1 then Some [x;x+3] else None) [1;2;3;4;5;6;7] 
(*Test8*)
let l1=[2;3;4;5;6;7]
let l2=[2;4;6]
let test80 = all_answers (fun x -> if x = 1 then Some [x] else None) [2;3;4;5;6;7]
	= None
let test81 = all_answers (fun x -> if x = 2 then Some [x] else None) l1
	= None
let test82 = all_answers (fun x -> if x mod 2 = 0 then Some [x] else None) l1
	= None
let test83 = all_answers (fun x -> if x > 0 then Some [x] else None) l1
	= Some(l1)
let test84 = all_answers (fun x -> if x mod 2 = 0 then Some [x] else None) l2
	= Some(l2)
let test801 = all_answers (fun x -> if x = 2 then Some [x] else None) [3;2;4;5;6;7] = None
let test802 = all_answers (fun x -> if x mod 2 = 0 then Some [x] else None) [2;4;5;6;8] = None
let test803 = all_answers (fun x -> if x mod 2 = 0 then Some [x] else None) [2;4;6;8] = Some [2;4;6;8]
let test804 = all_answers (fun x -> if x mod 2 = 0 then Some [x; x + 1] else None) [2;4;6;8] =Some [2;3;4;5;6;7;8;9]
let test805 = all_answers (fun x -> if x mod 2 = 0 then Some [] else None) [2;4;6;8] = Some []
let test806 = all_answers (fun x -> if x mod 2 = 0 then Some [x] else None) [] = Some []
(*Test9*)
let test9a0 = count_wildcards WildcardP = 1
let test9a1 = count_wildcards (TupleP [WildcardP;WildcardP]) = 2
let test9a2 = count_wildcards (TupleP [WildcardP;(VariableP "test")]) = 1
let test9a3 = count_wildcards (TupleP [WildcardP;ConstructorP("c",WildcardP)]) = 2
let test9a4 = count_wildcards (TupleP [UnitP;ConstructorP("c", WildcardP)]) = 1
let test9a5 = count_wildcards (TupleP [ConstantP 7;ConstructorP("c", WildcardP)]) = 1
let test9a01 = count_wildcards (VariableP "str") = 0
let test9a02 = count_wildcards (TupleP [WildcardP; ConstantP 12; WildcardP]) = 2
let test9a03 = count_wildcards (ConstructorP("pattern",(TupleP [WildcardP; ConstantP 12; WildcardP]))) = 2
let test9b0 = count_wild_and_variable_lengths (VariableP("a")) = 1
let test9b1 = count_wild_and_variable_lengths (VariableP("ab")) = 2
let test9b2 = count_wild_and_variable_lengths (TupleP [VariableP("ab");WildcardP]) = 3
let test9b01 = count_wild_and_variable_lengths WildcardP = 1
let test9b02 = count_wild_and_variable_lengths (TupleP [WildcardP; ConstantP 12; WildcardP]) = 2
let test9b03 = count_wild_and_variable_lengths (TupleP [WildcardP; VariableP "str"; WildcardP]) = 5
let test9b04 = count_wild_and_variable_lengths (TupleP [WildcardP; VariableP "str"; WildcardP; VariableP "str2"]) = 9
let test9b05 = count_wild_and_variable_lengths (ConstructorP("pattern",(TupleP [WildcardP; ConstantP 12; WildcardP]))) = 2
let test9b06 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [WildcardP; VariableP "str"; WildcardP]))) = 5
let test9c0 = count_a_var "x"  (VariableP("x")) = 1
let test9c1 = count_a_var "x" (TupleP [VariableP("x")]) = 1
let test9c2 = count_a_var "x" (TupleP [VariableP("x");VariableP("x")]) = 2
let test9c3 = count_a_var "x" (TupleP [VariableP("x");ConstructorP("a",
  VariableP("x"))]) = 2
let test9c01 = count_a_var "x" ((TupleP [WildcardP; ConstantP 12; WildcardP])) = 0
let test9c02 = count_a_var "x" ((TupleP [WildcardP; VariableP "str"; WildcardP])) = 0
let test9c03 = count_a_var "x" ((TupleP [WildcardP; VariableP "x"; WildcardP])) = 1
let test9c04 = count_a_var "x" ((TupleP [WildcardP; VariableP "x"; WildcardP; VariableP "x"])) = 2
let test9c05 = count_a_var "x" ((ConstructorP("pattern",(TupleP [WildcardP; VariableP "x"; WildcardP])))) = 1
let test9c06 = count_a_var "x" ((ConstructorP("x", (TupleP [WildcardP; VariableP "x"; WildcardP])))) = 1
(*Test10*)
let test100 = check_pat (VariableP("x")) = true
let test1001 = check_pat (TupleP [WildcardP; VariableP "x";WildcardP]) = true
let test1002 = check_pat (TupleP [WildcardP; VariableP "x"; VariableP "y"]) = true
let test1003 = check_pat (TupleP [WildcardP; VariableP "x"; VariableP "x"]) = false
let test1004 = check_pat (ConstructorP("x", (TupleP [WildcardP; VariableP "x";  WildcardP]))) = true
let test1005 = check_pat (ConstructorP("x", (TupleP [WildcardP;  VariableP "x";  ConstructorP("y",  VariableP "y")]))) = true
let test1006 = check_pat (ConstructorP("x", (TupleP [WildcardP; VariableP "x"; ConstructorP("y", VariableP "x")]))) = false
let test1007 = check_pat (ConstructorP("x", (TupleP [WildcardP; VariableP "x"; ConstructorP("y", TupleP [VariableP "y"])]))) = true
let test1008 = check_pat (ConstructorP("x", (TupleP [WildcardP; VariableP "x"; ConstructorP("y", TupleP [VariableP "z"])]))) = true
let test1009 = check_pat (ConstructorP("x", (TupleP [WildcardP; VariableP "x"; ConstructorP("y", TupleP [VariableP "x"])]))) = false
let test1010 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [VariableP "x"; VariableP "y"])))) = true
let test1011 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [VariableP "x"; VariableP "x"])))) = false
let test1012 = check_pat (TupleP [WildcardP; VariableP "x"; TupleP [VariableP "y"]]) = true
let test1013 = check_pat (TupleP [VariableP("x");VariableP("x")]) = false
let test1014 = check_pat (TupleP [VariableP("x");VariableP("y")]) = true
let test1015 = check_pat (TupleP []) = true
let test1016 = check_pat (TupleP [ConstructorP("c", VariableP("x"))]) = true
(*Test11*)
let test110 = matches (Constant 1) UnitP = None
let test111 = matches (Constant 1) (ConstantP 2) = None
let test112 = matches (Constant(1)) (ConstantP(1)) = Some []
let test113 = matches (Constant(1))(VariableP("x")) = Some [("x",Constant(1))]
let test1101 = matches (Constant(1))( ConstantP 1) = Some []
let test1102 = matches (Constant(1)) (VariableP "s") = Some [("s", Constant(1))]
let test1103 = matches (Constant(1)) (TupleP [WildcardP]) = None
let test1104 = matches (Constant(1)) (TupleP [ConstantP 1]) = None
let test1105 = matches (Tuple [Unit]) (TupleP [UnitP]) = Some []
let test1106 = matches (Tuple [Tuple [Unit]])( TupleP [TupleP[UnitP]]) = Some []
let test1107 = matches (Tuple [Tuple [Unit]]) (TupleP [TupleP[UnitP; VariableP "x"]]) = None
let test1108 = matches (Tuple [Constant(1); Tuple [Unit]]) (TupleP [ConstantP 1; TupleP[UnitP]]) = Some []
let test1109 = matches (Tuple [Constant(1); Tuple [Unit; Constant(2)]]) (TupleP [ConstantP 1; TupleP[UnitP; VariableP("s")]]) = Some [("s", Constant(2))]
let test1110 = matches (Tuple [Constant(1);Tuple [Unit; Constant(2)]]) (TupleP [ConstantP 2; TupleP[UnitP;VariableP("s")]]) = None
let test1111 = matches (Tuple [Constant(1); Tuple [Unit; Constant(2)]]) (TupleP [ConstantP 1;TupleP[UnitP; VariableP("s"); WildcardP]]) = None
let test1112 = matches (Tuple [Unit]) (TupleP[UnitP;VariableP "x"]) = None
(*Test12*)
let test1200 = first_match Unit [UnitP] = Some []
let test1201 = first_match Unit [VariableP ("s")] = Some [("s", Unit)]
let test1202 = first_match (Tuple [Constant(1); Tuple [Unit; Constant(2)]]) [(TupleP [ConstantP 1; TupleP[UnitP; VariableP("s")]])] = Some [("s", Constant(2))]
let test1203 = first_match (Tuple [Constant(1); Tuple [Unit; Constant(2)]]) [(TupleP [ConstantP 1; TupleP[UnitP; ConstantP 3]])] = None
  