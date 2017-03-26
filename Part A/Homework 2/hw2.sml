(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a *)
fun all_except_option(str, lst) =
  let fun aux([], false, _) = NONE
        | aux([], true, acc) = SOME acc
        | aux(head::tail, meet, acc) =
            if same_string(head, str) then aux(tail, true, acc)
            else aux(tail, meet, acc @ [head])
  in
    aux(lst, false, [])
  end

(* 1b *)
fun get_substitutions1(lst, str) =
  let fun aux([], acc) = acc
        | aux(head::tail, acc) =
            case all_except_option(str, head) of
              NONE => aux(tail, acc)
            | SOME substitutions => aux(tail, acc @ substitutions)
  in
    aux(lst, [])
  end

(* 1c *)
fun get_substitutions2(lst, str) =
  let fun aux([], acc) = acc
        | aux(head::tail, acc) =
            case all_except_option(str, head) of
              NONE => aux(tail, acc)
            | SOME substitutions => aux(tail, acc @ substitutions)
  in
    aux(lst, [])
  end

(* 1d *)
fun similar_names(lst, {first=first, middle=middle, last=last}) =
  let fun aux([], acc) = acc
        | aux(head::tail, acc) =
            aux(tail, acc @ [{first=head, middle=middle, last=last}])
  in
    aux(first::get_substitutions2(lst, first), [])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2a *)
fun card_color(Clubs, _) = Black
  | card_color(Diamonds, _) = Red
  | card_color(Hearts, _) = Red
  | card_color(Spades, _) = Black

(* 2b *)
fun card_value(_, Num i) = i
  | card_value(_, Ace) = 11
  | card_value(_, _) = 10

(* 2c *)
fun remove_card(cs, c, e) =
  let fun aux([], _) = raise e
        | aux(head::tail, acc) =
            if head = c then acc @ tail
            else aux(tail, acc @ [head])
  in
    aux(cs, [])
  end

(* 2d *)
fun all_same_color([]) = true
  | all_same_color(head::[]) = true
  | all_same_color(head::neck::tail) =
      (card_color(head) = card_color(neck)) andalso all_same_color(neck::tail)

(* 2e *)
fun sum_cards(cs) =
  let fun aux([], acc) = acc
        | aux(head::tail, acc) =
            aux(tail, acc + card_value(head))
  in
    aux(cs, 0)
  end

(* 2f *)
fun score(cs, goal) =
  let
    val sum = sum_cards(cs)
    val same_color = all_same_color(cs)
    val preliminary_score =
      if sum > goal then
        3 * (sum - goal)
      else
        goal - sum
  in
    if same_color then preliminary_score div 2
    else preliminary_score
  end


(* 2g *)
fun officiate(cards, moves, goal) =
  let
    fun aux(_, held_cards, []) = score(held_cards, goal)
      | aux(card::cards', held_cards, move::moves') =
        case move of
          Discard c => aux(card::cards', remove_card(held_cards, c, IllegalMove), moves')
        | Draw =>
          if cards = [] then score(held_cards, goal)
          else if sum_cards(held_cards) > goal then score(held_cards, goal)
            else aux(cards', card::held_cards, moves')
  in
    aux(cards, [], moves)
  end
