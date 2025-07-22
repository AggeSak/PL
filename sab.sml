(* Identity function (unchanged) *)
fun id x = x;

fun readInput filename =
  let
    val inFile = TextIO.openIn filename
    val N =
      case TextIO.inputLine inFile of
        SOME line =>
          (case Int.fromString line of
            SOME n => n
          | NONE => raise Fail "Invalid integer in input")
      | NONE => raise Fail "Empty file or invalid input"
    val numbers =
      case TextIO.inputLine inFile of
        SOME line =>
          let
            val parsedNumbers = List.map Int.fromString (String.tokens Char.isSpace line)
          in
            case List.mapPartial id parsedNumbers of
              [] => raise Fail "Invalid input format"
            | nums => nums
          end
      | NONE => raise Fail "Empty file or invalid input"
    val _ = TextIO.closeIn inFile
  in
    (N, numbers)
  end

(* All contiguous subsequences (optimized) *)
fun allContiguousSubsequences (xs: int list) =
  let
    fun exploreSubsequences (i: int, j: int) =
      if i > j then
        []
      else
        List.take (j - i + 1) xs (* Faster subsequence extraction with slicing *)
        @ exploreSubsequences (i + 1, j)
  in
    exploreSubsequences (0, List.length xs - 1) (* Explore from all starting positions *)
  end;

(* Sum of sublists (unchanged) *)
fun sumOfSublists (list_of_lists) =
  map (fn sublist => List.foldl (op +) 0 sublist) list_of_lists;

(* Smallest absolute difference (optimized) *)
fun smallestAbsDiff (list : int list, target : int) =
  let
    fun helper (remaining_list : int list, current_min : int) =
      if null remaining_list then
        current_min
      else
        let
          val current_element = hd remaining_list
          val current_diff = abs(current_element - target)
          val new_min = if current_diff < current_min then current_diff else current_min
        in
          helper (tl remaining_list, new_min)
        end
  in
    helper (list, abs(hd list - target))  (* Initialize min with difference of first element *)
  end;

(* Total sum (unchanged) *)
fun totalSum numbers = List.foldl (op +) 0 numbers;

(* Fair sequence processing *)
fun fairseq filename =
  let
    val (N, numbers) = readInput filename
    val prefixSums = allContiguousSubsequences numbers (* Use optimized allContiguousSubsequences *)
    val diffs = sumOfSublists prefixSums
    val total = totalSum numbers
    val diff = smallestAbsDiff (diffs, total)
  in
    print (Int.toString diff ^ "\n")
  end;