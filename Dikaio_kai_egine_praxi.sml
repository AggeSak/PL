fun id x = x

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


fun allSums xs =
    let
        fun sublistsFromIndex (_, []) = []
          | sublistsFromIndex (start, y::ys) =
              let
                  val sublist = start @ [y]
              in
                  sublist :: sublistsFromIndex (sublist, ys)
              end
        
        fun allSublists [] = []
          | allSublists (x::xs) =
              sublistsFromIndex ([x], xs) @ allSublists xs
    in
        [] :: allSublists xs
    end;



fun sumOfSublists (list_of_lists) =
  map (fn sublist => List.foldl (op +) 0 sublist) list_of_lists;

fun smallestAbsDiff (list : int list, target : int) =
  let
    fun helper (remaining_list : int list, current_min : int) =
      if null remaining_list then
        current_min
      else
        let
          val current_element = hd remaining_list
          val current_diff = abs(current_element -( target -current_element))
          val new_min = if current_diff < current_min then current_diff else current_min
        in
          helper (tl remaining_list, new_min)
        end
  in
    helper (list, abs(hd list - (target-hd list)))  (* Initialize min with difference of first element *)
  end;

  fun totalSum numbers = List.foldl (op +) 0 numbers


  fun fairseq filename =
  let
    val (N, numbers) = readInput filename
    val prefixSums = allSums numbers 
    val diffs = sumOfSublists prefixSums
    val total = totalSum numbers
    val diff = smallestAbsDiff (diffs ,total)

  in
    print (Int.toString diff ^ "\n")
  end

