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

fun prefixSum [] _ = []
  | prefixSum (x :: xs) acc = let
      val acc' = acc + x
  in
      acc' :: prefixSum xs acc'
  end

fun totalSum numbers = List.foldl (op +) 0 numbers

fun calculateDifference (i, j, prefixSums, total) =
  let
    val sum_ij = List.nth (prefixSums, j - 1)
    val sum_i = List.nth (prefixSums, i - 1)
  in
    abs(sum_ij - sum_i - (total - sum_ij))
  end

fun findFairSubsequence (numbers, prefixSums) =
  let
    val total = totalSum numbers
    val N = length numbers
    val maxDiff = ref (NONE, 0, 0) : (int option * int * int) ref

    fun updateMaxDiff j =
      let
        val diff = calculateDifference (1, j, prefixSums, total)
      in
        case !maxDiff of
          (NONE, _, _) => maxDiff := (SOME diff, 1, j)
        | (SOME minDiff, _, _) =>
            if diff < minDiff then
              maxDiff := (SOME diff, 1, j)
            else
              ()
      end
  in
    app updateMaxDiff (List.tabulate (N, fn j => j + 1));
    case !maxDiff of
      (NONE, _, _) => raise Fail "No fair subsequence found"
    | (SOME diff, _, _) => abs(diff - 1)  (* Return the absolute value of the difference, adjusting for -1 *)
  end

fun fairseq filename =
  let
    val (N, numbers) = readInput filename
    val prefixSums = prefixSum numbers 0
    val diff = findFairSubsequence (numbers, prefixSums)
  in
    print (Int.toString diff ^ "\n")
  end

