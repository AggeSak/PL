(* Function to read input from a file *)
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

(* Function to calculate prefix sum *)
fun prefixSum [] _ = []
  | prefixSum (x :: xs) acc =
      let
        val acc' = acc + x
      in
        acc' :: prefixSum xs acc'
  end

(* Function to calculate the total sum *)
fun totalSum numbers = List.foldl (op +) 0 numbers

(* Function to calculate the difference between subsequences using prefix sums *)
fun calculateDifference (i, j, prefixSums, total) =
  let
    val sum_ij = List.nth (prefixSums, j - 1)
    val sum_i = List.nth (prefixSums, i - 1)
  in
    abs(sum_ij - sum_i - (total - sum_ij))
  end

(* Function to find the fair subsequence using nested loops *)
fun findFairSubsequence (numbers, prefixSums) =
  let
    val total = totalSum numbers
    val N = length numbers
    val minDiff = ref (NONE, 0, 0) : (int option * int * int) ref  (* Reference to store minimum difference, starting index, and ending index *)

    (* Function to update minimum difference if a smaller difference is found *)
    fun updateMaxDiff (i, j) =
      let
        val diff = calculateDifference (i, j, prefixSums, total)
      in
        case !minDiff of
          (NONE, _, _) => minDiff := (SOME diff, i, j)
          | (SOME minDiffVal, _, _) =>
              if diff < minDiffVal then
                minDiff := (SOME diff, i, j)
              else
                ()
      end
  in
    (* Iterate through all possible starting indices (outer loop) *)
    for i = 1 to N do
      (* Iterate for subsequences starting at the current index (inner loop) *)
      for j = i to N do
        updateMaxDiff (i, j)
      done;
    case !minDiff of
      (NONE, _, _) => raise Fail "No fair subsequence found"
      | (SOME diff, _, _) => abs(diff - 1) (* Return the absolute value of the difference, adjusting for -1 *)
  end

(* Main function to read input, calculate prefix sums, find fair subsequence, and print the difference *)
fun fairseq filename =
  let
    val (N, numbers) = readInput filename
    val prefixSums = prefixSum numbers 0
    val diff = findFairSubsequence (numbers, prefixSums)
  in
    print (Int.toString diff ^ "\n")
  end
