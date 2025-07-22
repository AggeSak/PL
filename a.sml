fun readInput filename =
  let
    val inFile = TextIO.openIn filename
    val N =
      (case TextIO.inputLine inFile of
        SOME line =>
          (case Int.fromString line of
            SOME n => n
            | NONE => raise Fail "Invalid integer in input")
        | NONE => raise Fail "Empty file or invalid input")
    val numbers =
      (case TextIO.inputLine inFile of
        SOME line =>
          let
            val parsedNumbers = List.map Int.fromString (String.tokens Char.isSpace line)
          in
            case List.mapPartial id parsedNumbers of
              [] => raise Fail "Invalid input format"
              | nums => nums
          end
        | NONE => raise Fail "Empty file or invalid input")
    val _ = TextIO.closeIn inFile
  in
    (N, numbers)
  end;

(* Function to calculate prefix sum *)
fun prefixSum [] _ = []
  | prefixSum (x :: xs) acc =
    let
      val acc' = acc + x
    in
      acc' :: prefixSum xs acc'
  end;

(* Function to calculate the total sum *)
fun totalSum numbers = List.foldl (op +) 0 numbers;

(* Function to calculate the difference between subsequences using prefix sums *)
fun calculateDifference (i, j, prefixSums, total) =
  let
    val sum_ij = List.nth (prefixSums, j - 1)
    val sum_i = List.nth (prefixSums, i - 1)
  in
    abs(sum_ij -sum_i - (total - sum_ij))
  end;

(* Function to find the fair subsequence recursively *)
fun findFairSubsequence (numbers, prefixSums) =
  let
    val total = totalSum numbers
    (* Helper function for finding the minimum difference recursively *)
    fun findMinDiff (start, currentMin) =
      if start > length numbers then
        currentMin
      else
        let
          (* Calculate the difference for the current subsequence *)
          val currentDiff = calculateDifference (start , length numbers, prefixSums, total)
          (* Update the minimum difference *)
          val newMin = Int.min(currentMin, currentDiff)
          (* Explore starting a new subsequence from the next element *)
          val nextMin = findMinDiff (start + 1, newMin)
        in
          (* Return the minimum difference found *)
          (*print (Int.toString newMin ^ "\n");*)
          newMin
      end
  in
    (* Start the recursive search from the first element *)
    findMinDiff (1, total)  (* Use total as initial minimum difference *)
  end;


(* Main function to read input, calculate prefix sums, find fair subsequence, and print the difference *)
fun fairseq filename =
  let
    val (N, numbers) = readInput filename
    val prefixSums = prefixSum numbers 0
    val minDiff = findFairSubsequence (numbers, prefixSums)
  in
    print (Int.toString minDiff ^ "\n")  (* Print the minimum difference *)
  end;
