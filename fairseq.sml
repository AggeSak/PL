

(* Your existing functional definitions *)

fun totalSum numbers = List.foldl (op +) 0 numbers

fun calculateDifference (i, j, prefixSums, total) =
  let
    val sum_ij = List.nth (prefixSums, j - 1)
    val sum_i = List.nth (prefixSums, i - 1)
    val remainingSum = total - sum_ij
  in
    abs(sum_ij - (sum_i + remainingSum)) (* Adjust calculation *)
  end

(* Your existing functional definitions *)

fun totalSum numbers = List.foldl (op +) 0 numbers

fun calculateDifference (i, j, prefixSums, total) =
  let
    val sum_ij = List.nth (prefixSums, j - 1)
    val sum_i = List.nth (prefixSums, i - 1)
    val remainingSum = total - sum_ij
  in
    abs(sum_ij - (sum_i + remainingSum)) (* Adjust calculation *)
  end

fun findFairSubsequence (numbers, prefixSums) =
  let
    val total = totalSum numbers
    val N = length numbers
    val maxDiff = ref (NONE, 0, 0) : (int option * int * int) ref

    (* Iterate through all starting indices (i) *)
fun innerLoop i =
  if i <= N then (
    (* Iterate through all ending indices (j) from i to N *)
    for j = i to N do
      let
        val diff = calculateDifference (i, j, prefixSums, total)
      in
        case !maxDiff of
          (NONE, _, _) => maxDiff := (SOME diff, i, j)
          | (SOME minDiff, _, _) =>
            if diff < minDiff then
              maxDiff := (SOME diff, i, j)
            else
              ()
          end
      done;
  )
  else
    raise Fail "Invalid input: i must be less than or equal to N"
  end
