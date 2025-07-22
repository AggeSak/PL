(* Dummy definitions for missing components *)
val n = 5 (* Replace with actual grid size or parameterize it *)
fun validMove ((x, y): int * int) ((dx, dy): int * int): bool = 
    (* Replace with actual logic for determining if a move is valid *)
    x + dx >= 0 andalso x + dx < n andalso y + dy >= 0 andalso y + dy < n

fun toDirection ((dx, dy): int * int): string = 
    (* Replace with actual logic for converting (dx, dy) to a direction string *)
    case (dx, dy) of
        (1, 0) => "down"
      | (~1, 0) => "up"
      | (0, 1) => "right"
      | (0, ~1) => "left"
      | (~1, ~1) => "up-left"
      | (~1, 1) => "up-right"
      | (1, ~1) => "down-left"
      | (1, 1) => "down-right"
      | _ => "unknown"

(* Breadth-first search function *)
fun bfs [] _ = NONE (* No path found *)
  | bfs ((x, y, steps) :: rest) visited =
        if x = n - 1 andalso y = n - 1 then SOME (List.rev steps) (* Found path *)
        else
            let
                val possibleMoves = [(1, 0), (~1, 0), (0, 1), (0, ~1),
                                     (~1, ~1), (~1, 1), (1, ~1), (1, 1)]
                
                fun exploreMove (dx, dy) (queue, visited) =
                    let
                        val newX = x + dx
                        val newY = y + dy
                    in
                        if validMove (x, y) (dx, dy) andalso not (List.exists (fn (nx, ny) => nx = newX andalso ny = newY) visited) then
                            ((newX, newY, toDirection (dx, dy) :: steps) :: queue, (newX, newY) :: visited)
                        else
                            (queue, visited)
                    end
                
                val (newQueue, newVisited) = List.foldl (fn (move, acc) => exploreMove move acc) (rest, visited) possibleMoves
            in
                bfs newQueue newVisited
            end
