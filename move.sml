
val n = ref 0


fun validMove ((x, y): int * int) ((dx, dy): int * int): bool =
    x + dx >= 0 andalso x + dx < !n andalso
    y + dy >= 0 andalso y + dy < !n


fun toDirection ((dx, dy): int * int): string =
    case (dx, dy) of
        (1, 0) => "S"
      | (~1, 0) => "N"
      | (0, 1) => "E"
      | (0, ~1) => "W"
      | (~1, ~1) => "NW"
      | (~1, 1) => "NE"
      | (1, ~1) => "SW"
      | (1, 1) => "SE"
      | _ => "unknown"

(* Breadth-first search function *)
fun bfs n grid =
    let
        val possibleMoves = [(1, 0), (~1, 0), (0, 1), (0, ~1), (~1, ~1), (~1, 1), (1, ~1), (1, 1)]

        
        fun exploreMove (x, y, steps, cost) (dx, dy) (queue, visited) =
            let
                val newX = x + dx
                val newY = y + dy
                val isValid = validMove (x, y) (dx, dy)
                val notVisited = not (List.exists (fn (nx, ny) => nx = newX andalso ny = newY) visited)
                val newCost = if isValid then List.nth (List.nth (grid, newX), newY) else 0
            in
                if isValid andalso notVisited andalso newCost < cost then
                    ((newX, newY, (toDirection (dx, dy)) :: steps, newCost) :: queue, (newX, newY) :: visited)
                else
                    (queue, visited)
            end
        
        (* Actual BFS function *)
        fun bfs' [] _ = NONE (* No path found *)
          | bfs' ((x, y, steps, cost) :: rest) visited =
                if x = !n - 1 andalso y = !n - 1 then SOME (List.rev steps) (* Found path *)
                else
                    let
                        val (newQueue, newVisited) =
                            List.foldl (fn (move, acc) => exploreMove (x, y, steps, cost) move acc)
                                       (rest, visited) possibleMoves
                    in
                        bfs' newQueue newVisited
                    end
        
        val initialQueue = [(0, 0, [], List.nth (List.nth (grid, 0), 0))]  
        val initialVisited = [(0, 0)]    
    in
        bfs' initialQueue initialVisited
    end

(* Function to read grid from file *)
fun readGridFromFile filename =
    let
        val stream = TextIO.openIn filename
        val nVal =
            case TextIO.inputLine stream of
                NONE => 0
              | SOME line =>
                    (case Int.fromString line of
                         NONE => 0
                       | SOME n => n)
                      
        (* Read grid data *)
        fun readGrid 0 acc = (0, List.rev acc)
          | readGrid k acc =
                case TextIO.inputLine stream of
                    NONE => (k, List.rev acc)
                  | SOME line =>
                        let
                            val tokens = String.tokens (fn c => c = #" ") line
                            val filteredTokens = List.filter (fn s => s <> "") tokens
                            val row = List.map (fn s => case Int.fromString s of
                                                            NONE => 0
                                                          | SOME n => n) filteredTokens
                        in
                            readGrid (k - 1) (row :: acc)
                        end
        val (_, grid) = readGrid nVal []
        val () = TextIO.closeIn stream
    in
        (nVal, grid)
    end


fun moves filename =
    let
        val (nVal, grid) = readGridFromFile filename
        val () = n := nVal  (* Set the value of n dynamically *)
        val result = bfs n grid  (* Pass n as a reference to bfs *)
    in
        case result of
            NONE => print "IMPOSSIBLE\n"
          | SOME path => print ("[" ^ String.concatWith "," path ^ "]\n")
    end
        