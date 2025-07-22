(* Structure to represent a node in the binary tree *)
datatype Tree = Empty | Node of int * Tree * Tree;

(* Function to build the binary tree from a list *)
fun buildTree [] = Empty
  | buildTree (0 :: xs) = Empty
  | buildTree (x :: xs) = Node (x, buildTree xs, buildTree xs);

(* Function to swap the left and right children of a node *)
fun swapNodes Empty = Empty
  | swapNodes (Node (v, left, right)) = Node (v, swapNodes right, swapNodes left);

(* Function to perform in-order traversal of the binary tree *)
fun inOrderTraversal Empty = ()
  | inOrderTraversal (Node (v, left, right)) = (
      inOrderTraversal left;
      print (Int.toString v ^ " ");
      inOrderTraversal right
    );

(* Main function *)
fun main filename =
  let
    val inputFile = TextIO.openIn filename;
    val line = TextIO.inputLine inputFile;
    val numbers = map (fn (SOME x) => x | NONE => raise Fail "Invalid integer") (String.tokens (fn c => c = #" ") line);
    val root = buildTree (map (fn x => case Int.fromString x of SOME n => n | NONE => raise Fail "Invalid integer") numbers);
    val swappedRoot = swapNodes root;
  in
    inOrderTraversal swappedRoot;
    print "\n";
    TextIO.closeIn inputFile
  end;

(* Entry point *)
val _ = main (hd (CommandLine.arguments ()));
