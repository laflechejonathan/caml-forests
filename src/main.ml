open Batteries
open Printf
open Str
open Parse
open Types
open Tree
open Forest

(* This is some disgusting code for generating decision trees and random forest
 * classifiers.
 *
 * TODOs:
 *      - Clean up code (figure out Ocaml style)
 *      - Performance -> has not even been considered yet
 *      - prune trees after creation for pointless children (all same label)
 *      - Introduce more randomness (need to read some papers)
 *      - refactor so tree creation parameters aren't hard-coded:
 *          - max depth
 *          - num attributes per branch
 *          - num trees in forest
 *      - save/load trees from file
 *      - ARFF support
 *      - Add CLI
 *      - better random forest training:
 *          - Train trees with different random data subsets
 *          - Discard poorly performing trees (e.g. train 500 take top 50)
 *      - support for categorical attributes (non linear)
 *          |-> rather than threshold, just boolean comparison, e.g. ==
 *
 *)

let rec print_list something =
    match something with
    [] -> ()
    | a::b -> (printf "fuck%s\n") a ; print_list b;;
    printf "\n"

let rec print_float_list something =
    match something with
    [] -> ()
    | a::b -> printf "%f "  a ; print_float_list b;
    printf "\n";;

let rec print_int_list l =
    match l with 
        | [] -> ()
        | a::b -> printf "%d " a ; print_int_list b;
    printf "\n";;

(* print a list of attributes *)
let rec print_data lines = 
    (*printf "Entropy: %f\n" (compute_entropy lines);*)
    match lines with
    [] -> ()
    | a::b -> (printf "%s ") a.class_id ; print_string "\n" ; print_data b;;

let pick_random some_list =
    List.nth some_list (Random.int (List.length some_list))

let train_tree filename = 
    let training_data = parse_file filename in
    train_decision_tree training_data;;

let eval_tree tree filename=
    let rec eval data total correct =
        match data with
            | [] -> float_of_int correct  /. float_of_int total
            | a::b -> 
                    let predict = classify tree a.attributes in
                    let correct_count = if predict = a.class_id then correct + 1 else correct in
                    printf "Prediction = %s Real Class = %s\n" a.class_id predict;
                    eval b (total + 1) correct_count in
    eval (parse_file filename) 0 0;;

let print_tree tree =
    let o = open_out "tree.dot" in
    fprintf o "digraph Tree {\n\n";
    let rec do_print tree counter = 
        match tree with
            | Nil -> counter
            | Tree n ->
                    let comparison_label =
                        match n.cmp.actual_comparison with
                            | Discrete (cmp_value) -> sprintf "(%d) == %s" n.cmp.attribute_index cmp_value
                            | Continuous (threshold) -> sprintf "(%d) < %f" n.cmp.attribute_index threshold in
                    fprintf o "Node%d [label=\"%s class=%s\"];\n" counter comparison_label n.likely_class;

                    let new_counter = 
                        match n.true_child with
                            | Nil -> counter;
                            | Tree t -> 
                                do_print n.true_child (counter + 1) in
                    if new_counter > counter then fprintf o "Node%d -> Node%d [label=\"T\"]\n" counter (counter + 1);
                    
                    let newer_counter = 
                        match n.false_child with
                            | Nil -> new_counter;
                            | Tree t -> 
                                do_print n.false_child (new_counter + 1) in
                    if newer_counter > new_counter then fprintf o "Node%d -> Node%d [label=\"F\"];\n" counter (new_counter + 1); 

                    newer_counter in
    do_print tree 0;
    fprintf o "}\n";
    close_out o;;

let get_num_classes data_set =
    let classes = List.group (fun x y -> compare (x.class_id) (y.class_id) ) data_set in
    List.length classes;;


let () = Random.self_init () in
let a_tree = train_tree "training.data.full" in
print_tree a_tree;
let perc = eval_tree a_tree "eval.data" in
printf "Correct Percentage (tree) = %f\n" (perc *. 100.0);

let train_forest_files = List.map (fun n -> "training.data" ^ string_of_int(n)) (List.range 0 `To 3) in
let training_data_sets = List.map(fun f -> parse_file f) train_forest_files in
let num_classes = get_num_classes (List.hd training_data_sets) in
let perc = eval_forest (train_forest training_data_sets) (parse_file "eval.data") num_classes in
printf "Correct Percentage (forest) = %f\n" (perc *. 100.0);
