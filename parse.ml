open Printf
open Str
open List

(* Type definition for one entry in the training data *)
type class_instance = 
    {
        class_id : float;
        attributes : float list; 
    }

type comparison =
    {
        attribute_index : int;
        threshold : float;
    }

type tree = 
    | Nil
    | Tree of node
    and node =
    {
        cmp : comparison;
        true_child: tree;
        false_child: tree;
        likely_class: float;
    };;

(* Generate list in range x to y *)
let rec range x y =
    if x >= y then [] else x :: range (x + 1) y;;

let rec print_list something =
    match something with
    [] -> ()
    | a::b -> (printf "fuck%s\n") a ; print_list b;;
    printf "\n"

let rec print_float_list something =
    match something with
    [] -> ()
    | a::b -> (printf "fuck%f (float)\n|")  a ; print_float_list b;;
    printf "\n"

(* Naive parsing for CSV file 
    Takes a filename, and outputs a list
    of 'class_instance' records.
*)
let rec parse_file filename =
    let comma = Str.regexp "," in

    let rec parse_line input_chan lines = 
        try 
            let floats = List.map float_of_string (Str.split_delim comma (input_line input_chan)) in
            let line =
                { class_id = List.hd floats;
                  attributes = List.tl floats;
                } in
            parse_line input_chan (line :: lines);
        with End_of_file ->
            print_string "done parsing";
            close_in input_chan;                  (* close the input channel *) 
            lines in
   let input_chan = open_in filename in
   parse_line input_chan [];;

let get_most_popular_class set = 
    (* Get the sorted list of classes *)
    let class_set = List.map (fun x -> x.class_id) set in 
    let sorted_set = List.sort compare class_set in

    let rec most_popular set prev curr_count best_elem best_count =

        match set with
            | [] -> best_elem
            | hd::tl ->
                let new_count = 
                    if hd = prev then curr_count + 1 else 1 in

                let new_winner = 
                    if new_count > best_count then hd else best_elem in

                most_popular tl hd new_count new_winner (max new_count best_count) in

    print_float_list sorted_set;
    let pop = most_popular (List.tl sorted_set) (List.hd sorted_set) 1 (List.hd sorted_set) 1 in
    printf "most popular: %f" pop;
    pop;;


(* Calculate the entropy of a given set, where each
   element belongs to one class C_i, according to this
   equation:
    
    - SUM( Count(C_i) * LOG2(Count(C_i)) ) | for each class C_i

*)
 let rec compute_entropy set =
    (* Get the sorted list of classes *)
    let class_set = List.map (fun x -> x.class_id) set in 
    let sorted_set = List.sort compare class_set in

    
    let class_counts = 
        match sorted_set with 
            | [] -> []
            | hd::tl ->
                  (* This code is mysterious to me *)
                  let acc,x,c = List.fold_left (fun (acc,x,c) y -> if y = x then acc, x ,c+1 else c::acc, y,1) ([],hd,1) tl in
                  c::acc in
    let rec sum_entropy classes acc =
        match classes with
            | [] -> acc
            | hd::tl -> sum_entropy tl ( ~-.(float_of_int hd) *. log (float_of_int hd) ) in
    abs_float (sum_entropy class_counts 0.0);;

(* print a list of attributes *)
let rec print_data lines = 
    (*printf "Entropy: %f\n" (compute_entropy lines);*)
    match lines with
    [] -> ()
    | a::b -> (printf "%f ") a.class_id ; print_string "\n" ; print_data b;;

let split_data_set set comparison =
    List.partition (fun x -> List.nth x.attributes comparison.attribute_index < comparison.threshold) set;;

let compute_entropy_for_cmp set cmp = 
    let l1, l2 = split_data_set set cmp in
    let entropy = abs_float (compute_entropy l1 +. compute_entropy l2) in
    (* printf "attribute %d, entropy: %f\n" cmp.attribute_index entropy; *)
    entropy;;

let find_optimal_split set attribute =
    let attribute_values = List.map (fun x -> List.nth x.attributes attribute) set in
    let min = List.fold_left (fun x y -> if x < y then x else y) (List.hd attribute_values) attribute_values in
    let max = List.fold_left (fun x y -> if x > y then x else y) (List.hd attribute_values) attribute_values in
    let mid = (max -. min) /. 2.0 +. min in
    (* let () = printf "\nmin: %f\n" min in *)
    (* let () = printf "\nmax: %f\n" max in *)

    let is_low_better low high =
        (* let () = printf "low: %f high: %f\n" low high in *)
        let entropy_low = compute_entropy_for_cmp set { threshold = low; attribute_index = attribute } in
        let entropy_high = compute_entropy_for_cmp set { threshold = high; attribute_index = attribute } in

        (* let () = printf "low entropy: %f high entropy: %f\n" entropy_low entropy_high in *)
        entropy_low < entropy_high in

    let rec find_optimal_thresh low high =
        if abs_float(high -. low) < 0.001 then {threshold = (low +. high) /. 2.0; attribute_index = attribute}  (* result accurate enough *)
        else (if (is_low_better low high) 
                    then find_optimal_thresh low (low +. (high -. low) /. 2.0) 
                    else find_optimal_thresh (low +. (high -. low) /. 2.0) high) in
    let cmp = find_optimal_thresh mid max in
    let entropy = compute_entropy_for_cmp set cmp in
    let () = printf "\nThe optimal threshold for attribute %d is %f (entropy: %f)\n" attribute cmp.threshold entropy in
    cmp;;

let find_best_possible_split set =
    let num_attributes = List.length (List.hd set).attributes in
    let attributes = range 0 num_attributes in
    let splits = List.map (fun x -> find_optimal_split set x) attributes in
    List.fold_left (fun x y -> if (compute_entropy_for_cmp set x < compute_entropy_for_cmp set y) then x else y) (List.hd splits) splits;;

let rec build_decision_tree depth set =
    (*let () = print_data set in*)
    let curr_entropy = compute_entropy set in
    if curr_entropy = 0.0 || depth >= 10 || List.length set <= 1 then (print_data set; Nil;) else
        let best_cmp = find_best_possible_split set in
        let true_set, false_set = split_data_set set best_cmp in
        
        printf("Depth %d: Split on attribute %d with threshold: %f\n") depth best_cmp.attribute_index best_cmp.threshold;

        (* build decision tree *)
        let likely_class = get_most_popular_class set in

        (* recurse *)
        let true_child = 
            if (compute_entropy true_set) < curr_entropy then
                build_decision_tree (depth+1) true_set
            else
                Nil in
        let false_child = 
            if (compute_entropy false_set) < curr_entropy then 
                build_decision_tree (depth+1) false_set
            else
                Nil in
    
        Tree { cmp = best_cmp; true_child = true_child; false_child = false_child; likely_class = likely_class } ;;

let rec classify tree_node best_prediction attributes =

    match tree_node with
        | Tree n ->
            let next_node = 
                if (List.nth attributes n.cmp.attribute_index) < n.cmp.threshold then
                    n.true_child
                else
                    n.false_child in
            classify next_node n.likely_class attributes
        | Nil -> best_prediction;;


let train_tree = 
    let training_data = parse_file "wine.data" in
    build_decision_tree 0 training_data;;

let eval_tree tree =
    let rec eval data total correct =
        match data with
            | [] -> float_of_int correct  /. float_of_int total
            | a::b -> 
                    let predict = classify tree 0.0 a.attributes in
                    let correct_count = if predict = a.class_id then correct + 1 else correct in
                    printf "Prediction = %f Real Class = %f\n" a.class_id predict;
                    eval b (total + 1) correct_count in

    eval (parse_file "wine.eval") 0 0;;


let perc = eval_tree train_tree in
printf "Correct Percentage = %f\n" (perc *. 100.0);;
