open Types
open Batteries
open Random

(* Decision Tree Classifier *)

(* The comparison performed at a node in a tree *)
type attribute_comparison = {
    attribute_index: int;
    actual_comparison : attribute;
}

type discrete_cmp_result =
{
    entropy : float;
    value : string;
}


(* binary decision tree *)
type tree = 
    | Nil
    | Tree of node
    and node =
    {
        cmp : attribute_comparison;
        true_child: tree;
        false_child: tree;
        likely_class: string;
    };;

let apply_comparison comparison attribute = 
    match comparison.actual_comparison with
        | Continuous (threshold) -> 
                (match attribute with 
                    | Continuous (value) -> value < threshold
                    | Discrete _ -> raise (Failure "Discrete encountered - expected Continuous"))
        | Discrete (cmp_value) ->
                (match attribute with
                    | Continuous _ -> raise (Failure "Discrete encountered - expected Continuous")
                    | Discrete (value) -> value == cmp_value)


(* split list of classes based on a decison tree comparison *)
let split_data_set set comparison =
    let compare_class_instance cmp instance =
        let attribute = List.nth instance.attributes cmp.attribute_index in
        apply_comparison cmp attribute in

    List.partition (compare_class_instance comparison) set;;
    
(* Given a list of floats, get the most frequent float 
 *
 * TODO This sounds weird - indicates that
 *      class as a float doesn't make much sense..
 *)
let get_most_popular_float set = 
    let groups = List.group compare set in
    let sorted_groups = List.sort (fun x y -> compare (List.length x) (List.length y)) groups in
    List.hd (List.hd (List.rev sorted_groups));;

(* Given a list of class instances, get the most frequent class *)
let get_most_popular_class set = 
    let class_set = List.map (fun x -> x.class_id) set in 
    get_most_popular_float class_set;;

(* 
 * Calculate the entropy of a list of class instances, where 
 * each element belongs to one class C_i, according to this
 * equation:
 *  
 *  SUM( Count(C_i) * LOG2(Count(C_i)) ) | for each class C_i
 *
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

(*
 * Given a comparison and a list of class instances, compute the aggregate
 * entropy for the two sets, which is just the sum of each set's entropy.
 *
 *)
let compute_entropy_for_cmp set cmp = 
    let l1, l2 = split_data_set set cmp in
    let entropy = abs_float (compute_entropy l1 +. compute_entropy l2) in
    entropy;;

(*
 * Given an attribute and a list of class instances, find the comparison
 * threshold that results in the most information gain.
 *
 *)
let find_optimal_split_continuous set attribute =

    let unpack_attr_values x =
        match List.nth x.attributes attribute with
            | Discrete (value) -> raise (Failure "Discrete encountered where it shouldn't be")
            | Continuous (value) -> value in
    let attribute_values = List.map unpack_attr_values set in

    let min = List.fold_left (fun x y -> if x < y then x else y) (List.hd attribute_values) attribute_values in
    let max = List.fold_left (fun x y -> if x > y then x else y) (List.hd attribute_values) attribute_values in
    let mid = (max -. min) /. 2.0 +. min in

    let is_low_better low high =
        let entropy_low = compute_entropy_for_cmp set { actual_comparison = (Continuous low); attribute_index = attribute } in
        let entropy_high = compute_entropy_for_cmp set { actual_comparison = (Continuous high); attribute_index = attribute } in

        entropy_low < entropy_high in

    let rec find_optimal_thresh low high =
        let threshold = (low +. high) /. 2.0 in
        if abs_float(high -. low) < 0.001 then {actual_comparison = (Continuous threshold); attribute_index = attribute}  (* result accurate enough *)
        else (if (is_low_better low high) 
                    then find_optimal_thresh low (low +. (high -. low) /. 2.0) 
                    else find_optimal_thresh (low +. (high -. low) /. 2.0) high) in
    let cmp = find_optimal_thresh mid max in
    cmp;;

let find_optimal_split_discrete set attribute = 

    let unpack_attr_values x =
        match List.nth x.attributes attribute with
            | Discrete (value) -> value
            | Continuous (value) -> raise (Failure "Continuous encountered where it shouldn't be.") in
    let attribute_values = List.map unpack_attr_values set in

    let groups = List.group compare attribute_values in
    let eval_discrete_group group = 
        let value = List.hd group in
        let entropy = compute_entropy_for_cmp set { attribute_index = attribute; actual_comparison = (Discrete value) } in
        {entropy = entropy; value = value} in
        
    let sorted_cmp_results = List.sort (fun x y  -> compare x.entropy y.entropy) (List.map eval_discrete_group groups) in
    let best_cmp_value = (List.hd sorted_cmp_results).value in
    { attribute_index = attribute; actual_comparison = (Discrete best_cmp_value) };;


let find_optimal_split set attribute =
    let sample_instance = List.hd set in 
    let sample_attribute = List.nth sample_instance.attributes attribute in 
    match sample_attribute with
        | Discrete (value) -> find_optimal_split_discrete set attribute
        | Continuous (value) -> find_optimal_split_continuous set attribute;;

(*
 * Given a list of class instances, find the comparison
 * (attribute and threshold) that maximizes information
 * gain.
 *
 *)
let find_best_possible_split set =
    let num_attributes = List.length (List.hd set).attributes in
    let attributes = List.range 0 `To num_attributes in
    let splits = List.map (fun x -> find_optimal_split set x) attributes in
    List.fold_left (fun x y -> if (compute_entropy_for_cmp set x < compute_entropy_for_cmp set y) then x else y) (List.hd splits) splits;;

(* 
 * Given a list of class instances, choose a subset of
 * attributes, and among them, find the comparison that
 * maximizes information gain.
 *
 * TODO - magic number '4' should be a parameter.
 *      - what if dataset has fewer than 4?
 *
 *)
let find_best_possible_split_with_randomness set = 
    let num_attributes = List.length (List.hd set).attributes in
    let attributes = List.range 0 `To (num_attributes - 1) in

    (* Shuffle the attributes and choose 4 to consider *)
    let shuffled = Random.shuffle (List.enum attributes) in
    let attributes_to_consider = Array.to_list (Array.sub shuffled 0 4) in

    let splits = List.map (fun x -> find_optimal_split set x) attributes_to_consider in
    List.fold_left (fun x y -> if (compute_entropy_for_cmp set x < compute_entropy_for_cmp set y) then x else y) (List.hd splits) splits;;



(* classify a class instance using a decision tree *)
let classify tree_node attributes = 
    let rec do_classify tree_node best_prediction attributes =
        match tree_node with
            | Tree n ->
                let next_node = 
                    if apply_comparison n.cmp (List.nth attributes n.cmp.attribute_index) then
                        n.true_child
                    else
                        n.false_child in
                do_classify next_node n.likely_class attributes
            | Nil -> best_prediction in
    do_classify tree_node "no prediction"  attributes;;


(* Train a decison tree for the given list of class instances*)
let train_decision_tree set =
    let rec do_train_decision_tree depth set =
        let curr_entropy = compute_entropy set in
        if curr_entropy = 0.0 || depth >= 8 || List.length set <= 1 then Nil else
            (* Construct decison tree node *) 
            let best_cmp = find_best_possible_split_with_randomness set in
            let true_set, false_set = split_data_set set best_cmp in
            let likely_class = get_most_popular_class set in

            (* recurse *)
            let true_child = 
                if (compute_entropy true_set) < curr_entropy then
                    do_train_decision_tree (depth+1) true_set
                else
                    Nil in
            let false_child = 
                if (compute_entropy false_set) < curr_entropy then 
                    do_train_decision_tree (depth+1) false_set
                else
                    Nil in
            Tree { cmp = best_cmp; true_child = true_child; false_child = false_child; likely_class = likely_class } in
    do_train_decision_tree 0 set;;



