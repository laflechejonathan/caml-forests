open Batteries
open Types
open Tree

(* Random Forests Classifier *)

let train_forest training_data_sets = 
    let rec do_train n forest =
        match n with
            | 0 -> forest
            | n ->  let data_set = Random.choice (List.enum training_data_sets) in
                    do_train (n-1) ((train_decision_tree data_set)::forest) in
    do_train 50 [];;

let classify_with_forest forest attributes = 
    let rec do_classify forest predictions attributes = 
        match forest with 
            | [] -> get_most_popular_float predictions
            | a::b -> do_classify b ((classify a attributes)::predictions) attributes in
    do_classify forest [] attributes;;

let eval_forest forest dataset num_classes =
    let rec eval data total correct =
        match data with
            | [] -> float_of_int correct  /. float_of_int total
            | a::b -> 
                    let predict = classify_with_forest forest a.attributes in
                    let correct_count = if predict = a.class_id then correct + 1 else correct in
                    eval b (total + 1) correct_count in
    eval (dataset) 0 0

