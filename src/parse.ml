open Batteries
open Types
open Printf

(* 
 *
 * This module parses a CSV file corresponding to training data
 * for a machine learning task.  It is incredibly strict and
 * poorly written:
 *
 *  - All nodes MUST be floats - there is no error handling.
 *  - The first item in each line MUST be the class ID.
 *  - All lines MUST have the same number of entries.
 *  - There is NO SUPPORT for fancy localized float formats.
 *  - Generally, if anything is weird, this will throw up.
 *  
 *  TODO:
 *  
 *  - Loosen restrictions above
 *  - support for other formats? XML, JSON, ARFF
 *
 *
 *)

let attribute_of_string str =
    try
        Continuous (float_of_string str)
    with Failure _ -> Discrete str;;


let should_data_be_discrete class_instances =
    let attributes = List.map (fun x -> x.attributes) class_instances in

    let should_be_discrete index =
        let attributes_at_index = List.map (fun x -> List.nth x index) attributes in
        let groups = List.group compare attributes_at_index in
        let result = List.length groups < 8 in
        (* printf "Data @ %d should be discrete: %b\n" index result; *)
        result in

    let range = List.range 0 `To ((List.length (List.hd attributes) ) - 1) in
    List.map should_be_discrete range;;


(* detect numeric arguments that should be discrete (less than 8 distinct values) *)
let transform_discretes set =
    let should_be_discrete = should_data_be_discrete set in
    let is_discrete n =
        List.nth should_be_discrete n in


    let transform_class_instance class_instance = 
        let rec transform_attributes old_attributes new_attributes index =
            match old_attributes with
                | [] -> List.rev new_attributes
                | hd :: tl ->
                        let new_value =
                            match hd with
                                | Continuous (value) -> if (is_discrete index) then (Discrete (string_of_float value))else (Continuous value)
                                | Discrete (value) -> if (is_discrete index) then (Discrete value) else ( (*printf "WARNING - crazy discrete encountered.\n";*) Discrete value) in
                        transform_attributes tl (new_value :: new_attributes) (index + 1) in
        let attributes = transform_attributes class_instance.attributes [] 0 in
        { class_id = class_instance.class_id; attributes = attributes } in
    
    List.map transform_class_instance set 

(* Naive parsing for CSV file 
    Takes a filename, and outputs a list
    of 'class_instance' records.
*)
let rec parse_file filename =
    let comma = Str.regexp "," in

    let rec parse_line input_chan lines = 
        try 
            let str_list = Str.split_delim comma (input_line input_chan) in
            let class_id = List.hd str_list in
            let attributes = List.map attribute_of_string (List.tl str_list) in
            let line =
                { class_id = class_id;
                  attributes = attributes;
                } in
            parse_line input_chan (line :: lines);
        with End_of_file ->
            close_in input_chan;                  (* close the input channel *) 
            lines in
   let input_chan = open_in filename in
   let class_instances = parse_line input_chan [] in
   transform_discretes class_instances;;

