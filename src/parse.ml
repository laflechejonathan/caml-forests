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

let validate_data_set class_instances = 
    let attributes = List.map (fun x -> x.attributes) class_instances in

    let is_insane_discrete group = 
        match List.hd (List.hd group) with 
            | Discrete _ -> if (List.length group) > 8 then printf "WARNING - Crazy Discrete Detected. Shit might hit the fan.\n"
            | _ -> () in

    let validate_attribute index =
        let attributes_at_index = List.map (fun x -> List.nth x index) attributes in
        match List.hd attributes_at_index with
            | Discrete _ -> 
                let groups = List.group compare attributes_at_index in
                is_insane_discrete groups; ()
            | _ -> () in

    let range = List.range 0 `To ((List.length (List.hd attributes) ) - 1) in
    List.map validate_attribute range; ();;

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
   validate_data_set class_instances;
   class_instances;;


