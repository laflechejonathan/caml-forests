(* Common types use throughout the project *)

(* A machine learning is either a continuous float or an discrete string *)
type attribute = 
    | Continuous of float (* continuous attributes*)
    | Discrete of string (* discrete attributes *);;

(* Type definition for one entry in the training data *)
type class_instance = 
    {
        class_id : string;
        attributes : attribute list; 
    }

