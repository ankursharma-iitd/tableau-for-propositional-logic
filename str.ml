open Printf;;
open Arg;;

module PropLogicTableau: Sig.PropLogicTableau =
struct
    exception Atom_exception
    type prop = 
    | ATOM of string 
    | NOT of prop 
    | AND of prop * prop 
    | OR of prop * prop 
    | COND of prop * prop 
    | BIC of prop * prop
    type argument = prop list * prop
    type leaf = X of prop list | O of prop list;;   
    
    type binary_tree =
    | Leaf of leaf
    | One of (prop list) * binary_tree
    | Two of (prop list) * binary_tree * binary_tree ;;

    type indexed_binary_tree =
    | Leaf of int * leaf
    | One of int * (prop list) * indexed_binary_tree
    | Two of int * (prop list) * indexed_binary_tree * indexed_binary_tree ;;

    (* method to check if the list of propositions is a literal set or not *)
    let rec check_literal_set(x : prop list) : bool = 
        match x with
        |[] -> true
        |ATOM(_) ::  xs -> check_literal_set(xs)
        |NOT(ATOM(_)) :: xs -> check_literal_set(xs)
        |_ -> false

    (* method to check whether a complimentary set of literals exist in the literal set or not*)
    let rec check_complimentary_pair(x : prop list) : bool =
        match x with
        |[] -> false;
        |ATOM(x) :: xs -> if(List.mem (NOT(ATOM(x))) (xs)) then true else check_complimentary_pair(xs);
        |NOT(ATOM(x)) :: xs -> if(List.mem (ATOM(x)) (xs)) then true else check_complimentary_pair(xs);
        |_ :: xs -> check_complimentary_pair(xs);;

    (* convert a proposition to a string *)
    let rec prop_to_string(x : prop) : string = 
        match x with
        |ATOM(y) -> y;
        |NOT(y) -> "(NOT " ^ prop_to_string(y) ^ ")";
        |AND(y1, y2) -> "(" ^ prop_to_string(y1) ^ " AND " ^ prop_to_string(y2) ^ ")";
        |OR(y1, y2) -> "(" ^ prop_to_string(y1) ^ " OR " ^ prop_to_string(y2) ^ ")";
        |COND(y1, y2) -> "(" ^ prop_to_string(y1) ^ " COND " ^ prop_to_string(y2) ^ ")";
        |BIC(y1, y2) -> "(" ^ prop_to_string(y1) ^ " BIC " ^ prop_to_string(y2) ^ ")";;
    
    (* convert a list of propositions to a string *)
    let rec prop_list_to_string(x : prop list) : string = 
        match x with
        |[] -> "";
        |x :: xs -> prop_to_string(x) ^ "; " ^ prop_list_to_string(xs);;

    (* print the tree - for debugging purposes *)
    let rec print_the_tree(t : binary_tree) : string = 
        match t with
        |One(x, t) -> "One([" ^ prop_list_to_string(x) ^ "]," ^ print_the_tree(t) ^ ")";
        |Two(x, t1, t2) -> "Two([" ^ prop_list_to_string(x) ^ "]," ^ print_the_tree(t1) ^ "," ^ print_the_tree(t2) ^ ")";
        |Leaf(O(x)) -> "Leaf(O[" ^ prop_list_to_string(x) ^ "])";
        |Leaf(X(x)) -> "Leaf(X[" ^ prop_list_to_string(x) ^ "])";;

    (* print the tree after indexing it - for debugging purposes *)
    let rec print_the_indexed_tree(t : indexed_binary_tree) : string = 
        match t with
        |One(index, x, t) -> string_of_int(index) ^ "_One([" ^ prop_list_to_string(x) ^ "]," ^ print_the_indexed_tree(t) ^ ")";
        |Two(index, x, t1, t2) -> string_of_int(index) ^ "_Two([" ^ prop_list_to_string(x) ^ "]," ^ print_the_indexed_tree(t1) ^ "," ^ print_the_indexed_tree(t2) ^ ")";
        |Leaf(index, O(x)) -> string_of_int(index) ^ "_Leaf(O[" ^ prop_list_to_string(x) ^ "])";
        |Leaf(index, X(x)) -> string_of_int(index) ^ "_Leaf(X[" ^ prop_list_to_string(x) ^ "])";;

    (* used for making the tree *)
    let rec make_tree(x : prop list) : binary_tree = 
        if check_literal_set(x) then 
            if check_complimentary_pair(x) then (Leaf(X(x))) else (Leaf(O(x)))
        else match x with
            |ATOM(x1) :: xs -> make_tree(xs @ [ATOM(x1)])
            |NOT(ATOM(x1)) :: xs -> make_tree(xs @ [NOT(ATOM(x1))])
            |AND(x1, x2) :: xs -> One(x, make_tree(xs @ [x1; x2]))
            |NOT(OR(x1, x2)) :: xs -> One(x, make_tree(xs @ [NOT(x1); NOT(x2)]))
            |NOT(COND(x1, x2)) :: xs -> One(x, make_tree(xs @ [x1; NOT(x2)]))
            |OR(x1, x2) :: xs -> Two(x, make_tree(xs @ [x1]), make_tree(xs @ [x2]))
            |COND(x1, x2) :: xs -> Two(x, make_tree(xs @ [NOT(x1)]), make_tree(xs @ [x2]))
            |BIC(x1, x2) :: xs -> Two(x, make_tree(xs @ [AND(x1, x2)]), make_tree(xs @ [AND(NOT(x1), NOT(x2))]))
            |NOT(BIC(x1, x2)) :: xs -> Two(x, make_tree(xs @ [AND(x1, NOT(x2))]), make_tree(xs @ [AND(NOT(x1),x2)]))
            |NOT(AND(x1, x2)) :: xs -> Two(x, make_tree(xs @ [NOT(x1)]), make_tree(xs @ [NOT(x2)]))
            |NOT(NOT(x1)) :: xs -> One(x, make_tree(xs @ [x1]));;

    let counter = ref 0;;

    (* label each node of the tree to create a new tree of indexed tree struxcture *)
    let rec label_the_tree(tree : binary_tree) : indexed_binary_tree = 
        match tree with
        |One(x, t) -> incr counter; let value = !counter in One(value, x, label_the_tree(t));
        |Two(x, t1, t2) -> incr counter; let value = !counter in Two(value, x, label_the_tree(t1), label_the_tree(t2));
        |Leaf(O(x)) -> incr counter; let value = !counter in Leaf(value, O(x));
        |Leaf(X(x)) -> incr counter; let value = !counter in Leaf(value, X(x));;

    (*Given the indexed tree, this method will populate the hash table with each node assigned a unique integer*)
    let rec put_values(hash, tree) = 
        match tree with
        |One(index1, x, One(index2, y, z)) -> ((Hashtbl.add hash index1 ((x), [index2])); (put_values(hash, One(index2,y, z))));
        |One(index1, x, Two(index2, y, z, w)) -> (Hashtbl.add hash index1 ((x), [index2])); put_values(hash, Two(index2, y, z, w));
        |One(index1, x, Leaf(index2, O(y))) -> (Hashtbl.add hash index1 ((x), [index2])); put_values(hash, Leaf(index2, O(y)));
        |One(index1, x, Leaf(index2, X(y))) -> (Hashtbl.add hash index1 ((x), [index2])); put_values(hash, Leaf(index2, X(y)));
        |Two(index1, x, One(index2, y, z), One(index3, w, o)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, One(index2, y, z)) in (put_values(hash1, One(index3, w, o)));
        |Two(index1, x, One(index2, y, z), Two(index3, w, o, s)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, One(index2, y, z)) in (put_values(hash1, Two(index3, w, o, s)));
        |Two(index1, x, One(index2, y, z), Leaf(index3, O(w))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, One(index2, y, z)) in put_values(hash1, Leaf(index3, O(w)));
        |Two(index1, x, One(index2, y, z), Leaf(index3, X(w))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, One(index2, y, z)) in put_values(hash1, Leaf(index3, X(w)));
        |Two(index1, x, Two(index3, w, o, s), One(index2, y, z)) -> (Hashtbl.add hash index1 ((x), [index3; index2])); let hash1 = put_values(hash, One(index2, y, z)) in (put_values(hash1, Two(index3, w, o, s)));
        |Two(index1, x, Two(index2, a, b, c), Two(index3, d, e, f)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Two(index2, a, b, c)) in (put_values(hash1, Two(index3, d, e, f)));
        |Two(index1, x, Two(index2, a, b, c), Leaf(index3, O(w))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Two(index2, a, b, c)) in put_values(hash1, Leaf(index3, O(w)));
        |Two(index1, x, Two(index2, a, b, c), Leaf(index3, X(w))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Two(index2, a, b, c)) in put_values(hash1, Leaf(index3, X(w)));
        |Two(index1, x, Leaf(index2, O(y)), One(index3, z, w)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, One(index3, z, w)) in put_values(hash1, Leaf(index2, O(y)));
        |Two(index1, x, Leaf(index2, O(y)), Two(index3, z, w, s)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Two(index3, z, w, s)) in put_values(hash1, Leaf(index2, O(y)));
        |Two(index1, x, Leaf(index2, O(y)), Leaf(index3, O(z))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Leaf(index2, O(y))) in put_values(hash1, Leaf(index3, O(z)));
        |Two(index1, x, Leaf(index2, O(y)), Leaf(index3, X(z))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Leaf(index2, O(y))) in put_values(hash1, Leaf(index3, X(z)));
        |Two(index1, x, Leaf(index2, X(y)), One(index3, z, w)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, One(index3, z, w)) in put_values(hash1, Leaf(index2, X(y)));
        |Two(index1, x, Leaf(index2, X(y)), Two(index3, z, w, s)) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Two(index3, z, w, s)) in put_values(hash1, Leaf(index2, X(y)));
        |Two(index1, x, Leaf(index2, X(y)), Leaf(index3, O(z))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Leaf(index2, X(y))) in put_values(hash1, Leaf(index3, O(z)));
        |Two(index1, x, Leaf(index2, X(y)), Leaf(index3, X(z))) -> (Hashtbl.add hash index1 ((x), [index2; index3])); let hash1 = put_values(hash, Leaf(index2, X(y))) in put_values(hash1, Leaf(index3, X(z)));
        |Leaf(index, O(x)) -> (Hashtbl.add hash index ((x), [123456])); hash;
        |Leaf(index, X(x)) -> (Hashtbl.add hash index ((x), [-123456])); hash;;

    (* convert the hashtable to a list of key-value pairs *)
    let condense_hashtable(hash) = Hashtbl.fold (fun k v acc -> (k, v) :: acc) hash [];;

    (* save the string to a file *)
    let save(file, string) =
        let channel = open_out file in
        (output_string(channel) string);
        close_out channel;;

    (* hardcode the first part of the dot file *)
    let rec make_string_first(key_value_list): string =
        match key_value_list with
        |[] -> "";
        |x :: xs -> ("\t" ^ string_of_int(fst x) ^ " [ label = \"" ^ prop_list_to_string(fst (snd x)) ^ "\" ];\n") ^ make_string_first(xs);;

    (* create the last part of the dot file *)
    let rec make_list(x : int list) : string = 
        match x with
        |[] -> ""
        |[x] -> string_of_int(x);
        |x :: xs -> string_of_int(x) ^ " " ^ make_list(xs);;

    let rec make_string_last(key_value_list): string = 
        match key_value_list with
        |[] -> "";
        |x :: xs -> ("\t" ^ string_of_int(fst x) ^ " -> {" ^ make_list(snd (snd x)) ^ "};\n") ^ make_string_last(xs);;

    (* Define the file name, and the constant start string *)
    let file_name = "mygraph.dot";;
    let start_message = "digraph BT{\n\tnode [fontname=\"Arial\" ];\n";;

    (* make the graph as per the intructions *)
    let make_graph(key_value_list) = 
        save(file_name, start_message);
        let first_part = make_string_first(key_value_list) in
            let last_part = make_string_last(key_value_list) in
                save(file_name, start_message ^ first_part ^ last_part ^ "}\n");;

    (* answer the validity question *)
    let rec answer_validity(key_value_list) =
        match key_value_list with
        |[] -> true;
        |(_, (_, [123456])) :: xs -> false;
        |x :: xs -> answer_validity(xs);;

    (* method to remove duplicates from the list *)
    let remove_duplicates_using_a_hashtable l =
        let open List in
        let tbl = Hashtbl.create (length l) in
        let f l e = 
            try 
            let _ = Hashtbl.find tbl e in l
            with
            | Not_found -> 
            Hashtbl.add tbl e ();
            e::l
        in
        List.rev (List.fold_left f [] l);;

    (* get the truth assignment of a proposition in the form of string *)
    let get_assigment_per_prop(x : prop) : string = 
        match x with
        |ATOM(y) -> y ^ " <= True";
        |NOT(ATOM(y)) -> y ^ " <= False";
        |_ -> "";;

    (* get the falsigying truth assignment of a literal set *)
    let rec get_assignments(x : prop list) = 
        match x with
        |[] -> ""
        |t :: ts -> get_assigment_per_prop(t) ^ "; " ^ get_assignments(ts);;

    (* method used for displaying all the assignments *)
    let rec display_all_assignments(key_value_list) =
        match key_value_list with
        |[] -> "\n";
        |(_, (x, [123456])) :: xs -> get_assignments(remove_duplicates_using_a_hashtable(x)) ^ "\n" ^ display_all_assignments(xs);
        |x :: xs -> display_all_assignments(xs);;

    (* this method is used for solving the tableau *)
    let solve(tree : binary_tree) : unit = 
        let indexed_tree = label_the_tree(tree) in
            let hash_table = Hashtbl.create 123456 in
                let final_hash_table = put_values(hash_table, indexed_tree) in
                    let key_value_list = condense_hashtable(final_hash_table) in
                            if(answer_validity(key_value_list)) then (print_string("\nThe Argument is Valid. \n"))
                            else (print_string("\nThe Argument is Not Valid.\n\nFalsifying Assignments are : \n"); print_string(display_all_assignments(key_value_list)));
                            make_graph(key_value_list);
                            print_string("graph.dot has been created successfully\n\n");;

    (* first method called before running the tableau *)
    let tableau(x : prop list) : unit =
        let tree = make_tree(x) in solve(tree);;
end;;

(* ------------------------------------------------------------------ RUNNING THE TABLEAU ------------------------------------------------------------------ *)
let rec convert_prop(x : Arg.prop) : PropLogicTableau.prop =
    match x with
    |ATOM(y) -> ATOM(y);
    |NOT(y) -> NOT(convert_prop(y));
    |AND(y1, y2) -> AND(convert_prop(y1), convert_prop(y2));
    |OR(y1, y2) -> OR(convert_prop(y1), convert_prop(y2));
    |COND(y1, y2) -> COND(convert_prop(y1), convert_prop(y2));
    |BIC(y1, y2) -> BIC(convert_prop(y1), convert_prop(y2));;

let rec convert_prop_list(x : Arg.prop list) : PropLogicTableau.prop list =
    match x with
    |[] -> []
    |x :: xs -> convert_prop(x) :: convert_prop_list(xs);; 

let arg : PropLogicTableau.argument = (convert_prop_list(fst Arg.arg), convert_prop(snd Arg.arg));;

let rec join_using_AND(x : PropLogicTableau.prop list) : PropLogicTableau.prop = 
    match x with
    |[] -> ATOM("Exception");
    |[x1] -> x1;
    |x :: xs -> AND(x, join_using_AND(xs));;

let create_prop_list(arg : PropLogicTableau.argument) : PropLogicTableau.prop = AND(join_using_AND(fst arg), NOT(snd arg));;

let temp : PropLogicTableau.prop = create_prop_list(arg);;

PropLogicTableau.tableau [temp]

(* let p : PropLogicTableau.prop = ATOM("P")
let q : PropLogicTableau.prop  = ATOM("Q")
let r : PropLogicTableau.prop = ATOM("R")
let temp1 : PropLogicTableau.prop = AND((OR(AND(p, q),AND(NOT(p), r))), (NOT(AND(OR(NOT(p), q),OR(p, r)))));;
let temp2 : PropLogicTableau.prop = AND(OR(p, q), AND(NOT(p), NOT(q)));;
PropLogicTableau.tableau [temp1] *)