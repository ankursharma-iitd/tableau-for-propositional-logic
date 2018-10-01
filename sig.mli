module type PropLogicTableau =
sig
    exception Atom_exception
    type prop = 
    ATOM of string 
    | NOT of prop 
    | AND of prop * prop 
    | OR of prop * prop 
    | COND of prop * prop 
    | BIC of prop * prop
    type argument = prop list * prop
    val tableau: prop list -> unit
end;;
