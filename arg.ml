(* Import the Data Types *)
type prop = 
    | ATOM of string 
    | NOT of prop 
    | AND of prop * prop 
    | OR of prop * prop 
    | COND of prop * prop 
    | BIC of prop * prop
type argument = prop list * prop

(* open Sig.PropLogicTableau;; *)

(* Atoms are defined below *)
let perceive = ATOM("P");;
let delusive = ATOM("Q");;
let veridical = ATOM("R");;
let something = ATOM("S");;
let material = ATOM("T");;
let sensation = ATOM("U");;
let differ = ATOM("V");;

(* Hypothesis are defined below *)
let h1 : prop = COND(perceive, NOT(BIC(delusive,veridical)));;
let h2 : prop = COND(delusive,AND(perceive,NOT(material)));; 
let h3 : prop = COND(AND(perceive,NOT (material)),sensation);; 

(* Hypothesis list *)
let hypo_list : prop list = [h1; h2; h3;]

(* Check the validity of the following arguemnt given the hypothesis *)
let c : prop = COND(perceive,AND(sensation,NOT(material)));;

(* Final argument to pass to the Tableau *)
let arg : argument = (hypo_list, c);;