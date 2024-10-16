(** Abstract syntax of expressions and toplevel directives. *)

(** We use String for user named variables, Gensym for fresh substitution variables, and Dummy for pretty-printing anonymous functions. *)
type variable =
  | String of string
  | Gensym of string * int
  | Dummy

type expr =
  | Var of variable
  | Universe of int
  | Pi of abstraction
  | Lambda of abstraction
  | App of expr * expr
  | Nat
  | Zero
  | Succ of expr
  | ElimNat of expr * expr * expr * expr
 
(** An abstraction [(x,t,e)] indicates that [x] of type [t] is bound in [e]. *)
and abstraction = variable * expr * expr

type directive =
  | Quit
  | Help
  | Context
  | Parameter of variable * expr
  | Definition of variable * expr
  | Check of expr
  | Eval of expr

(** [fresh x] generates a fresh variable name whose preferred form is [x]. Used for pretty printing. *)
let fresh =
  let k = ref 0 in
    function
      | String x | Gensym (x, _) -> (incr k ; Gensym (x, !k))
      | Dummy -> (incr k ; Gensym ("_", !k))

(** [subst [(x1,e1); ...; (xn;en)] e] performs the given substitution of
    expressions [e1], ..., [en] for variables [x1], ..., [xn] in expression [e]. *)
let rec subst s = function
  | Var x -> (try List.assoc x s with Not_found -> Var x)
  | Universe k -> Universe k
  | Pi a -> Pi (subst_abstraction s a)
  | Lambda a -> Lambda (subst_abstraction s a)
  | App (e1, e2) -> App (subst s e1, subst s e2)
  | Nat -> Nat
  | Zero -> Zero
  | Succ e -> Succ (subst s e)
  | ElimNat (e1, e2, e3, e4) -> ElimNat (subst s e1, subst s e2, subst s e3, subst s e4)

and subst_abstraction s (x, t, e) =
  let x' = fresh x in
    (x', subst s t, subst ((x, Var x') :: s) e)
