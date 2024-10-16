(** Type inference and normalization. *)

open Ast
open Ctx

(** [normalize ctx e] normalizes the given expression [e] in context [ctx]. It removes
    all redexes and it unfolds all definitions. It performs normalization under binders.  *)
let rec normalize ctx = function
  | Var x ->
    (match
        (try lookup_value x ctx
         with Not_found -> Error.runtime "unkown identifier %t" (Print.variable x))
     with
       | None -> Var x
       | Some e -> normalize ctx e)
  | App (e1, e2) ->
    let e2 = normalize ctx e2 in
      (match normalize ctx e1 with
        | Lambda (x, _, e1') -> normalize ctx (subst [(x,e2)] e1')
        | e1 -> App (e1, e2))
  | Universe k -> Universe k
  | Pi a -> Pi (normalize_abstraction ctx a)
  | Lambda a -> Lambda (normalize_abstraction ctx a)
  | Nat -> Nat
  | Zero -> Zero
  | Succ e -> Succ (normalize ctx e)
  | ElimNat (e1, e2, e3, e4) -> 
    let e1 = normalize ctx e1 in
    let e2 = normalize ctx e2 in
    let e3 = normalize ctx e3 in
    let e4 = normalize ctx e4 in 
    match e4 with
    | Succ s -> normalize ctx (App (App(e3, s), ElimNat (normalize ctx e1, normalize ctx e2, normalize ctx e3, normalize ctx s)))
    | Zero -> e1
    | e4 -> ElimNat(e1, e2, e3, e4)

and normalize_abstraction ctx (x, t, e) =
  let t = normalize ctx t in
    (x, t, normalize (extend x t ctx) e)

(** [equal ctx e1 e2] determines whether normalized [e1] and [e2] are equal up to renaming
    of bound variables. *)
let equal ctx e1 e2 =
  let rec equal e1 e2 =
    match e1, e2 with
      | Var x1, Var x2 -> x1 = x2
      | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
      | Universe k1, Universe k2 -> k1 = k2
      | Pi a1, Pi a2 -> equal_abstraction a1 a2
      | Lambda a1, Lambda a2 -> equal_abstraction a1 a2
      | Nat, Nat -> true
      | Zero, Zero -> true
      | Succ e1, Succ e2 -> equal e1 e2
      | ElimNat (e11, e12, e13, e14), ElimNat (e21, e22, e23, e24) -> equal e11 e21 && equal e12 e22 && equal e13 e23 && equal e14 e24
      | (Var _ | App _ | Universe _ | Pi _ | Lambda _ | Nat | Zero | Succ _ | ElimNat _), _ -> false
  and equal_abstraction (x, t1, e1) (y, t2, e2) =
    let z = Var (fresh x) in
      equal t1 t2 && (equal (subst [(x,z)] e1) (subst [(y, z)] e2))
  in
    equal (normalize ctx e1) (normalize ctx e2)

(** [infer_type ctx e] infers the type of expression [e] in context [ctx].  *)
let rec infer_type ctx = function
  | Var x ->
    (try lookup_ty x ctx
     with Not_found -> Error.typing "unkown identifier %t" (Print.variable x))
  | Universe k -> Universe (k + 1)
  | Pi (x, t1, t2) ->
    let k1 = infer_universe ctx t1 in
    let k2 = infer_universe (extend x t1 ctx) t2 in
      Universe (max k1 k2)
  | Lambda (x, t, e) ->
    let _ = infer_universe ctx t in
    let te = infer_type (extend x t ctx) e in
      Pi (x, t, te)
  | App (e1, e2) ->
    let (x, s, t) = infer_pi ctx e1 in
    let te = infer_type ctx e2 in
      check_equal ctx s te ;
      subst [(x, e2)] t
  | Nat -> Universe 0
  | Succ e -> let t = infer_type ctx e in 
    check_equal ctx t Nat ;
    Nat
  | Zero -> Nat
  | ElimNat (e1, e2, e3, e4) ->
    let t1' = Pi(Dummy, Nat, Universe 0) in
      check_equal ctx (infer_type ctx e1) t1' ;
    let t2' = (App (e1, Zero)) in
      check_equal ctx (infer_type ctx e2) t2'  ;
    let t3' = Pi (String "x", Nat, Pi (Dummy, App (e1, Var (String "x")), App (e1, Succ (Var (String "x"))))) in
      check_equal ctx (infer_type ctx e3) t3' ;
    let t4' = Nat in
      check_equal ctx (infer_type ctx e4) t4' ;
    normalize ctx (App (e1, e4))


(** [infer_universe ctx t] infers the universe level of type [t] in context [ctx]. *)
and infer_universe ctx t =
  let u = infer_type ctx t in
    match normalize ctx u with
      | Universe k -> k
      | Var _ | App _ | Pi _ | Lambda _ | Nat | Zero | Succ _ | ElimNat _ -> Error.typing "function expected"

(** [infer_pi ctx e] infers the type of [e] in context [ctx], verifies that it is
    of the form [Pi (x, t1, t2)] and returns the triple [(x, t1, t2)]. *)
and infer_pi ctx e =
  let t = infer_type ctx e in
    match normalize ctx t with
      | Pi a -> a
      | Var _ | App _ | Universe _ | Lambda _ | Nat | Zero | Succ _ | ElimNat _ -> Error.typing "function expected"

(** [check_equal ctx e1 e2] checks that expressions [e1] and [e2] are equal. *)
and check_equal ctx e1 e2 =
  if not (equal ctx e1 e2)
  then Error.typing "expressions %t and %t are not equal" (Print.expr e1) (Print.expr e2)
