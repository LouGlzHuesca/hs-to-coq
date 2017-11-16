(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Wf.

(* Preamble *)

(*
Inductive WrappedMonad (m : Type -> Type) a : Type := Mk_WrapMonad : m a -> WrappedMonad m a.

Inductive WrappedArrow (a : Type -> Type -> Type) b c : Type := Mk_WrapArrow : a b c -> WrappedArrow a b c.

Arguments Mk_WrapMonad {_} {_} _.

Arguments Mk_WrapArrow {_} {_} {_} _.

Definition unwrapMonad {m} {a} (arg_0__ : WrappedMonad m a) :=
  match arg_0__ with
    | Mk_WrapMonad unwrapMonad => unwrapMonad
  end.

Definition unwrapArrow {a} {b} {c} (arg_1__ : WrappedArrow a b c) :=
  match arg_1__ with
    | Mk_WrapArrow unwrapArrow => unwrapArrow
  end.
*)
(* Converted imports: *)

Require Control.Arrow.
Require Control.Category.
Require Coq.Program.Basics.
Require Data.Functor.
Require Data.Tuple.
Require GHC.Base.

(* Converted type declarations: *)

Inductive WrappedMonad (m : Type -> Type) a : Type := Mk_WrapMonad : m
                                                                     a -> WrappedMonad m a.

Inductive WrappedArrow (a : Type -> Type -> Type) b c : Type := Mk_WrapArrow : a
                                                                               b c -> WrappedArrow a b c.

Arguments Mk_WrapMonad {_} {_} _.

Arguments Mk_WrapArrow {_} {_} {_} _.

Definition unwrapMonad {m : Type -> Type} {a} (arg_0__ : WrappedMonad m a) :=
  match arg_0__ with
    | Mk_WrapMonad unwrapMonad => unwrapMonad
  end.

Definition unwrapArrow {a : Type -> Type -> Type} {b} {c} (arg_1__
                         : WrappedArrow a b c) :=
  match arg_1__ with
    | Mk_WrapArrow unwrapArrow => unwrapArrow
  end.
(* Converted value declarations: *)

Local Definition instance_forall___GHC_Base_Monad_m___GHC_Base_Functor__WrappedMonad_m__fmap {inst_m}
                                                                                             `{GHC.Base.Monad inst_m}
    : forall {a} {b},
        (a -> b) -> (WrappedMonad inst_m) a -> (WrappedMonad inst_m) b :=
  fun {a} {b} =>
    fun arg_28__ arg_29__ =>
      match arg_28__ , arg_29__ with
        | f , Mk_WrapMonad v => Mk_WrapMonad (GHC.Base.liftM f v)
      end.

Local Definition instance_forall___GHC_Base_Monad_m___GHC_Base_Functor__WrappedMonad_m__op_zlzd__ {inst_m}
                                                                                                  `{GHC.Base.Monad
                                                                                                  inst_m} : forall {a}
                                                                                                                   {b},
                                                                                                              a -> (WrappedMonad
                                                                                                              inst_m)
                                                                                                              b -> (WrappedMonad
                                                                                                              inst_m)
                                                                                                              a :=
  fun {a} {b} =>
    fun x =>
      instance_forall___GHC_Base_Monad_m___GHC_Base_Functor__WrappedMonad_m__fmap
      (GHC.Base.const x).

Instance instance_forall___GHC_Base_Monad_m___GHC_Base_Functor__WrappedMonad_m_ {m}
                                                                                `{GHC.Base.Monad m} : GHC.Base.Functor
                                                                                                      (WrappedMonad
                                                                                                      m) := fun _ k =>
    k (GHC.Base.Functor__Dict_Build (WrappedMonad m) (fun {a} {b} =>
                                      instance_forall___GHC_Base_Monad_m___GHC_Base_Functor__WrappedMonad_m__op_zlzd__)
                                    (fun {a} {b} =>
                                      instance_forall___GHC_Base_Monad_m___GHC_Base_Functor__WrappedMonad_m__fmap)).

Local Definition instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__op_zlztzg__ {inst_m}
                                                                                                        `{GHC.Base.Monad
                                                                                                        inst_m}
    : forall {a} {b},
        (WrappedMonad inst_m) (a -> b) -> (WrappedMonad inst_m) a -> (WrappedMonad
        inst_m) b :=
  fun {a} {b} =>
    fun arg_24__ arg_25__ =>
      match arg_24__ , arg_25__ with
        | Mk_WrapMonad f , Mk_WrapMonad v => Mk_WrapMonad (GHC.Base.ap f v)
      end.

Local Definition instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__op_ztzg__ {inst_m}
                                                                                                      `{GHC.Base.Monad
                                                                                                      inst_m}
    : forall {a} {b},
        (WrappedMonad inst_m) a -> (WrappedMonad inst_m) b -> (WrappedMonad inst_m) b :=
  fun {a} {b} =>
    fun x y =>
      instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__op_zlztzg__
      (GHC.Base.fmap (GHC.Base.const GHC.Base.id) x) y.

Local Definition instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__pure {inst_m}
                                                                                                 `{GHC.Base.Monad
                                                                                                 inst_m} : forall {a},
                                                                                                             a -> (WrappedMonad
                                                                                                             inst_m)
                                                                                                             a :=
  fun {a} => Coq.Program.Basics.compose Mk_WrapMonad GHC.Base.pure.

Instance instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m_ {m}
                                                                                    `{GHC.Base.Monad m}
  : GHC.Base.Applicative (WrappedMonad m) := fun _ k =>
    k (GHC.Base.Applicative__Dict_Build (WrappedMonad m) (fun {a} {b} =>
                                          instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__op_ztzg__)
                                        (fun {a} {b} =>
                                          instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__op_zlztzg__)
                                        (fun {a} =>
                                          instance_forall___GHC_Base_Monad_m___GHC_Base_Applicative__WrappedMonad_m__pure)).

(* Translating `instance forall {m}, forall `{GHC.Base.MonadPlus m},
   GHC.Base.Alternative (WrappedMonad m)' failed: OOPS! Cannot find information for
   class "GHC.Base.Alternative" unsupported *)

Local Definition instance_forall___Control_Arrow_Arrow_a___GHC_Base_Functor__WrappedArrow_a_b__fmap {inst_a}
                                                                                                    {inst_b}
                                                                                                    `{Control.Arrow.Arrow
                                                                                                    inst_a} : forall {a}
                                                                                                                     {b},
                                                                                                                (a -> b) -> (WrappedArrow
                                                                                                                inst_a
                                                                                                                inst_b)
                                                                                                                a -> (WrappedArrow
                                                                                                                inst_a
                                                                                                                inst_b)
                                                                                                                b :=
  fun {a} {b} =>
    fun arg_19__ arg_20__ =>
      match arg_19__ , arg_20__ with
        | f , Mk_WrapArrow a => Mk_WrapArrow (Control.Category.op_zgzgzg__ a
                                                                           (Control.Arrow.arr f))
      end.

Local Definition instance_forall___Control_Arrow_Arrow_a___GHC_Base_Functor__WrappedArrow_a_b__op_zlzd__ {inst_a}
                                                                                                         {inst_b}
                                                                                                         `{Control.Arrow.Arrow
                                                                                                         inst_a}
    : forall {a} {b},
        a -> (WrappedArrow inst_a inst_b) b -> (WrappedArrow inst_a inst_b) a :=
  fun {a} {b} =>
    fun x =>
      instance_forall___Control_Arrow_Arrow_a___GHC_Base_Functor__WrappedArrow_a_b__fmap
      (GHC.Base.const x).

Instance instance_forall___Control_Arrow_Arrow_a___GHC_Base_Functor__WrappedArrow_a_b_ {a}
                                                                                       {b} `{Control.Arrow.Arrow a}
  : GHC.Base.Functor (WrappedArrow a b) := fun _ k =>
    k (GHC.Base.Functor__Dict_Build (WrappedArrow a b) (fun {a} {b} =>
                                      instance_forall___Control_Arrow_Arrow_a___GHC_Base_Functor__WrappedArrow_a_b__op_zlzd__)
                                    (fun {a} {b} =>
                                      instance_forall___Control_Arrow_Arrow_a___GHC_Base_Functor__WrappedArrow_a_b__fmap)).

Local Definition instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__op_zlztzg__ {inst_a}
                                                                                                               {inst_b}
                                                                                                               `{Control.Arrow.Arrow
                                                                                                               inst_a}
    : forall {a} {b},
        (WrappedArrow inst_a inst_b) (a -> b) -> (WrappedArrow inst_a inst_b)
        a -> (WrappedArrow inst_a inst_b) b :=
  fun {a} {b} =>
    fun arg_15__ arg_16__ =>
      match arg_15__ , arg_16__ with
        | Mk_WrapArrow f , Mk_WrapArrow v => Mk_WrapArrow (Control.Category.op_zgzgzg__
                                                          (Control.Arrow.op_zazaza__ f v) (Control.Arrow.arr
                                                          (Data.Tuple.uncurry GHC.Base.id)))
      end.

Local Definition instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__op_ztzg__ {inst_a}
                                                                                                             {inst_b}
                                                                                                             `{Control.Arrow.Arrow
                                                                                                             inst_a}
    : forall {a} {b},
        (WrappedArrow inst_a inst_b) a -> (WrappedArrow inst_a inst_b)
        b -> (WrappedArrow inst_a inst_b) b :=
  fun {a} {b} =>
    fun x y =>
      instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__op_zlztzg__
      (GHC.Base.fmap (GHC.Base.const GHC.Base.id) x) y.

Local Definition instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__pure {inst_a}
                                                                                                        {inst_b}
                                                                                                        `{Control.Arrow.Arrow
                                                                                                        inst_a}
    : forall {a}, a -> (WrappedArrow inst_a inst_b) a :=
  fun {a} =>
    fun arg_12__ =>
      match arg_12__ with
        | x => Mk_WrapArrow (Control.Arrow.arr (GHC.Base.const x))
      end.

Instance instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b_ {a}
                                                                                           {b} `{Control.Arrow.Arrow a}
  : GHC.Base.Applicative (WrappedArrow a b) := fun _ k =>
    k (GHC.Base.Applicative__Dict_Build (WrappedArrow a b) (fun {a} {b} =>
                                          instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__op_ztzg__)
                                        (fun {a} {b} =>
                                          instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__op_zlztzg__)
                                        (fun {a} =>
                                          instance_forall___Control_Arrow_Arrow_a___GHC_Base_Applicative__WrappedArrow_a_b__pure)).

(* Translating `instance forall {a} {b}, forall `{Control.Arrow.ArrowZero a}
   `{Control.Arrow.ArrowPlus a}, GHC.Base.Alternative (WrappedArrow a b)' failed:
   OOPS! Cannot find information for class "GHC.Base.Alternative" unsupported *)

(* Skipping instance instance_GHC_Base_Applicative_ZipList *)

(* Translating `instance GHC.Generics.Generic1 ZipList' failed: OOPS! Cannot
   find information for class "GHC.Generics.Generic1" unsupported *)

(* Translating `instance forall {a}, GHC.Generics.Generic (ZipList a)' failed:
   OOPS! Cannot find information for class "GHC.Generics.Generic" unsupported *)

(* Translating `instance Data.Foldable.Foldable ZipList' failed: type
   applications unsupported *)

(* Translating `instance GHC.Base.Functor ZipList' failed: type applications
   unsupported *)

(* Translating `instance forall {a}, forall `{GHC.Read.Read a}, GHC.Read.Read
   (ZipList a)' failed: OOPS! Cannot find information for class "GHC.Read.Read"
   unsupported *)

(* Translating `instance forall {a}, forall `{GHC.Base.Ord a}, GHC.Base.Ord
   (ZipList a)' failed: type applications unsupported *)

(* Translating `instance forall {a}, forall `{GHC.Base.Eq_ a}, GHC.Base.Eq_
   (ZipList a)' failed: type applications unsupported *)

(* Translating `instance forall {a}, forall `{GHC.Show.Show a}, GHC.Show.Show
   (ZipList a)' failed: OOPS! Cannot find information for class "GHC.Show.Show"
   unsupported *)

(* Translating `instance forall {a} {b}, GHC.Generics.Generic1 (WrappedArrow a
   b)' failed: OOPS! Cannot find information for class "GHC.Generics.Generic1"
   unsupported *)

(* Translating `instance forall {a} {b} {c}, GHC.Generics.Generic (WrappedArrow
   a b c)' failed: OOPS! Cannot find information for class "GHC.Generics.Generic"
   unsupported *)

(* Translating `instance forall {m}, forall `{GHC.Base.Monad m}, GHC.Base.Monad
   (WrappedMonad m)' failed: type applications unsupported *)

(* Translating `instance forall {m}, GHC.Generics.Generic1 (WrappedMonad m)'
   failed: OOPS! Cannot find information for class "GHC.Generics.Generic1"
   unsupported *)

(* Translating `instance forall {m} {a}, GHC.Generics.Generic (WrappedMonad m
   a)' failed: OOPS! Cannot find information for class "GHC.Generics.Generic"
   unsupported *)

Definition optional {f} {a} `{GHC.Base.Alternative f} : f a -> f (option a) :=
  fun arg_2__ =>
    match arg_2__ with
      | v => GHC.Base.op_zlzbzg__ (Data.Functor.op_zlzdzg__ Some v) (GHC.Base.pure
                                  None)
    end.

(* Unbound variables:
     Control.Arrow.Arrow Control.Arrow.arr Control.Arrow.op_zazaza__
     Control.Category.op_zgzgzg__ Coq.Program.Basics.compose Data.Functor.op_zlzdzg__
     Data.Tuple.uncurry GHC.Base.Alternative GHC.Base.Applicative
     GHC.Base.Applicative__Dict_Build GHC.Base.Functor GHC.Base.Functor__Dict_Build
     GHC.Base.Monad GHC.Base.ap GHC.Base.const GHC.Base.fmap GHC.Base.id
     GHC.Base.liftM GHC.Base.op_zlzbzg__ GHC.Base.pure None Some Type option
*)
