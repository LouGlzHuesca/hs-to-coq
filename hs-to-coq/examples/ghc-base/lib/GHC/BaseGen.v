(* Default settings (from HsToCoq.Coq.Preamble) *)

Set Implicit Arguments.
Set Maximal Implicit Insertion.
Generalizable All Variables.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Axiom patternFailure : forall {a}, a.

(* Preamble *)
Require Export Prim.

(* Successfully converted the following code: *)
Local Definition instance_Monoid_unit_mappend : unit -> (unit -> unit) :=
  fun arg_228__ arg_229__ => tt.
Local Definition instance_Monoid_unit_mconcat : (list unit) -> unit :=
  fun arg_230__ => tt.
Local Definition instance_Monoid_unit_mempty : unit :=
  tt.
Local Definition instance_Monoid_comparison_mappend
    : comparison -> (comparison -> comparison) :=
  fun arg_203__ arg_204__ =>
    let j_205__ :=
      match arg_203__ , arg_204__ with
        | Gt , _ => Gt
        | _ , _ => patternFailure
      end in
    let j_206__ :=
      match arg_203__ , arg_204__ with
        | Eq , y => y
        | _ , _ => j_205__
      end in
    match arg_203__ , arg_204__ with
      | Lt , _ => Lt
      | _ , _ => j_206__
    end.
Local Definition instance_Monoid_comparison_mempty : comparison :=
  Eq.
Local Definition instance_Functor_option_fmap : (forall {a} {b},
                                                  (a -> b) -> ((option a) -> (option b))) :=
  fun {a} {b} =>
    fun arg_192__ arg_193__ =>
      let j_195__ :=
        match arg_192__ , arg_193__ with
          | f , (Some a) => Some (f a)
          | _ , _ => patternFailure
        end in
      match arg_192__ , arg_193__ with
        | _ , None => None
        | _ , _ => j_195__
      end.
Local Definition instance_Applicative_option_op_ztzg__ : (forall {a} {b},
                                                           (option a) -> ((option b) -> (option b))) :=
  fun {a} {b} =>
    fun arg_188__ arg_189__ =>
      let j_190__ :=
        match arg_188__ , arg_189__ with
          | None , _m2 => None
          | _ , _ => patternFailure
        end in
      match arg_188__ , arg_189__ with
        | (Some _m1) , m2 => m2
        | _ , _ => j_190__
      end.
Local Definition instance_Applicative_option_pure : (forall {a},
                                                      a -> (option a)) :=
  fun {a} => Some.
Local Definition instance_Monad_option_fail : (forall {a},
                                                String -> (option a)) :=
  fun {a} => fun arg_182__ => None.
Local Definition instance_Monad_option_op_zgzgze__ : (forall {a} {b},
                                                       (option a) -> ((a -> (option b)) -> (option b))) :=
  fun {a} {b} =>
    fun arg_177__ arg_178__ =>
      let j_179__ :=
        match arg_177__ , arg_178__ with
          | None , _ => None
          | _ , _ => patternFailure
        end in
      match arg_177__ , arg_178__ with
        | (Some x) , k => k x
        | _ , _ => j_179__
      end.
(* Skipping instance instance_Alternative_option *)
(* Skipping instance instance_MonadPlus_option *)
Local Definition instance_Applicative_list_op_zlztzg__ : (forall {a} {b},
                                                           (list (a -> b)) -> ((list a) -> (list b))) :=
  fun {a} {b} =>
    fun arg_165__ arg_166__ =>
      match arg_165__ , arg_166__ with
        | fs , xs => concatMap (fun f => (concatMap (fun x => (f x :: nil)) xs)) fs
      end.
Local Definition instance_Applicative_list_op_ztzg__ : (forall {a} {b},
                                                         (list a) -> ((list b) -> (list b))) :=
  fun {a} {b} =>
    fun arg_169__ arg_170__ =>
      match arg_169__ , arg_170__ with
        | xs , ys => concatMap (fun _ => (concatMap (fun y => (y :: nil)) ys)) xs
      end.
Local Definition instance_Applicative_list_pure : (forall {a}, a -> (list a)) :=
  fun {a} => fun arg_162__ => match arg_162__ with | x => (x :: nil) end.
Local Definition instance_Monad_list_fail : (forall {a}, String -> (list a)) :=
  fun {a} => fun arg_161__ => nil.
Local Definition instance_Monad_list_op_zgzgze__ : (forall {a} {b},
                                                     (list a) -> ((a -> (list b)) -> (list b))) :=
  fun {a} {b} =>
    fun arg_157__ arg_158__ =>
      match arg_157__ , arg_158__ with
        | xs , f => concatMap (fun x => (concatMap (fun y => (y :: nil)) (f x))) xs
      end.
(* Skipping instance instance_Alternative_list *)
(* Skipping instance instance_MonadPlus_list *)
(* Skipping instance instance_Functor_IO *)
(* Skipping instance instance_Applicative_IO *)
(* Skipping instance instance_Monad_IO *)
(* Skipping instance instance_Alternative_IO *)
(* Skipping instance instance_MonadPlus_IO *)
Definition assert {a} : bool -> (a -> a) :=
  fun arg_49__ arg_50__ => match arg_49__ , arg_50__ with | _pred , r => r end.
Definition breakpoint {a} : a -> a :=
  fun arg_47__ => match arg_47__ with | r => r end.
Definition breakpointCond {a} : bool -> (a -> a) :=
  fun arg_44__ arg_45__ => match arg_44__ , arg_45__ with | _ , r => r end.
Definition const {a} {b} : a -> (b -> a) :=
  fun arg_41__ arg_42__ => match arg_41__ , arg_42__ with | x , _ => x end.
Definition asTypeOf {a} : a -> (a -> a) :=
  const.
Definition foldr {a} {b} : (a -> (b -> b)) -> (b -> ((list a) -> b)) :=
  fun arg_67__ arg_68__ =>
    match arg_67__ , arg_68__ with
      | k , z => let go :=
                   fix go arg_69__
                         := let j_71__ :=
                              match arg_69__ with
                                | (y :: ys) => k y (go ys)
                                | _ => patternFailure
                              end in
                            match arg_69__ with
                              | nil => z
                              | _ => j_71__
                            end in
                 go
    end.
Local Definition instance_Monoid_comparison_mconcat : (list
                                                      comparison) -> comparison :=
  foldr instance_Monoid_comparison_mappend instance_Monoid_comparison_mempty.
Definition id {a} : a -> a :=
  fun arg_52__ => match arg_52__ with | x => x end.
Definition map {a} {b} : (a -> b) -> ((list a) -> (list b)) :=
  fix map arg_62__ arg_63__
        := let j_65__ :=
             match arg_62__ , arg_63__ with
               | f , (x :: xs) => (f x :: map f xs)
               | _ , _ => patternFailure
             end in
           match arg_62__ , arg_63__ with
             | _ , nil => nil
             | _ , _ => j_65__
           end.
Local Definition instance_Functor_list_fmap : (forall {a} {b},
                                                (a -> b) -> ((list a) -> (list b))) :=
  fun {a} {b} => map.
Definition mapFB {a} {elt} {lst}
    : (elt -> (lst -> lst)) -> ((a -> elt) -> (a -> (lst -> lst))) :=
  fun arg_54__ arg_55__ =>
    match arg_54__ , arg_55__ with
      | c , f => fun arg_56__ arg_57__ =>
                   match arg_56__ , arg_57__ with
                     | x , ys => c (f x) ys
                   end
    end.
Definition op_z2218U__ {a} {b} {c} : (b -> c) -> ((a -> b) -> (a -> c)) :=
  fun arg_34__ arg_35__ =>
    match arg_34__ , arg_35__ with
      | f , g => fun arg_36__ => match arg_36__ with | x => f (g x) end
    end.
Infix "∘" := (op_z2218U__) (left associativity, at level 40).
Local Definition instance_Functor_list_op_zlzd__ : (forall {a} {b},
                                                     a -> ((list b) -> (list a))) :=
  fun {a} {b} => (instance_Functor_list_fmap ∘ const).
Local Definition instance_Functor_option_op_zlzd__ : (forall {a} {b},
                                                       a -> ((option b) -> (option a))) :=
  fun {a} {b} => (instance_Functor_option_fmap ∘ const).
Notation "'_∘_'" := (op_z2218U__).
Definition op_zd__ {a} {b} : (a -> b) -> (a -> b) :=
  fun arg_30__ arg_31__ => match arg_30__ , arg_31__ with | f , x => f x end.
Infix "$" := (op_zd__) (at level 99).
Notation "'_$_'" := (op_zd__).
Definition op_zdzn__ {a} {b} : (a -> b) -> (a -> b) :=
  fun arg_26__ arg_27__ =>
    match arg_26__ , arg_27__ with
      | f , x => match x with
                   | vx => f vx
                 end
    end.
Infix "$!" := (op_zdzn__) (at level 99).
Notation "'_$!_'" := (op_zdzn__).
Definition otherwise : bool :=
  true.
Class Functor f := {
  op_zlzd__ : (forall {a} {b}, a -> ((f b) -> (f a))) ;
  fmap : (forall {a} {b}, (a -> b) -> ((f a) -> (f b))) }.
Infix "<$" := (op_zlzd__) (at level 99).
Notation "'_<$_'" := (op_zlzd__).
Class Applicative f `{Functor f} := {
  op_ztzg__ : (forall {a} {b}, (f a) -> ((f b) -> (f b))) ;
  op_zlzt__ : (forall {a} {b}, (f a) -> ((f b) -> (f a))) ;
  op_zlztzg__ : (forall {a} {b}, (f (a -> b)) -> ((f a) -> (f b))) ;
  pure : (forall {a}, a -> (f a)) }.
Infix "*>" := (op_ztzg__) (at level 99).
Notation "'_*>_'" := (op_ztzg__).
Infix "<*" := (op_zlzt__) (at level 99).
Notation "'_<*_'" := (op_zlzt__).
Infix "<*>" := (op_zlztzg__) (at level 99).
Notation "'_<*>_'" := (op_zlztzg__).
Class Monad m `{Applicative m} := {
  op_zgzg__ : (forall {a} {b}, (m a) -> ((m b) -> (m b))) ;
  op_zgzgze__ : (forall {a} {b}, (m a) -> ((a -> (m b)) -> (m b))) ;
  fail : (forall {a}, String -> (m a)) ;
  return_ : (forall {a}, a -> (m a)) }.
Infix ">>" := (op_zgzg__) (at level 99).
Notation "'_>>_'" := (op_zgzg__).
Infix ">>=" := (op_zgzgze__) (at level 99).
Notation "'_>>=_'" := (op_zgzgze__).
Definition op_zezlzl__ {a} {b} {m} `{Monad m} : (a -> (m b)) -> ((m a) -> (m
                                                b)) :=
  fun arg_121__ arg_122__ =>
    match arg_121__ , arg_122__ with
      | f , x => (x >>= f)
    end.
Infix "=<<" := (op_zezlzl__) (at level 99).
Notation "'_=<<_'" := (op_zezlzl__).
Definition liftM5 {a1} {a2} {a3} {a4} {a5} {m} {r} `{(Monad m)}
    : (a1 -> (a2 -> (a3 -> (a4 -> (a5 -> r))))) -> ((m a1) -> ((m a2) -> ((m
      a3) -> ((m a4) -> ((m a5) -> (m r)))))) :=
  fun arg_78__ arg_79__ arg_80__ arg_81__ arg_82__ arg_83__ =>
    match arg_78__ , arg_79__ , arg_80__ , arg_81__ , arg_82__ , arg_83__ with
      | f , m1 , m2 , m3 , m4 , m5 => (m1 >>= fun x1 =>
                                        (m2 >>= fun x2 =>
                                          (m3 >>= fun x3 =>
                                            (m4 >>= fun x4 => (m5 >>= fun x5 => (return_ (f x1 x2 x3 x4 x5)))))))
    end.
Definition liftM4 {a1} {a2} {a3} {a4} {m} {r} `{(Monad m)}
    : (a1 -> (a2 -> (a3 -> (a4 -> r)))) -> ((m a1) -> ((m a2) -> ((m a3) -> ((m
      a4) -> (m r))))) :=
  fun arg_86__ arg_87__ arg_88__ arg_89__ arg_90__ =>
    match arg_86__ , arg_87__ , arg_88__ , arg_89__ , arg_90__ with
      | f , m1 , m2 , m3 , m4 => (m1 >>= fun x1 =>
                                   (m2 >>= fun x2 =>
                                     (m3 >>= fun x3 => (m4 >>= fun x4 => (return_ (f x1 x2 x3 x4))))))
    end.
Definition liftM3 {a1} {a2} {a3} {m} {r} `{(Monad m)}
    : (a1 -> (a2 -> (a3 -> r))) -> ((m a1) -> ((m a2) -> ((m a3) -> (m r)))) :=
  fun arg_93__ arg_94__ arg_95__ arg_96__ =>
    match arg_93__ , arg_94__ , arg_95__ , arg_96__ with
      | f , m1 , m2 , m3 => (m1 >>= fun x1 =>
                              (m2 >>= fun x2 => (m3 >>= fun x3 => (return_ (f x1 x2 x3)))))
    end.
Definition liftM2 {a1} {a2} {m} {r} `{(Monad m)} : (a1 -> (a2 -> r)) -> ((m
                                                   a1) -> ((m a2) -> (m r))) :=
  fun arg_99__ arg_100__ arg_101__ =>
    match arg_99__ , arg_100__ , arg_101__ with
      | f , m1 , m2 => (m1 >>= fun x1 => (m2 >>= fun x2 => (return_ (f x1 x2))))
    end.
Definition liftM {a1} {m} {r} `{(Monad m)} : (a1 -> r) -> ((m a1) -> (m r)) :=
  fun arg_104__ arg_105__ =>
    match arg_104__ , arg_105__ with
      | f , m1 => (m1 >>= fun x1 => (return_ (f x1)))
    end.
Definition join {a} {m} `{(Monad m)} : (m (m a)) -> (m a) :=
  fun arg_125__ => match arg_125__ with | x => (x >>= id) end.
Definition mapM {a} {b} {m} `{Monad m} : (a -> (m b)) -> ((list a) -> (m (list
                                                                         b))) :=
  fun arg_108__ arg_109__ =>
    match arg_108__ , arg_109__ with
      | f , as_ => let k :=
                     fun arg_110__ arg_111__ =>
                       match arg_110__ , arg_111__ with
                         | a , r => (f a >>= fun x => (r >>= fun xs => (return_ ((x :: xs)))))
                       end in
                   foldr k (return_ nil) as_
    end.
Definition sequence {a} {m} `{Monad m} : (list (m a)) -> (m (list a)) :=
  mapM id.
Definition ap {a} {b} {m} `{(Monad m)} : (m (a -> b)) -> ((m a) -> (m b)) :=
  fun arg_74__ arg_75__ =>
    match arg_74__ , arg_75__ with
      | m1 , m2 => (m1 >>= fun x1 => (m2 >>= fun x2 => (return_ (x1 x2))))
    end.
Class Alternative f `{Applicative f} := {
  op_zlzbzg__ : (forall {a}, (f a) -> ((f a) -> (f a))) ;
  empty : (forall {a}, f a) ;
  many : (forall {a}, (f a) -> (f (list a))) ;
  some : (forall {a}, (f a) -> (f (list a))) }.
Infix "<|>" := (op_zlzbzg__) (at level 99).
Notation "'_<|>_'" := (op_zlzbzg__).
Class MonadPlus m `{Alternative m} `{Monad m} := {
  mplus : (forall {a}, (m a) -> ((m a) -> (m a))) ;
  mzero : (forall {a}, m a) }.
Definition when {f} `{(Applicative f)} : bool -> ((f unit) -> (f unit)) :=
  fun arg_117__ arg_118__ =>
    match arg_117__ , arg_118__ with
      | p , s => if p
                 then s
                 else pure tt
    end.
Definition liftA {a} {b} {f} `{Applicative f} : (a -> b) -> ((f a) -> (f b)) :=
  fun arg_139__ arg_140__ =>
    match arg_139__ , arg_140__ with
      | f , a => (pure f <*> a)
    end.
Definition liftA3 {a} {b} {c} {d} {f} `{Applicative f}
    : (a -> (b -> (c -> d))) -> ((f a) -> ((f b) -> ((f c) -> (f d)))) :=
  fun arg_128__ arg_129__ arg_130__ arg_131__ =>
    match arg_128__ , arg_129__ , arg_130__ , arg_131__ with
      | f , a , b , c => ((fmap f a <*> b) <*> c)
    end.
Definition liftA2 {a} {b} {c} {f} `{Applicative f} : (a -> (b -> c)) -> ((f
                                                     a) -> ((f b) -> (f c))) :=
  fun arg_134__ arg_135__ arg_136__ =>
    match arg_134__ , arg_135__ , arg_136__ with
      | f , a , b => (fmap f a <*> b)
    end.
Definition op_zlztztzg__ {a} {b} {f} `{Applicative f} : (f a) -> ((f
                                                        (a -> b)) -> (f b)) :=
  liftA2 (flip _$_).
Infix "<**>" := (op_zlztztzg__) (at level 99).
Notation "'_<**>_'" := (op_zlztztzg__).
Instance instance_Functor_list : !Functor list := {
  fmap := fun {a} {b} => instance_Functor_list_fmap ;
  op_zlzd__ := fun {a} {b} => instance_Functor_list_op_zlzd__ }.
Local Definition instance_Applicative_list_op_zlzt__ : (forall {a} {b},
                                                         (list a) -> ((list b) -> (list a))) :=
  fun {a} {b} =>
    fun arg_6__ arg_7__ =>
      match arg_6__ , arg_7__ with
        | a , b => instance_Applicative_list_op_zlztzg__ (fmap const a) b
      end.
Instance instance_Applicative_list : !Applicative list := {
  op_zlzt__ := fun {a} {b} => instance_Applicative_list_op_zlzt__ ;
  op_zlztzg__ := fun {a} {b} => instance_Applicative_list_op_zlztzg__ ;
  op_ztzg__ := fun {a} {b} => instance_Applicative_list_op_ztzg__ ;
  pure := fun {a} => instance_Applicative_list_pure }.
Local Definition instance_Monad_list_op_zgzg__ : (forall {a} {b},
                                                   (list a) -> ((list b) -> (list b))) :=
  fun {a} {b} => _*>_.
Local Definition instance_Monad_list_return_ : (forall {a}, a -> (list a)) :=
  fun {a} => pure.
Instance instance_Monad_list : !Monad list := {
  fail := fun {a} => instance_Monad_list_fail ;
  op_zgzg__ := fun {a} {b} => instance_Monad_list_op_zgzg__ ;
  op_zgzgze__ := fun {a} {b} => instance_Monad_list_op_zgzgze__ ;
  return_ := fun {a} => instance_Monad_list_return_ }.
Instance instance_Functor_option : !Functor option := {
  fmap := fun {a} {b} => instance_Functor_option_fmap ;
  op_zlzd__ := fun {a} {b} => instance_Functor_option_op_zlzd__ }.
Local Definition instance_Applicative_option_op_zlztzg__ : (forall {a} {b},
                                                             (option (a -> b)) -> ((option a) -> (option b))) :=
  fun {a} {b} =>
    fun arg_183__ arg_184__ =>
      let j_185__ :=
        match arg_183__ , arg_184__ with
          | None , _m => None
          | _ , _ => patternFailure
        end in
      match arg_183__ , arg_184__ with
        | (Some f) , m => fmap f m
        | _ , _ => j_185__
      end.
Local Definition instance_Applicative_option_op_zlzt__ : (forall {a} {b},
                                                           (option a) -> ((option b) -> (option a))) :=
  fun {a} {b} =>
    fun arg_6__ arg_7__ =>
      match arg_6__ , arg_7__ with
        | a , b => instance_Applicative_option_op_zlztzg__ (fmap const a) b
      end.
Instance instance_Applicative_option : !Applicative option := {
  op_zlzt__ := fun {a} {b} => instance_Applicative_option_op_zlzt__ ;
  op_zlztzg__ := fun {a} {b} => instance_Applicative_option_op_zlztzg__ ;
  op_ztzg__ := fun {a} {b} => instance_Applicative_option_op_ztzg__ ;
  pure := fun {a} => instance_Applicative_option_pure }.
Local Definition instance_Monad_option_op_zgzg__ : (forall {a} {b},
                                                     (option a) -> ((option b) -> (option b))) :=
  fun {a} {b} => _*>_.
Local Definition instance_Monad_option_return_ : (forall {a},
                                                   a -> (option a)) :=
  fun {a} => pure.
Instance instance_Monad_option : !Monad option := {
  fail := fun {a} => instance_Monad_option_fail ;
  op_zgzg__ := fun {a} {b} => instance_Monad_option_op_zgzg__ ;
  op_zgzgze__ := fun {a} {b} => instance_Monad_option_op_zgzgze__ ;
  return_ := fun {a} => instance_Monad_option_return_ }.
Class Monoid a := {
  mappend : a -> (a -> a) ;
  mconcat : (list a) -> a ;
  mempty : a }.
Local Definition instance__forall___Monoid_a___Monoid__option_a___mempty `{Monoid
                                                                         a} : (option a) :=
  None.
Local Definition instance__forall___Monoid_a___Monoid__option_a___mappend `{Monoid
                                                                          a} : (option a) -> ((option a) -> (option
                                                                               a)) :=
  fun arg_197__ arg_198__ =>
    let j_200__ :=
      match arg_197__ , arg_198__ with
        | (Some m1) , (Some m2) => Some (mappend m1 m2)
        | _ , _ => patternFailure
      end in
    let j_201__ :=
      match arg_197__ , arg_198__ with
        | m , None => m
        | _ , _ => j_200__
      end in
    match arg_197__ , arg_198__ with
      | None , m => m
      | _ , _ => j_201__
    end.
Local Definition instance__forall___Monoid_a___Monoid__option_a___mconcat `{Monoid
                                                                          a} : (list (option a)) -> (option a) :=
  foldr instance__forall___Monoid_a___Monoid__option_a___mappend
  instance__forall___Monoid_a___Monoid__option_a___mempty.
Instance instance__forall___Monoid_a___Monoid__option_a__ : !(forall `{Monoid
                                                                     a},
                                                              Monoid (option a)) := {
  mappend := instance__forall___Monoid_a___Monoid__option_a___mappend ;
  mconcat := instance__forall___Monoid_a___Monoid__option_a___mconcat ;
  mempty := instance__forall___Monoid_a___Monoid__option_a___mempty }.
Instance instance_Monoid_comparison : !Monoid comparison := {
  mappend := instance_Monoid_comparison_mappend ;
  mconcat := instance_Monoid_comparison_mconcat ;
  mempty := instance_Monoid_comparison_mempty }.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mempty `{Monoid
                                                                                                                                  a}
                                                                                                                                  `{Monoid
                                                                                                                                  b}
                                                                                                                                  `{Monoid
                                                                                                                                  c}
                                                                                                                                  `{Monoid
                                                                                                                                  d}
                                                                                                                                  `{Monoid
                                                                                                                                  e}
    : a * b * c * d * e :=
  pair (pair (pair (pair mempty mempty) mempty) mempty) mempty.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mappend `{Monoid
                                                                                                                                   a}
                                                                                                                                   `{Monoid
                                                                                                                                   b}
                                                                                                                                   `{Monoid
                                                                                                                                   c}
                                                                                                                                   `{Monoid
                                                                                                                                   d}
                                                                                                                                   `{Monoid
                                                                                                                                   e}
    : a * b * c * d * e -> (a * b * c * d * e -> (a * b * c * d * e)) :=
  fun arg_209__ arg_210__ =>
    match arg_209__ , arg_210__ with
      | (pair (pair (pair (pair a1 b1) c1) d1) e1) , (pair (pair (pair (pair a2 b2)
                                                                       c2) d2) e2) => pair (pair (pair (pair (mappend a1
                                                                                                                      a2)
                                                                                                             (mappend b1
                                                                                                                      b2))
                                                                                                       (mappend c1 c2))
                                                                                                 (mappend d1 d2))
                                                                                           (mappend e1 e2)
    end.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mconcat `{Monoid
                                                                                                                                   a}
                                                                                                                                   `{Monoid
                                                                                                                                   b}
                                                                                                                                   `{Monoid
                                                                                                                                   c}
                                                                                                                                   `{Monoid
                                                                                                                                   d}
                                                                                                                                   `{Monoid
                                                                                                                                   e}
    : (list (a * b * c * d * e)) -> (a * b * c * d * e) :=
  foldr
  instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mappend
  instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mempty.
Instance instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e__
  : !(forall `{Monoid a} `{Monoid b} `{Monoid c} `{Monoid d} `{Monoid e},
      Monoid (a * b * c * d * e)) := {
  mappend := instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mappend ;
  mconcat := instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mconcat ;
  mempty := instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d____Monoid_e___Monoid__a___b___c___d___e___mempty }.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mempty `{Monoid
                                                                                                                  a}
                                                                                                                  `{Monoid
                                                                                                                  b}
                                                                                                                  `{Monoid
                                                                                                                  c}
                                                                                                                  `{Monoid
                                                                                                                  d} : a
                                                                                                                       *
                                                                                                                       b
                                                                                                                       *
                                                                                                                       c
                                                                                                                       *
                                                                                                                       d :=
  pair (pair (pair mempty mempty) mempty) mempty.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mappend `{Monoid
                                                                                                                   a}
                                                                                                                   `{Monoid
                                                                                                                   b}
                                                                                                                   `{Monoid
                                                                                                                   c}
                                                                                                                   `{Monoid
                                                                                                                   d}
    : a * b * c * d -> (a * b * c * d -> (a * b * c * d)) :=
  fun arg_214__ arg_215__ =>
    match arg_214__ , arg_215__ with
      | (pair (pair (pair a1 b1) c1) d1) , (pair (pair (pair a2 b2) c2) d2) => pair
                                                                               (pair (pair (mappend a1 a2) (mappend b1
                                                                                                                    b2))
                                                                                     (mappend c1 c2)) (mappend d1 d2)
    end.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mconcat `{Monoid
                                                                                                                   a}
                                                                                                                   `{Monoid
                                                                                                                   b}
                                                                                                                   `{Monoid
                                                                                                                   c}
                                                                                                                   `{Monoid
                                                                                                                   d}
    : (list (a * b * c * d)) -> (a * b * c * d) :=
  foldr
  instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mappend
  instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mempty.
Instance instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d__
  : !(forall `{Monoid a} `{Monoid b} `{Monoid c} `{Monoid d},
      Monoid (a * b * c * d)) := {
  mappend := instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mappend ;
  mconcat := instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mconcat ;
  mempty := instance__forall___Monoid_a____Monoid_b____Monoid_c____Monoid_d___Monoid__a___b___c___d___mempty }.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mempty `{Monoid
                                                                                                  a} `{Monoid b}
                                                                                                  `{Monoid c} : a * b *
                                                                                                                c :=
  pair (pair mempty mempty) mempty.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mappend `{Monoid
                                                                                                   a} `{Monoid b}
                                                                                                   `{Monoid c} : a * b *
                                                                                                                 c -> (a
                                                                                                                 * b *
                                                                                                                 c -> (a
                                                                                                                 * b *
                                                                                                                 c)) :=
  fun arg_219__ arg_220__ =>
    match arg_219__ , arg_220__ with
      | (pair (pair a1 b1) c1) , (pair (pair a2 b2) c2) => pair (pair (mappend a1 a2)
                                                                      (mappend b1 b2)) (mappend c1 c2)
    end.
Local Definition instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mconcat `{Monoid
                                                                                                   a} `{Monoid b}
                                                                                                   `{Monoid c} : (list
                                                                                                                 (a * b
                                                                                                                 *
                                                                                                                 c)) -> (a
                                                                                                                 * b *
                                                                                                                 c) :=
  foldr
  instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mappend
  instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mempty.
Instance instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c__
  : !(forall `{Monoid a} `{Monoid b} `{Monoid c}, Monoid (a * b * c)) := {
  mappend := instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mappend ;
  mconcat := instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mconcat ;
  mempty := instance__forall___Monoid_a____Monoid_b____Monoid_c___Monoid__a___b___c___mempty }.
Local Definition instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mempty `{Monoid
                                                                                  a} `{Monoid b} : a * b :=
  pair mempty mempty.
Local Definition instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mappend `{Monoid
                                                                                   a} `{Monoid b} : a * b -> (a *
                                                                                                    b -> (a * b)) :=
  fun arg_224__ arg_225__ =>
    match arg_224__ , arg_225__ with
      | (pair a1 b1) , (pair a2 b2) => pair (mappend a1 a2) (mappend b1 b2)
    end.
Local Definition instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mconcat `{Monoid
                                                                                   a} `{Monoid b} : (list (a * b)) -> (a
                                                                                                    * b) :=
  foldr instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mappend
  instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mempty.
Instance instance__forall___Monoid_a____Monoid_b___Monoid__a___b__
  : !(forall `{Monoid a} `{Monoid b}, Monoid (a * b)) := {
  mappend := instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mappend ;
  mconcat := instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mconcat ;
  mempty := instance__forall___Monoid_a____Monoid_b___Monoid__a___b___mempty }.
Instance instance_Monoid_unit : !Monoid unit := {
  mappend := instance_Monoid_unit_mappend ;
  mconcat := instance_Monoid_unit_mconcat ;
  mempty := instance_Monoid_unit_mempty }.

(* Unbound variables:
     * :: Eq None Some String bool comparison concatMap e flip list nil option pair
     true tt unit
*)