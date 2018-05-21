Require Import GHC.Base.
Require Import CoreFVs.
Require Import Id.
Require Import Core.
Require UniqFM.

Import GHC.Base.ManualNotations.

Require Import Proofs.GHC.Base.
Require Import Proofs.GHC.List.

Require Import Psatz.
Require Import Coq.Lists.List.
Require Import Coq.NArith.BinNat.

Import ListNotations.

Require Import Proofs.GhcTactics.
Require Import Proofs.Unique.
Require Import Proofs.Var.


Require Import Proofs.Base.
Require Import Proofs.ContainerAxioms.
Require Import IntSetProofs. 
(* Require Import IntMapProofs. *)

Require Import Proofs.VarSetFSet.

Import VarSetFSet.VarSetFacts.

Open Scope Z_scope.

Set Bullet Behavior "Strict Subproofs".


(** ** [VarSet] *)


Lemma delVarSet_remove : forall x s, delVarSet s x = remove x s.
tauto. Qed.

Lemma extendVarSet_add : forall x s, extendVarSet s x = add x s.
tauto. Qed.

Lemma unitVarSet_singleton : forall x, unitVarSet x = singleton x.
auto. Qed.

Lemma extendVarSetList_foldl' : forall x xs, 
    extendVarSetList x xs = 
    Foldable.foldl' (fun x y => add y x) x xs.
Proof.
  intros.
  unfold extendVarSetList,
         UniqSet.addListToUniqSet;
  replace UniqSet.addOneToUniqSet with 
      (fun x y => add y x).
  auto.
  auto.
Qed.

Lemma delVarSetList_foldl : forall vl vs,
    delVarSetList vs vl = Foldable.foldl delVarSet vs vl.
Proof. 
  induction vl.
  - intro vs. 
    unfold delVarSetList.
    unfold UniqSet.delListFromUniqSet.
    destruct vs.
    unfold UniqFM.delListFromUFM.
    unfold_Foldable_foldl.
    simpl.
    auto.
  - intro vs. 
    unfold delVarSetList in *.
    unfold UniqSet.delListFromUniqSet in *.
    destruct vs.
    unfold UniqFM.delListFromUFM in *.
    revert IHvl.
    unfold_Foldable_foldl.
    simpl.
    intro IHvl.
    rewrite IHvl with (vs:= (UniqSet.Mk_UniqSet (UniqFM.delFromUFM u a))).
    auto.
Qed.

Lemma mkVarSet_extendVarSetList : forall xs,
    mkVarSet xs = extendVarSetList emptyVarSet xs.
Proof.
  reflexivity.
Qed.


Ltac rewrite_extendVarSetList := 
  unfold extendVarSetList, UniqSet.addListToUniqSet;
  replace UniqSet.addOneToUniqSet with (fun x y => add y x) by auto.



(* This tactic rewrites the boolean functions into the 
   set properties to make them more suitable for fsetdec. *)

Ltac set_b_iff :=
  repeat
   progress
   rewrite <- not_mem_iff in *
  || rewrite <- mem_iff in *
  || rewrite <- subset_iff in *
  || rewrite <- is_empty_iff in *
  || rewrite delVarSet_remove in *
  || rewrite extendVarSet_add in *
  || rewrite empty_b in *
  || rewrite unitVarSet_singleton in *
  || rewrite_extendVarSetList
  || rewrite delVarSetList_foldl in *.


(*
Hint Rewrite <- F.not_mem_iff : set_b.
Hint Rewrite <- F.mem_iff : set_b.
Hint Rewrite <- F.subset_iff : set_b.
Hint Rewrite <- F.is_empty_iff : set_b.
Hint Rewrite delVarSet_remove : set_b.
Hint Rewrite extendVarSet_add : set_b.
Hint Rewrite empty_b : set_b.
*)

(**************************************)

(* Q: is there a way to do the automatic destructs safely? Sometimes 
   loses too much information. *)



Ltac unfold_VarSet :=
  unfold subVarSet,elemVarSet, isEmptyVarSet, 
         minusVarSet, extendVarSet, extendVarSetList in *;
  unfold UniqSet.elementOfUniqSet, 
         UniqSet.isEmptyUniqSet, 
         UniqSet.addOneToUniqSet,
         UniqSet.minusUniqSet,
         UniqSet.addListToUniqSet in *;
  try repeat match goal with
  | vs: VarSet, H : context[match ?vs with _ => _ end]  |- _ => destruct vs
  end;
  try repeat match goal with
  | vs: VarSet |- context[match ?vs with _ => _ end ] => destruct vs
  end;

  unfold UniqFM.addToUFM, 
         UniqFM.minusUFM, UniqFM.isNullUFM, 
         UniqFM.elemUFM in *;
  try repeat match goal with
  | u: UniqFM.UniqFM ?a, H : context[match ?u with _ => _ end]  |- _ => destruct u
  end;
  try repeat match goal with
  | u: UniqFM.UniqFM ?a |- context[match ?u with _ => _ end] => destruct u
  end. 

Ltac safe_unfold_VarSet :=
  unfold subVarSet,elemVarSet, isEmptyVarSet, 
         minusVarSet, extendVarSet, extendVarSetList in *;
  unfold UniqSet.elementOfUniqSet, 
         UniqSet.isEmptyUniqSet, 
         UniqSet.addOneToUniqSet,
         UniqSet.minusUniqSet,
         UniqSet.addListToUniqSet in *;
  unfold UniqFM.addToUFM, 
         UniqFM.minusUFM, UniqFM.isNullUFM, 
         UniqFM.elemUFM in *.

Lemma extendVarSetList_nil:
  forall s,
  extendVarSetList s [] = s.
Proof.
  intro s.
  set_b_iff.
  reflexivity.
Qed.

Lemma extendVarSetList_cons:
  forall s v vs,
  extendVarSetList s (v :: vs) = extendVarSetList (extendVarSet s v) vs.
Proof.
  intros.
  set_b_iff.
  unfold_Foldable_foldl'.
  reflexivity.
Qed.



Lemma extendVarSetList_append:
  forall s vs1 vs2,
  extendVarSetList s (vs1 ++ vs2) = extendVarSetList (extendVarSetList s vs1) vs2.
Proof.
  intros.
  set_b_iff.
  rewrite Foldable_foldl'_app.
  auto.
Qed.

Lemma elemVarSet_mkVarset_iff_In:
  forall v vs,
  elemVarSet v (mkVarSet vs) = true <->  List.In (varUnique v) (map varUnique vs).
Proof.
  intros.
  set_b_iff.
  remember (mkVarSet vs) as vss.
  unfold_VarSet.
  rewrite <- getUnique_varUnique.
  rewrite unique_In.
  set (key := (Unique.getWordKey (Unique.getUnique v))).
  (* Need theory about IntMap. *)
Admitted. 

Lemma lookupVarSet_extendVarSet_self:
  forall v vs,
  lookupVarSet (extendVarSet vs v) v = Some v.
Admitted.



Lemma elemVarSet_extendVarSet:
  forall v vs v',
  elemVarSet v (extendVarSet vs v') = (F.eqb v' v) || elemVarSet v vs.
Proof.
  intros.
  eapply MP.Dec.F.add_b.
Qed.
  
Lemma subVarSet_refl:
  forall vs1,
  subVarSet vs1 vs1 = true.
Proof.
  intros.
  set_b_iff.
  fsetdec.
Qed.

Lemma elemVarSet_unitVarSet: forall v1 v2,
  (elemVarSet v1 (unitVarSet v2) = true) <-> (varUnique v1 = varUnique v2).
Proof.
  intros v1 v2.
  set_b_iff.
  rewrite singleton_iff.
  unfold Var_as_DT.eqb.
  unfold_zeze.
Admitted.
  

Lemma elemVarSet_delVarSet: forall v1 fvs v2,
  elemVarSet v1 (delVarSet fvs v2) = true <-> (varUnique v1 <> varUnique v2 /\ elemVarSet v1 fvs = true).
Proof.
  intros.
  set_b_iff.
  set_iff.
  unfold Var_as_DT.eqb.
  unfold_zeze.
Admitted.

Lemma elemVarSet_false_true:
  forall v1 fvs v2,
  elemVarSet v1 fvs = false ->
  elemVarSet v2 fvs = true ->
  varUnique v1 <> varUnique v2.
Proof.
  intros v1 fvs v2.
  set_b_iff.
  intros.
Admitted.
  

Lemma subVarSet_elemVarSet_true:
  forall v vs vs',
  subVarSet vs vs' = true ->
  elemVarSet v vs = true ->
  elemVarSet v vs' = true.
Proof.
  intros v vs vs'.
  set_b_iff.
  fsetdec.
Qed.

Lemma subVarSet_elemVarSet_false:
  forall v vs vs',
  subVarSet vs vs' = true ->
  elemVarSet v vs' = false ->
  elemVarSet v vs = false.
Proof.
  intros v vs vs'.
  set_b_iff.
  fsetdec.
Qed.

Lemma subVarSet_extendVarSetList_l:
  forall vs1 vs2 vs,
  subVarSet vs1 vs2 = true ->
  subVarSet vs1 (extendVarSetList vs2 vs) = true.
Proof.
  intros vs1 vs2 vs.
  generalize vs2. clear vs2.
  induction vs.
  - intro vs2. rewrite extendVarSetList_nil. auto.
  - intro vs2. intro h. 
    rewrite extendVarSetList_cons. 
    rewrite IHvs. auto. 
    set_b_iff. fsetdec.
Qed.

Lemma subVarSet_extendVarSetList_r:
  forall vs vs1 vs2,
  subVarSet vs1 (mkVarSet vs) = true ->
  subVarSet vs1 (extendVarSetList vs2 vs) = true.
Proof.
  intros vs. 
  induction vs; intros vs1 vs2.
  - set_b_iff.
    unfold_Foldable_foldl'.
    simpl.
    fsetdec.
  - intro h. 
    rewrite mkVarSet_extendVarSetList in h.
    rewrite extendVarSetList_cons in *.
Admitted.
    
    
Lemma subVarSet_extendVarSet:
  forall vs1 vs2 v,
  subVarSet vs1 vs2 = true ->
  subVarSet vs1 (extendVarSet vs2 v) = true.
Proof.
  intros.
  set_b_iff.
  fsetdec.
Qed.


Lemma subVarSet_delVarSetList:
  forall vs1 vl,
  subVarSet (delVarSetList vs1 vl) vs1 = true.
Proof.
  intros.
  set_b_iff.
  generalize vs1. clear vs1. induction vl.
  - intros vs1. unfold_Foldable_foldl.
    simpl.
    fsetdec.
  - intros vs1. revert IHvl.
    unfold_Foldable_foldl.
    simpl.
    intro IH. 
    rewrite IH with (vs1 := delVarSet vs1 a).
    set_b_iff.
    fsetdec.
Qed.

Axiom disjointVarSet_mkVarSet:
  forall vs1 vs2,
  disjointVarSet vs1 (mkVarSet vs2) = true <->
  Forall (fun v => elemVarSet v vs1 = false) vs2.

Axiom disjointVarSet_subVarSet_l:
  forall vs1 vs2 vs3,
  disjointVarSet vs2 vs3 = true ->
  subVarSet vs1 vs2 = true ->
  disjointVarSet vs1 vs3 = true.

(** ** [InScopeVars] *)

Lemma getInScopeVars_extendInScopeSet:
  forall iss v,
  getInScopeVars (extendInScopeSet iss v) = extendVarSet (getInScopeVars iss) v.
Proof.
  intros.
  unfold getInScopeVars.
  unfold extendInScopeSet.
  destruct iss.
  reflexivity.
Qed.

Lemma getInScopeVars_extendInScopeSetList:
  forall iss vs,
  getInScopeVars (extendInScopeSetList iss vs) = extendVarSetList (getInScopeVars iss) vs.
Proof.
  intros.
  unfold getInScopeVars.
  unfold extendInScopeSetList.
  set_b_iff.
  destruct iss.
  unfold_Foldable_foldl'.
  unfold_Foldable_foldl.
  f_equal.
Qed.

(** ** [uniqAway] *)

Axiom isJoinId_maybe_uniqAway:
  forall s v, 
  isJoinId_maybe (uniqAway s v) = isJoinId_maybe v.



Lemma elemVarSet_uniqAway:
  forall v iss vs,
  subVarSet vs (getInScopeVars iss) = true ->
  elemVarSet (uniqAway iss v) vs = false.
Proof.
  intros.
  safe_unfold_VarSet.
  destruct vs.
  destruct iss.
  destruct v0.
  destruct u.
  destruct u0.
  simpl in *.
  unfold uniqAway.
  unfold elemInScopeSet.
  unfold elemVarSet.
  unfold uniqAway'.
  unfold realUnique.
Admitted.

