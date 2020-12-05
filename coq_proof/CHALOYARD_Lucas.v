(* TD9 - Sémantique petit pas                *)
(* Structural Operational Semantics (SOS)    *)


(* On importe les bibliothèques de Coq utiles pour le TD   *)

Require Import Bool Arith List.
Import List.ListNotations.

(* ========================================================================== *)
(** * Préliminaires *)
(** ** Syntaxe des expressions arithétiques *)

Inductive Aexp :=
| Aco : nat -> Aexp (** constantes *)
| Ava : nat -> Aexp (** variables *)
| Apl : Aexp -> Aexp -> Aexp
| Amu : Aexp -> Aexp -> Aexp
| Amo : Aexp -> Aexp -> Aexp
.

(** ** Syntaxe des expressions booléennes *)

Inductive Bexp :=
| Btrue : Bexp
| Bfalse : Bexp
| Bnot : Bexp -> Bexp
| Band : Bexp -> Bexp -> Bexp
| Bor : Bexp -> Bexp -> Bexp
| Beq : Bexp -> Bexp -> Bexp    (* test égalité de Bexp *)
| Beqnat : Aexp -> Aexp -> Bexp (* test égalité de Aexp *)
.

(** ** Syntaxe du langage impératif WHILE *)

Inductive Winstr :=
| Skip   : Winstr
| Assign : nat -> Aexp -> Winstr
| Seq    : Winstr -> Winstr -> Winstr
| If     : Bexp -> Winstr -> Winstr -> Winstr
| While  : Bexp -> Winstr -> Winstr
.

(* -------------------------------------------------- *)
(** ** États *)

Definition state := list nat.


Fixpoint get (x:nat) (s:state) : nat :=
match x,s with
| 0   , v::_      => v
| S x1, _::l1 => get x1 l1
| _   , _         => 0
end.


Fixpoint update (s:state) (v:nat) (n:nat): state :=
match v,s with
| 0   , a :: l1 => n :: l1
| 0   , nil     => n :: nil
| S v1, a :: l1 => a :: (update l1 v1 n)
| S v1, nil     => 0 :: (update nil v1 n)
end.

(* ----------------------------------------------- *)
(** ** Sémantique fonctionnelle de Aexp et de Bexp *)

Fixpoint evalA (a: Aexp) (s: state) : nat :=
  match a with
  | Aco n => n
  | Ava x => get x s
  | Apl a1 a2 =>  evalA a1 s + evalA a2 s
  | Amu a1 a2 =>  evalA a1 s * evalA a2 s
  | Amo a1 a2 =>  evalA a1 s - evalA a2 s
  end.

Definition eqboolb b1 b2 : bool :=
  match b1, b2  with
  | true , true  => true
  | false, false => true
  | _    , _     => false
  end.

Fixpoint eqnatb n1 n2 : bool :=
  match n1, n2 with
  | O    , O     => true
  | S n1', S n2' => eqnatb n1' n2'
  | _    , _     => false
  end.

Fixpoint evalB (b : Bexp) (s : state) : bool :=
  match b with
  | Btrue => true
  | Bfalse => false
  | Bnot b => negb (evalB b s)
  | Band e1 e2 => (evalB e1 s) && (evalB e2 s)
  | Bor e1 e2 => (evalB e1 s) || (evalB e2 s)
  | Beq e1 e2 => eqboolb (evalB e1 s) (evalB e2 s)
  | Beqnat n1 n2 => eqnatb (evalA n1 s) (evalA n2 s)
  end.


(* ========================================================================== *)

(** * SOS (Sémantique opérationnelle à petits pas) du langage While *)

Inductive config :=
| Inter : Winstr -> state -> config
| Final : state -> config.

(* La relation pour un pas de SOS *)

Inductive SOS_1: Winstr -> state -> config -> Prop :=
| SOS_Skip     : forall s,
                 SOS_1 Skip s (Final s)

| SOS_Assign   : forall x a s,
                 SOS_1 (Assign x a) s (Final (update s x (evalA a s)))

| SOS_Seqf     : forall i1 i2 s s1,
                 SOS_1 i1 s (Final s1) ->
                 SOS_1 (Seq i1 i2) s (Inter i2 s1)
| SOS_Seqi     : forall i1 i1' i2 s s1,
                 SOS_1 i1 s (Inter i1' s1) ->
                 SOS_1 (Seq i1 i2) s (Inter (Seq i1' i2) s1)

| SOS_If_true  : forall b i1 i2 s,
                 evalB b s = true  ->
                 SOS_1 (If b i1 i2) s (Inter i1 s)
| SOS_If_false : forall b i1 i2 s,
                 evalB b s = false ->
                 SOS_1 (If b i1 i2) s (Inter i2 s)

| SOS_While    : forall b i s,
                 SOS_1 (While b i) s (Inter (If b (Seq i (While b i)) Skip) s)
.

(** Fermeture réflexive-transitive de SOS_1 *)
(** Cette sémantique donne toutes les configurations atteignables
    par un (AST de) programme en partant d'un état initial.
 *)

Inductive SOS : config -> config -> Prop :=
| SOS_stop  : forall c, SOS c c
| SOS_again : forall i1 s1 c2 c3,
              SOS_1 i1 s1 c2 -> SOS c2 c3 ->
              SOS (Inter i1 s1) c3.


(* ========================================================================== *)

Definition N0 := Aco 0.
Definition N1 := Aco 1.
Definition N2 := Aco 2.
Definition N3 := Aco 3.
Definition N4 := Aco 4.


(** * I *)

(** *** Calcul du carré avec des additions *)
(** On code dans While un programme Pcarre correspondant à
    while not (i=n) do {i:= 1+i; x:= y+x ; y:= 2+y} *)
Definition Il := 0.
Definition Ir := Ava Il.
Definition Xl := 1.
Definition Xr := Ava Xl.
Definition Yl := 2.
Definition Yr := Ava Yl.

Definition incrI := Assign Il (Apl N1 Ir).
Definition incrX := Assign Xl (Apl Yr Xr).
Definition incrY := Assign Yl (Apl N2 Yr).
Definition corps_carre := Seq incrI (Seq incrX incrY).
Definition Pcarre_2 := While (Bnot (Beqnat Ir (Aco 2))) corps_carre.
Definition Pcarre n := While (Bnot (Beqnat Ir (Aco n))) corps_carre.
(** Nouveau : on peut jouer avec des programmes qui bouclent *)
Definition Pcarre_inf := While Btrue corps_carre.


Lemma Sn_2 n : S n + S n = S (S (n + n)).
Proof. ring. Qed.

Lemma Sn_carre n : S n * S n = S (n + n + n * n).
Proof. ring. Qed.

(* pour me simplifier la fin *)
Lemma Sn_special n : n + n * S n = n + n + n * n.
Proof. ring. Qed.


Lemma SOS_Pcarre_2_1er_tour : SOS (Inter Pcarre_2 [0;0;1]) (Inter Pcarre_2 [1; 1; 3]).
Proof.
  eapply SOS_again.
  { apply SOS_While. }
  eapply SOS_again.
  { cbv. refine (SOS_If_true _ _ _ _ _ ). cbn. reflexivity. }
  { refine (SOS_again _ _ _ _ _ _).
    { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
    { refine (SOS_again _ _ _ _ _ _).
      { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
      { cbn. refine (SOS_again _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _).
        { cbn. refine(SOS_Assign _ _ _). }
        { cbn. apply SOS_stop. }}}}
Qed.

Theorem SOS_Pcarre_inf_1er_tour : SOS (Inter Pcarre_inf [0;0;1]) (Inter Pcarre_inf [1; 1; 3]).
Proof.
   refine (SOS_again _ _ _ _ _ _).
  { cbv. apply SOS_While. }
  { refine (SOS_again _ _ _ _ _ _).
    { refine (SOS_If_true _ _ _ _ _). cbn. reflexivity. }
    { refine (SOS_again _ _ _ _ _ _).
      { refine(SOS_Seqi _ _ _ _ _ _). refine(SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
      { cbn. refine (SOS_again _ _ _ _ _ _).
        { refine(SOS_Seqi _ _ _ _ _ _). refine(SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
        { cbn. refine(SOS_again _ _ _ _ _ _).
          { refine(SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
          { cbn. apply SOS_stop. }}}}}
Qed.

Theorem SOS_Pcarre_2_V0 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
  eapply SOS_again.
  { apply SOS_While. }
  eapply SOS_again.
  { cbv. refine (SOS_If_true _ _ _ _ _ ). cbn. reflexivity. }
  { refine (SOS_again _ _ _ _ _ _).
    { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
    { refine (SOS_again _ _ _ _ _ _).
      { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
      { cbn. refine (SOS_again _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _).
        { cbn. refine(SOS_Assign _ _ _). }
        { cbn. refine(SOS_again _ _ _ _ _ _).
          { apply SOS_While. }
          eapply SOS_again.
          { cbv. refine (SOS_If_true _ _ _ _ _ ). cbn. reflexivity. }
          { refine (SOS_again _ _ _ _ _ _).
            { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
            { refine (SOS_again _ _ _ _ _ _).
              { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
              { cbn. refine (SOS_again _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _).
                { cbn. refine(SOS_Assign _ _ _). }
                { cbn. refine (SOS_again _ _ _ _ _ _).
                  { apply SOS_While. }
                  { refine (SOS_again _ _ _ _ _ _).
                    { refine (SOS_If_false _ _ _ _ _). cbn. reflexivity. }
                    { refine (SOS_again _ _ _ _ _ _).
                      { apply SOS_Skip. }
                      { apply SOS_stop. }}}}}}}}}}}
Qed.

(** Le but de la suite est d'éviter les redites, puis éventuellement
    de considérer le cas général Pcarre. *)

(** Propriété essentielle de SOS, qui a un intérêt pratique. *)
Theorem SOS_trans : forall c1 c2 c3, SOS c1 c2 -> SOS c2 c3 -> SOS c1 c3.
Proof.
  intros c1 c2 c3.
  intros h1 h2.
  induction h1 as [].
  - apply h2.
  - refine (SOS_again _ _ _ _ _ _).
    -- apply H.
    -- apply IHh1. apply h2.
Qed.

(** Il n'est pas demandé de faire celui-ci
    (bien qu'un copié-collé d'un lemme précédent fonctionne). *)
(* Ce théorème prouve que la 2eme itération de Pcare_2 (initialisé avec [1;1;3]) donnera
   le state suivant : [2; 4; 5] *)
Lemma SOS_Pcarre_2_2e_tour : SOS (Inter Pcarre_2 [1; 1; 3]) (Inter Pcarre_2 [2; 4; 5]).
Proof.
  eapply SOS_again.
  { apply SOS_While. }
  eapply SOS_again.
  { cbv. refine (SOS_If_true _ _ _ _ _ ). cbn. reflexivity. }
  { refine (SOS_again _ _ _ _ _ _).
    { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
    { refine (SOS_again _ _ _ _ _ _).
      { refine (SOS_Seqi _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _). refine (SOS_Assign _ _ _). }
      { cbn. refine (SOS_again _ _ _ _ _ _). refine (SOS_Seqf _ _ _ _ _).
        { cbn. refine(SOS_Assign _ _ _). }
        { cbn. apply SOS_stop. }}}}
Qed.

(* Ce théorème permet de démontrer que Pcarre_2 termine lorsqu'il arrive au state :
   [2; 4; 5] *)
Theorem SOS_Pcarre_2_fini : SOS (Inter Pcarre_2 [2; 4; 5]) (Final [2; 4; 5]).
Proof.
   eapply SOS_again.
  { apply SOS_While. }
  eapply SOS_again.
  { cbv. refine (SOS_If_false _ _ _ _ _ ). cbn. reflexivity. }
  { refine (SOS_again _ _ _ _ _ _).
    { apply SOS_Skip. }
    { apply SOS_stop. }}
Qed.

(** Même énoncé que SOS_Pcarre_2_V0. Utiliser SOS_trans *)
Theorem SOS_Pcarre_2_fin_V1 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
   apply SOS_trans with (Inter Pcarre_2 [1; 1; 3]).
  { apply SOS_Pcarre_2_1er_tour. }
  { apply SOS_trans with (Inter Pcarre_2 [2;4;5]).
    {  apply SOS_Pcarre_2_2e_tour. }
    {  apply SOS_Pcarre_2_fini. }}
Qed.

(** Généralisation à Pcarre *)

(** On a besoin de deux lemmes arithmétiques, démontrables avec la tactique ring. *)


Definition invar_cc n := [n; n*n; S (n+n)].
(* Ce théorème démontre qu'une itération de corps_carre va modifier le state de la manière
   suivante : [i; i*i; S (i+i)] =====> [i + 1; (i+1)*(i+1); S((i+1)+(i+1)) ],
   et que corps_carre s'arrêtera là *)
Theorem SOS_corps_carre n : SOS (Inter corps_carre (invar_cc n)) (Final (invar_cc (S n))).
Proof.
   induction n as [].
  { refine (SOS_again _ _ _ _ _ _).
    { cbv. refine (SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
    { refine (SOS_again _ _ _ _ _ _).
      { refine (SOS_Seqf _ _ _ _ _). cbn. apply SOS_Assign. }
      { refine (SOS_again _ _ _ _ _ _).
        { cbn. apply SOS_Assign. }
        { cbn. cbv. apply SOS_stop. }}}}
  { refine (SOS_again _ _ _ _ _ _).
    { cbv[corps_carre]. cbv[incrI incrX incrY]. refine (SOS_Seqf _ _ _ _ _). cbv[Il N1 Ir].
      apply SOS_Assign. }
    { cbv[Xl Yr Xr N2 Yl]. (* ok *) refine (SOS_again _ _ _ _ _ _).
      { refine (SOS_Seqf _ _ _ _ _). cbn. apply SOS_Assign. }
      { cbn. (* ok 2 *) refine (SOS_again _ _ _ _ _ _).
        { apply SOS_Assign. }
        { cbn. cbv[invar_cc]. rewrite Sn_2. cbn[Nat.add]. rewrite Sn_carre. cbn[Nat.add]. rewrite Sn_carre. rewrite Sn_special. apply SOS_stop. }}}}
Qed.

(** Celui-ci est court mais difficile. Laisser Admitted au début. *)
(* Si une certaine instruction donne un state s, alors
   rajouter une instruction à cette séquence donnera le state
   de l'instruction i2 appliqué à s 
*)
Fixpoint SOS_seq i1 i2 s1 s2 (so : SOS (Inter i1 s1) (Final s2)) :
  SOS (Inter (Seq i1 i2) s1) (Inter i2 s2).
Proof.
 
Admitted.

(** Réutiliser les lemmes précédents (facile et très court). *)
(* Ce théorème est similaire au précedent, mais à la différence qu'ici
   on montre que cette propriétè SOS_corps_carre est respecté même
   si on y ajoute une instruction quelconque au bout de corps_carre *)
Lemma SOS_corps_carre_inter n i :
  SOS (Inter (Seq corps_carre i) (invar_cc n)) (Inter i (invar_cc (S n))).
Proof.
  apply SOS_seq.
  apply SOS_corps_carre. 
Qed.

Lemma eqnatb_refl : forall n, eqnatb n n = true.
Proof.
  induction n as [].
  - cbn. reflexivity.
  - cbn. apply IHn.
Qed.

(** Réutiliser les lemmes précédents (facile). *)
(* Ce théorème montre que le programme Pcarre n, vérifie bien le changement de state
   suivant : [i; i*i; S (i+i)] =====> [i + 1; (i+1)*(i+1); S((i+1)+(i+1)) ]
   si i est différent de n. *)
Lemma SOS_Pcarre_tour :
  forall n i, eqnatb i n = false ->
  SOS (Inter (Pcarre n) (invar_cc i)) (Inter (Pcarre n) (invar_cc (S i))).
Proof.
   intros n0 i0 h0.
  cbv[Pcarre]. cbv[Ir]. cbv[Il].
  refine (SOS_again _ _ _ _ _ _).
  { refine (SOS_While _ _ _). }
  refine (SOS_again _ _ _ _ _ _).
  { refine (SOS_If_true _ _ _ _ _). cbn. rewrite h0. reflexivity. }
  { apply SOS_corps_carre_inter. }
Qed.

(** Facile *)
(* Ce théorème permet de démontrer que Pcarre n termine bien lorsque on 
   est au state : (invar_cc n) . *)
Theorem SOS_Pcarre_n_fini :
  forall n, SOS (Inter (Pcarre n) (invar_cc n)) (Final (invar_cc n)).
Proof.
    intro n0.
  cbv[Pcarre].
  refine (SOS_again _ _ _ _ _ _).
  { cbv[Ir]. cbv[Il]. refine(SOS_While _ _ _). }
  refine (SOS_again _ _ _ _ _ _).
  { refine (SOS_If_false _ _ _ _ _). cbn. rewrite eqnatb_refl. cbn. reflexivity. }
  refine (SOS_again _ _ _ _ _ _).
  { apply SOS_Skip. }
  { apply SOS_stop. } 
Qed.

(* Cette démonstration a lieu en 3 etapes :
   - 1er itération : On passe du state [0;0;1] à [1; 1; 3] (utilisation de
   SOS_trans pour dire qu'on souhaite passer par [1; 1; 3]
   grâce à l'utilisation de SOS_Pcarre_tour qui permet d'effectuer une
   itération
   - 2eme itération : On passe du state [1;1;3] à [2; 4; 5] (utilisation de
   SOS_trans pour dire qu'on souhaite passer par [2; 4; 5]
   grâce à l'utilisation de SOS_Pcarre_tour qui permet d'effectuer une
   itération
   - FIN : Maintenant qu'on est arrivé à l'état [2; 4; 5], on utilise 
   notre théorème SOS_trans pour arrivé à l'état FINAL([2;4;5]),
   pour prouver que Inter (Pcarre 2) (invar_cc 2)) aboutit bien à
   Final (invar_cc 2), on utilise le théorème SOS_Pcarre_n_fini.
   Puis il suffit de terminer avec SOS_stop.
   *)
Theorem SOS_Pcarre_2_fin_V2 : SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
  eapply SOS_trans.
  { apply SOS_Pcarre_tour. reflexivity. }
  eapply SOS_trans.
  { apply SOS_Pcarre_tour. reflexivity. }
  eapply SOS_trans.
  { apply SOS_Pcarre_n_fini. }
  apply SOS_stop.
Qed.

Fixpoint eqnatb_true_relf n i : eqnatb i n = true -> n = i.
  refine (
      match n, i with
      | 0, 0 => fun e => _
      | S n', S i' => fun e => _
      | x, y => fun e => _
      end).
  - reflexivity.
  - cbn[eqnatb] in e. discriminate e.
  - cbn[eqnatb] in e. discriminate e.
  - cbn[eqnatb] in e. pose (H := eqnatb_true_relf n' i'). rewrite H.
    { reflexivity. }
    { apply e. }
Qed.

(** On peut dire des choses sur la version qui boucle. *)
(* Ce théorème signifie la même chose que le théorème SOS_Pcarre_tour
   mais dans le cas ou corps_carre va boucler à l'infini , d'où le fait
   que nous n'avons pas besoin d'avoir (eqnatb i n != false), étant donné
   que Pcarre_inf n'a pas de fin (habituellement défini par un n) *)
Lemma SOS_Pcarre_inf_tour :
  forall i,
  SOS (Inter Pcarre_inf (invar_cc i)) (Inter Pcarre_inf (invar_cc (S i))).
Proof.
  intro i0.
   refine (SOS_again _ _ _ _ _ _).
   { cbv[Pcarre_inf]. apply SOS_While. }
   { refine (SOS_again _ _ _ _ _ _).
     { refine (SOS_If_true _ _ _ _ _). cbn. reflexivity. }
     { refine (SOS_again _ _ _ _ _ _).
       { refine(SOS_Seqi _ _ _ _ _ _). refine(SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
       { cbn. refine (SOS_again _ _ _ _ _ _).
         { refine(SOS_Seqi _ _ _ _ _ _). refine(SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
         { cbn. refine(SOS_again _ _ _ _ _ _).
           { refine(SOS_Seqf _ _ _ _ _). apply SOS_Assign. }
           { cbn. cbv[invar_cc]. rewrite Sn_2. cbn[Nat.add]. rewrite Sn_carre. cbn[Nat.add]. apply SOS_stop. }}}}}
Qed.

(* Ce théorème permet de prouver que pour n'importe quel i (appartenent au entier naturel)
   , si on a lancé le programme Pcarre_inf avec le state [0; 0; 1], on pourra toujours atteindre
   un state : (invar_cc i)
*)
Theorem SOS_Pcarre_inf_n :
  forall i,
  SOS (Inter Pcarre_inf [0; 0; 1]) (Inter Pcarre_inf (invar_cc i)).
Proof.
  intro i.
  induction i as [].
  - cbv[invar_cc]. cbn. apply SOS_stop.
  - apply SOS_trans with (Inter Pcarre_inf (invar_cc i )).
    -- apply IHi.
    -- apply SOS_Pcarre_inf_tour.
Qed.

(** Énoncer et démontrer le théorème général pour Pcarre *)


(* Theoreme sur les entiers pour prouver notre théorème général *)
Lemma SSki_SkSi : forall i k, S (S (k + i)) = S (k + (S i)).
Proof.
  intros i k.
  ring.
Qed.
    
Lemma i_Si_dif : forall i, eqnatb i (S i) = false.
Proof.
  intro i.
  induction i as [].
  - cbn. reflexivity.
  - cbn. apply IHi.
Qed.

Lemma i_Si_diff_2 : forall i k, eqnatb i (S (S (k + i))) = false.
Proof.
  intros i k.
  induction i as [].
  - cbn. reflexivity.
  - cbn. rewrite <- SSki_SkSi. apply IHi.
Qed.
    
Lemma sk_i_diff_n_ind : forall n i k, (S k) + i = n -> eqnatb i n = false.
Proof.
  intros n i k.
  intro h0.
  induction k as [].
  - rewrite <- h0. cbn. apply i_Si_dif.
  - rewrite <- h0. cbn. apply i_Si_diff_2.
Qed.

Theorem Sn_i_egale_n_Si : forall i k,  S k + i = k + S i.
Proof.
  intros i k.
  induction k as [].
  - cbn. reflexivity.
  - cbn. rewrite <- IHk. cbn. reflexivity.
Qed.

Theorem pred_Sn_n : forall n, eqnatb 0 n = false -> S (pred n) = n.
Proof.
  intros n h0.
  induction n as [].
  - cbn in h0. discriminate h0.
  - cbn. reflexivity.
Qed.

Fixpoint SOS_Pcarre_n_general_aux i k {struct k} : SOS (Inter (Pcarre (i + k)) (invar_cc i)) (Inter (Pcarre (i + k)) (invar_cc (i + k))).
  refine(
      match k with
      | 0 => _
      | S k' => _
      end).
  - cbn. rewrite plus_0_r. apply SOS_stop.
  - apply SOS_trans with ((Inter (Pcarre (S i + k')) (invar_cc (S i)))).
    { rewrite <- plus_Snm_nSm. apply SOS_Pcarre_tour. apply sk_i_diff_n_ind with k'. rewrite Sn_i_egale_n_Si. rewrite plus_comm. reflexivity. }
    { rewrite <- plus_Snm_nSm. pose (H := SOS_Pcarre_n_general_aux (S i) k'). apply H. }
Qed.

(** Énoncer et démontrer le théorème général pour Pcarre *)
Theorem SOS_Pcarre_n_general : forall n, SOS (Inter (Pcarre n) (invar_cc 0)) (Final (invar_cc n)).
Proof.
  pose (i := 0).
  change (forall n : nat, SOS (Inter (Pcarre n) (invar_cc i)) (Final (invar_cc n))).
  intro n0.
  apply SOS_trans with ((Inter (Pcarre n0) (invar_cc n0))).
  { case_eq(eqnatb i n0).
    { intro h0. apply eqnatb_true_relf in h0. rewrite h0. apply SOS_stop. }
    { intro h0. apply SOS_trans with ((Inter (Pcarre n0) (invar_cc (S i)))). 
      { apply SOS_Pcarre_tour. apply h0. }
      { pose (H := SOS_Pcarre_n_general_aux 1 (pred n0)). cbn in H. rewrite pred_Sn_n in H.
        { unfold i. apply H. }
        { unfold i in h0. apply h0. }}}}
  apply SOS_Pcarre_n_fini.
Qed.
 
(* ========================================================================== *)


(** * II *)


(** ** Définir une version fonctionnelle de SOS_1 *)
Fixpoint f_SOS_1 (i : Winstr) (s : state) : config :=
  match i with
  | Skip => Final s
  | Assign n a1 => Final (update s n (evalA a1 s))
  | Seq i1 i2 => (match (f_SOS_1 i1 s) with
                 | Final s' => Inter i2 s'
                 | Inter i1' s' => Inter (Seq i1' i2) s'
                 end)
  | If b1 i1 i2 => (match (evalB b1 s) with
                   | true => Inter i1 s
                   | false => Inter i2 s
                   end)
  | While b1 i1 => Inter (If b1 (Seq i1 (While b1 i1)) Skip) s
  end.

(** ** Utilisation de f_SOS_1 pour éviter les eapply SOS_again *)

(** PC = pt de contrôle *)
Definition PC0 := Pcarre_2.
Eval cbn in (f_SOS_1 PC0 [0;0;1]).

(** Il faut un peu désosser le code pour y retrouver les points de contrôle *)

Definition PC2 := Seq corps_carre PC0.
Definition PC1 := If (Bnot (Beqnat Ir (Aco 2))) PC2 Skip.

(** Ici on note toutes les étapes du 1er tour de Pcarre_2 grâce à notre "oracle"
    et on vérifie que cela concorde bien.
    Eval : Permet de "prédire" la prochaine étape
    Et on déclare un Fact afin de vérifier que les résultats concordent **)
(** On vérifie la progression *)
Fact fa1 : f_SOS_1 PC0 [0;0;1] = Inter PC1 [0;0;1]. reflexivity. Qed.
Eval cbn in (f_SOS_1 PC1 [0;0;1]).
(** Continuer, on retombe sur PC0 après quelques étapes. *)
Fact fa2 : f_SOS_1 PC1 [0;0;1] = Inter PC2 [0;0;1]. reflexivity. Qed.
Eval cbn in (f_SOS_1 PC2 [0;0;1]).

Fact fa3 : f_SOS_1 PC2 [0;0;1] = Inter (Seq (Seq incrX incrY) PC0) [1;0;1]. reflexivity. Qed.
Eval cbn in ((f_SOS_1 (Seq (Seq incrX incrY) PC0)) [1;0;1]).

Fact fa4 : f_SOS_1 (Seq (Seq incrX incrY) PC0) [1;0;1] = Inter (Seq incrY PC0) [1; 1; 1]. reflexivity.
Qed.
Eval cbn in (f_SOS_1 (Seq incrY PC0)) [1;1;1].

Fact fa5 : f_SOS_1 (Seq incrY PC0) [1;1;1] = Inter PC0 [1;1;3]. reflexivity. Qed.

(** Utilisation sur un lemme SOS *)
Lemma SOS_Pcarre_2_1er_tour_V1 :
  SOS (Inter Pcarre_2 [0;0;1]) (Inter Pcarre_2 [1; 1; 3]).
Proof.
  change Pcarre_2 with PC0.
  apply SOS_again with (Inter PC1 [0;0;1]).
  { apply SOS_While. }
  { apply SOS_again with (Inter PC2 [0;0;1]).
    { apply SOS_If_true. cbn. reflexivity. }
    { apply SOS_again with ( Inter (Seq (Seq incrX incrY) PC0) [1;0;1]).
      { apply SOS_Seqi. apply SOS_Seqf. apply SOS_Assign. }
      { apply SOS_again with (Inter (Seq incrY PC0) [1; 1; 1]).
        { apply SOS_Seqi. apply SOS_Seqf. apply SOS_Assign. }
        { apply SOS_again with (Inter PC0 [1;1;3]).
          { apply SOS_Seqf. apply SOS_Assign. }
          { apply SOS_stop. }}}}}
Qed.

(** ** Théorèmes généraux reliant SOS_1 et f_SOS_1 *)

(** Court mais non trivial. *)
Lemma f_SOS_1_corr : forall i s, SOS_1 i s (f_SOS_1 i s).
Proof.
  intros i s.
  induction i as []; cbn[f_SOS_1].
  - apply SOS_Skip.
  - apply SOS_Assign.
  - destruct (f_SOS_1 i1 s) as [].
    -- apply SOS_Seqi. apply IHi1.
    -- apply SOS_Seqf. apply IHi1.
  - case_eq (evalB b s).
    -- intro h. apply SOS_If_true. apply h.
    -- intro h. apply SOS_If_false. apply h.
  - apply SOS_While.
Qed.

(** Court. Attention : utiliser la tactique injection. *)
Lemma f_SOS_1_compl : forall i s c, SOS_1 i s c -> c = f_SOS_1 i s.
Proof.
  intros i s c h.
  induction h as []; cbn.
  - reflexivity.
  - reflexivity.
  - rewrite <- IHh. reflexivity.
  - rewrite <- IHh. reflexivity.
  - rewrite H. reflexivity.
  - rewrite H. reflexivity.
  - reflexivity.
Qed.




