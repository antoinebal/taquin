%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de façon synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

%recherche et suppression de l‘état Uqui minimise fU)
%UFMin etat de f minimal 
recherche_et_suppr_f_u_min(Avl, NewAvl, UFMin):-
	suppress_min([_,UFMin] , Avl, NewAvl).

%peut etre tout récupérer au lieu de seulement U : suppress_min(UFMin , Avl, NewAvl).
	
%pour chaque successeur d’un étatUqui vient d’être développé, test d'appartenanceà P et test d'appartenanceà Q	


	

main :-
	% initialisations Pf, Pu et Q 
	initial_state(S0),
	empty(Pu0),
	empty(Pf0),
	empty(Q),
	heuristique(S0, H0),
	insert([[0, H0, 0], S0], Pf0, Pf),
	insert([S0, [0, H0, 0], nil, nil], Pu0, Pu),
	aetoile(Pf,Pu,Q).
	% lancement de Aetoile

	   %********
			% A FAIRE
			%********



%*******************************************************************************

expand(Successeurs, [Fs, Hs, Gs], Gu):-
	final_state(Sf),
	findall(H, (member(S, Successeurs), heuristique(S, H)), Hs),
	findall(G, (member(S, Successeurs), G is Gu+1), Gs),
	sommeListe(Gs, Hs, Fs).
	
sommeListe([], [], []).
sommeListe([H1|T1], [H2|T2], [H3|T3]) :-
	H3 is H2+H1,
	sommeListe(T1, T2, T3).

loop_successors([],_,_,_,_,_,_,_,_,_).
%on a besoin des actions, du pere
loop_successors([S1|Succ], [[F|Fs], [H|Hs], [G|Gs]], Pu0, Pf0, Q0, Pu, Pf, Q, Pere, [Action|Suite]) :-
	traiter_successeur(S1, [F, H, G], Pu0, Pf0, Q0, Pu1, Pf1, Q1, Pere, Action),
	loop_successors(Succ, [Fs, Hs, Gs], Pu1, Pf1, Q1, Pu, Pf, Q, Pere, Suite).
%%
%traiter_successeur(Succ, [F, H, G], Pu0, Pf0, Q0, Pu1, Pf1, Q1, Pere, Action) :-
%	belongs([Succ, _, _, _], Q0). % ON a deja dev l'etat : on passe au successeur suivant !

%traiter_successeur(Succ, [FNew, HNew, GNew], Pu0, Pf0, Q0, Pu1, Pf1, Q1, Pere, Action) :-
%	belongs([Succ, [FOld,HOld,GOld], _, _],Pu0),
	%compare les états et met à jour Pu et Pf si cela vaut le coup	
	%todo : peut être le faire avec un if
%	compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pu1, Pere, Action).

%% 
traiter_successeur(Succ, [FNew, HNew, GNew], Pu0, Pf0, Q0, Pu1, Pf1, Q1, Pere, Action) :-
	(belongs([Succ, _, _, _], Q0) -> true %IF
		; %else 
	 	(belongs([Succ, [FOld,HOld,GOld], _, _],Pu0) -> %if
								compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pu1, Pere, Action) 
			; %% else des deux du coup , insert ds Pf et pu 
		insert([Succ,[FNew, HNew, GNew],Pere, Action],Pu0,Pu1),
		insert([[FNew, HNew, GNew],Succ],Pf0,Pf1)
			)
		) .







%dans le cas où le nouvel état n'est pas mieux que l'ancien
compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu0, Pf0, Pf0, Pere, Action) :-
	FOld < FNew.

%cas où les f sont égaux: on compare les h
compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu0, Pf0, Pf0, Pere, Action) :-
	FOld = FNew,
	HOld < HNew.

compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) :-
	FOld > FNew,
	
	% on le supprime dans Pu et on le remet avec les nouvelles valeurs
	suppress([Succ, _, _, _], Pu0, AuxPu),
	insert([Succ, [FNew, HNew, GNew], Pere, Action], Aux, Pu1),
	
	% on le supprime dans Pf et on le remet avec les nouvelles valeurs
	suppress([_, Succ], Pf0, AuxPf),
	insert([[FNew, HNew, GNew], Succ], AuxPf, Pf1).

compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) :-
	FOld = FNew,
	HOld > HNew,

	% on le supprime dans Pu et on le remet avec les nouvelles valeurs
	suppress([Succ, _, _, _], Pu0, AuxPu),
	insert([Succ, [FNew, HNew, GNew], Pere, Action], Aux, Pu1),
	
	% on le supprime dans Pf et on le remet avec les nouvelles valeurs
	suppress([_, Succ], Pf0, AuxPf),
	insert([[FNew, HNew, GNew], Succ], AuxPf, Pf1).
	
	
	

%affiche 
affiche_solution(Q, U, NumeroEtape) :- 
	belongs([U, _, Pere, Action], Q),
	NE is NumeroEtape+1,
	afficheSolution(Q, Pere, NE),
	writeln("Etape "),
	write(NumeroEtape),
	write(" : "),
	writeln("Action : "),
	write(Action),
	writeln(U).
	
	


aetoile(nil, nil, _) :-
	writeln("PAS de SOLUTION: L’ETAT FINAL N’EST PAS ATTEIGNABLE!").

aetoile(Pf, _, _):-
	final_state(Sf),
	recherche_et_suppr_f_u_min(Pf, _, UFMin),
	UFMin=Sf,
	affiche_solution(Q, Sf, 1).

aetoile(Pf, Pu, Qs) :-
	%on cherche et supprime le u de f min
	recherche_et_suppr_f_u_min(Pf, NewPf, UFMin),
	 %on supprime le noeud frère dans Pu
	suppress([UFMin, [FUFMin,HUFMin,Gu], PereUFMin, ActionUFMin], Pu, NewPs),
	
	%on cherche les successeurs
	successeursEtActions(UFMin, [Successeurs, Actions]),
	expand(Successeurs, [Fs,Gs,Hs] , Gu),
	loop_successors(Successeurs, [Fs, Hs, Gs], Pu, Pf, Q, Pu1, Pf1, Q1, UFMin, Actions),
	
	%U ayant été développé et supprimé de P, il reste à l’insérer le nœud [U,Val,...,..] dans Q,
	insert([UFMin,[FUFMin,HUFMin,Gu], PereUFMin, ActionUFMin], Q1, Q2),
	aetoile(Pf1,Pu1,Q2).

	
   
