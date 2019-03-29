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
	- appelle recursivement etoile avec les nouvelles valeurs Pf2, NewPs, NewQs

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
	insert([[H0, H0, 0], S0], Pf0, Pf), % On insère les noeuds initiaux, f=g+H avec g=0
	insert([S0, [H0, H0, 0], nil, nil], Pu0, Pu),
	
	% lancement de Aetoile	

	aetoile(Pf,Pu,Q).



%*******************************************************************************
% fonction utile pour récupérer une liste d'Actions et une liste de Successeurs
% partir d'une liste de la forme [Successeur, Action]|Reste
splitSetA([],[],[]).
splitSetA([[Hs,Ha]|T], [Hs|LS], [Ha|LA] ) :-
	 splitSetA(T,LS,LA). 
	

expand(Successeurs, [Fs, Hs, Gs], Gu):- 
	%calcul de H pour chaque successeur                                
	findall(H, (member(S, Successeurs), heuristique(S, H)), Hs),
	%calcul de G pour chaque successeur	
	findall(G, (member(S, Successeurs), G is Gu+1), Gs),
	%calcul de F en faisant G+H pour chaque successeur
	sommeListe(Gs, Hs, Fs).
	
% sommeListe(L1, L2, L3), pour tout i, L3[i] = L1[i]+L2[i]
sommeListe([], [], []).
sommeListe([H1|T1], [H2|T2], [H3|T3]) :-
	H3 is H2+H1,
	sommeListe(T1, T2, T3).

loop_successors([],[[],[],[]],Pu,Pf,Pu,Pf, _, _, _). % y'avait [] pour F,HG et actions, false direct, avec _ met quelques trucs avant ([[],[],[]] pour F,H,H c'est ok)
%on a besoin des actions, du pere
loop_successors([S1|Succ], [[F|Fs], [H|Hs], [G|Gs]], Pu0, Pf0, Pu, Pf, Q, Pere, [Action|Suite]) :-
	traiter_successeur(S1, [F, H, G], Pu0, Pf0, Pu1, Pf1, Q, Pere, Action),
	loop_successors(Succ, [Fs, Hs, Gs], Pu1, Pf1, Pu, Pf, Q, Pere, Suite).
%%
%traiter_successeur(Succ, [F, H, G], Pu0, Pf0, Q0, Pu1, Pf1, Q1, Pere, Action) :-
%	belongs([Succ, _, _, _], Q0). % ON a deja dev l'etat : on passe au successeur suivant !

%traiter_successeur(Succ, [FNew, HNew, GNew], Pu0, Pf0, Q0, Pu1, Pf1, Q1, Pere, Action) :-
%	belongs([Succ, [FOld,HOld,GOld], _, _],Pu0),
	%compare les états et met à jour Pu et Pf si cela vaut le coup	
	%todo : peut être le faire avec un if
%	compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pu1, Pere, Action).

%% 
traiter_successeur(Succ, [FNew, HNew, GNew], Pu0, Pf0, Pu1, Pf1, Q, Pere, Action) :-
	(belongs([Succ, _, _, _], Q) -> Pu1 = Pu0 ,
									Pf1 = Pf0						 %IF ce successeur est connu dans Q, on l'oublie, on renvoi les arbres déjà présents
		; %else 
	 	(belongs([Succ, [FOld,HOld,GOld], _, _],Pu0) -> %if S connu dans Pu -> garde la meilleure eval ds pf et pu 
								compareEtats(Succ, FOld, HOld, GOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) 
			; %% else des deux du coup , insert ds Pf et pu 
		insert([Succ,[FNew, HNew, GNew],Pere, Action],Pu0,Pu1),
		insert([[FNew, HNew, GNew],Succ],Pf0,Pf1)
			)
		) .


%dans le cas où le nouvel état n'est pas mieux que l'ancien
compareEtats(_, FOld, _, _, FNew, _, _, Pu0, Pu0, Pf0, Pf0, _, _) :-
	FOld < FNew.

%cas où les f sont égaux: on compare les h
compareEtats(_, FOld, HOld, _, FNew, HNew, _, Pu0, Pu0, Pf0, Pf0, _, _) :-
	FOld == FNew,
	HOld < HNew.

compareEtats(Succ, FOld, _, _, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) :-
	FOld > FNew,
	
	% on le supprime dans Pu et on le remet avec les nouvelles valeurs
	suppress([Succ, _, _, _], Pu0, AuxPu),
	insert([Succ, [FNew, HNew, GNew], Pere, Action], AuxPu, Pu1),
	
	% on le supprime dans Pf et on le remet avec les nouvelles valeurs
	suppress([_, Succ], Pf0, AuxPf),
	insert([[FNew, HNew, GNew], Succ], AuxPf, Pf1).

compareEtats(Succ, FOld, HOld, _, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) :-
	FOld == FNew,
	HOld > HNew,

	% on le supprime dans Pu et on le remet avec les nouvelles valeurs
	suppress([Succ, _, _, _], Pu0, AuxPu),
	insert([Succ, [FNew, HNew, GNew], Pere, Action], AuxPu, Pu1),
	
	% on le supprime dans Pf et on le remet avec les nouvelles valeurs
	suppress([_, Succ], Pf0, AuxPf),
	insert([[FNew, HNew, GNew], Succ], AuxPf, Pf1).
	
	
	

affiche_solution(Q, U, 0):-
	belongs([U, _, nil, nil], Q),
	writeln("Etat initial : "),
	affiche_etat(U),
	writeln("--------------------------------------------------------------------").
%affiche 
affiche_solution(Q, U, NumeroEtape) :-
	belongs([U, _, Pere, Action], Q),
	affiche_solution(Q, Pere, NE),
	NumeroEtape is NE+1,
	write("Etape "),
	write(NumeroEtape),
	writeln(" : "),
	write("Action : "),
	writeln(Action),
	writeln("Etat : "),
	affiche_etat(U),
	writeln("--------------------------------------------------------------------").
	

affiche_ligne([]) :-
	writeln(" | ").
affiche_ligne([vide|Reste]) :-
	write(" | "),
	write(" "),
	affiche_ligne(Reste).
affiche_ligne([H|Reste]) :-
	write(" | "),	
	write(H),	
	affiche_ligne(Reste).

affiche_etat([]):-
	writeln("  ------------").
affiche_etat([Ligne|Reste]) :-
	writeln("  ------------"),
	affiche_ligne(Ligne),	
	affiche_etat(Reste).

aetoile(nil, nil, _) :-       %Cas trivial , Pf et Pu vides 
	writeln("PAS de SOLUTION: L’ETAT FINAL N’EST PAS ATTEIGNABLE!").

aetoile(Pf, Pu, Q):-  %cas trivial , solution trouvée et à afficher, le min de Pf vaut l'état final
	final_state(Sf),
	recherche_et_suppr_f_u_min(Pf, _, UFMin),
	UFMin==Sf,

	suppress([UFMin, [FUFMin,HUFMin,Gu], PereUFMin, ActionUFMin], Pu, _), %supp ds PU
	insert([UFMin, [FUFMin,HUFMin,Gu], PereUFMin, ActionUFMin],Q,Q1),% AJout le bon elmt dans Q
	
	affiche_solution(Q1, Sf, _).

aetoile(Pf, Pu, Q) :-
	%on cherche et supprime le u de f min
	recherche_et_suppr_f_u_min(Pf, Pf2, UFMin),
	 
	 %on supprime le noeud frère dans Pu
	suppress([UFMin, [FUFMin,HUFMin,Gu], PereUFMin, ActionUFMin], Pu, Pu2),
	
	%on cherche les successeurs
	successeursEtActions(UFMin, SetA),
	splitSetA(SetA,Successeurs,Actions), 
	expand(Successeurs, [Fs,Gs,Hs] , Gu),
	loop_successors(Successeurs, [Fs, Hs, Gs], Pu2, Pf2, Pu3, Pf3, Q, UFMin, Actions),
	
	%U ayant été développé et supprimé de P, il reste à insérer le nœud [U,Val,...,..] dans Q,
	insert([UFMin,[FUFMin,HUFMin,Gu], PereUFMin, ActionUFMin], Q, Q2),
	aetoile(Pf3,Pu3,Q2).

	% PROBLEME IDENTIFIE : EN GROS ON RENTRE DANS AFFICHE SOLUTION ALORS QU'IL NOUS MANQUE A FAIRE VRAIMENT LETAPE FINALE! IL MANQUE UNE DERNIERE ETAPE! LE BELONGS NE MARCHE PAS DU COUP ET FALSE, ALORS QUE UNE ACTION AVANT DE LE ETESTER MARCHERAI ! insere avant le affiche solution le UFMIN dans Q mais du coup voir ce que renvoit rch et supp UF min , valeur de U ou tout le U ? 
   
