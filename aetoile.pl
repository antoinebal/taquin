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
	insert([S0, [0, H0, 0], nil, nil], Pu0, Pu)
	
	% lancement de Aetoile

	.   %********
			% A FAIRE
			%********



%*******************************************************************************

expand(Successeurs, Fs, Hs, Gs, Gu):-
	final_state(Sf),
	findall(H, (member(S, Successeurs), 


aetoile(nil, nil, _) :-
	writeln("PAS de SOLUTION: L’ETAT FINAL N’EST PAS ATTEIGNABLE!"),

aetoile(Pf, _, _):-
	final_state(Sf),
	echerche_et_suppr_f_u_min(Pf, _, UFMin),
	UFMin=Sf,
	affiche_solution(Q).

aetoile(Pf, Pu, Qs) :-
	%on cherche et supprime le u de f min
	recherche_et_suppr(Pf, NewPf, UFMin),
	 %on supprime le noeud frère dans Pu
	suppress([UFMin, _, _, _], Pu, NewPs),
	

	%on cherche les successeurs
	successeurs(UFMin, Successeurs),
	

	
   
