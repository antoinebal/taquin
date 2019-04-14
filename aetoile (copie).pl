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


main :-
	initial_state(S0),
	% on initialise les trois arbres à nil
	empty(Pu0),
	empty(Pf0),
	empty(Q),
	% on calcule l'heuristique de l'état initial
	heuristique(S0, H0),
	%on insère [F0=H0+G0, H0, G0=0] dans Pu & Pf
	insert([[H0, H0, 0], S0], Pf0, Pf),
	insert([S0, [H0, H0, 0], nil, nil], Pu0, Pu),
	% lancement de A*
	aetoile(Pf,Pu,Q).



%*******************************************************************************
% fonction utile pour récupérer une liste d'Actions et une liste de Successeurs à
% partir d'une liste d'éléments de la forme [Successeur, Action]
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

% cas trivial : plus rien à traiter, les AVL restent inchanchés
loop_successors([],[[],[],[]],Pu,Pf,Pu,Pf, _, _, _).
loop_successors([S1|Succ], [[F|Fs], [H|Hs], [G|Gs]], Pu0, Pf0, Pu, Pf, Q, Pere, [Action|Suite]) :-
	traiter_successeur(S1, [F, H, G], Pu0, Pf0, Pu1, Pf1, Q, Pere, Action),
	loop_successors(Succ, [Fs, Hs, Gs], Pu1, Pf1, Pu, Pf, Q, Pere, Suite).


traiter_successeur(Succ, [FNew, HNew, GNew], Pu0, Pf0, Pu1, Pf1, Q, Pere, Action) :-
	%si le successeur est dans Q, on ignore, les AVL restent inchangés
	(belongs([Succ, _, _, _], Q) -> Pu1 = Pu0 ,
									Pf1 = Pf0													
		;
		%si est S connu dans Pu on compare les deux noeuds et on garde la meilleure évaluation  
	 	(belongs([Succ, [FOld,HOld,_], _, _],Pu0) ->
								compareEtats(Succ, FOld, HOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action)
			; 
			% le successeur n'est connu ni dans Q, ni dans Pu, on l'insère donc dans Pu et Pf
			insert([Succ,[FNew, HNew, GNew],Pere, Action],Pu0,Pu1),
			insert([[FNew, HNew, GNew],Succ],Pf0,Pf1)
			)
		).


% cas où la nouvelle évaluation est moins bonne
compareEtats(_, FOld, _, FNew, _, _, Pu0, Pu0, Pf0, Pf0, _, _) :-
	FOld < FNew.

%cas où les f sont égaux: on compare les h, ici on garde l'ancienne évaluation
compareEtats(_, FOld, HOld, FNew, HNew, _, Pu0, Pu0, Pf0, Pf0, _, _) :-
	FOld == FNew,
	HOld < HNew.

% cas où la nouvelle évaluation est mieux
compareEtats(Succ, FOld, _, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) :-		
	FOld > FNew,
	% on le supprime dans Pu et on le remet avec les nouvelles valeurs
	suppress([Succ, [FOld, HOld, GOld], _, _], Pu0, AuxPu),
	insert([Succ, [FNew, HNew, GNew], Pere, Action], AuxPu, Pu1),	
	% on le supprime dans Pf et on le remet avec les nouvelles valeurs
	suppress([[FOld, HOld,GOld], Succ], Pf0, AuxPf),
	insert([[FNew, HNew, GNew], Succ], AuxPf, Pf1).

%cas où les f sont égaux: on compare les h, ici on prend la nouvlle évaluation
compareEtats(Succ, FOld, HOld, FNew, HNew, GNew, Pu0, Pu1, Pf0, Pf1, Pere, Action) :-
	FOld == FNew,
	HOld > HNew,
	% on le supprime dans Pu et on le remet avec les nouvelles valeurs
	suppress([Succ, [FOld, HOld, GOld], _, _], Pu0, AuxPu),
	insert([Succ, [FNew, HNew, GNew], Pere, Action], AuxPu, Pu1),
	% on le supprime dans Pf et on le remet avec les nouvelles valeurs
	suppress([[FOld, HOld, GOld], Succ], Pf0, AuxPf),
	insert([[FNew, HNew, GNew], Succ], AuxPf, Pf1).

%dans le cas où les f et h sont égaux, on choisit le laisser les AVL inchangés
compareEtats(_, FOld, HOld, FNew, HNew, _, Pu0, Pu0, Pf0, Pf0, _, _) :-
	FOld == FNew,
	HOld == HNew.
	




% cas trivial dans lequel l'état initial est trouvé
affiche_solution(Q, U, 0):-
	belongs([U, [F, H, G], nil, nil], Q),
	initial_state(U),
	writeln("Etat initial : "),
	affiche_etat(U),
	write("F = "),
	write(F),
	write(" ; H = "),
	write(H),
	write(" ; G = "),
	writeln(G),
	writeln("*********************").

% cas général
affiche_solution(Q, U, NumeroEtape) :-
	belongs([U, [F, H, G], Pere, Action], Q),
	affiche_solution(Q, Pere, NE),
	NumeroEtape is NE+1,
	write("Etape "),
	write(NumeroEtape),
	writeln(" : "),
	write("Action : "),
	writeln(Action),
	affiche_etat(U),
	write("F = "),
	write(F),
	write(" ; H = "),
	write(H),
	write(" ; G = "),
	writeln(G), 
	writeln("*********************").
	

% cas trivial, Pf et Pu sont vides donc pas de solution 
aetoile(nil, nil, _) :-       
	writeln("PAS de SOLUTION: L’ETAT FINAL N’EST PAS ATTEIGNABLE!").

%cas trivial, on a trouvé la solution
aetoile(Pf, Pu, Q):- 
	% l'état du noeud minimum extrait correspond à la situation finale
	final_state(Sf),
	suppress_min([_,UFMin], Pf, _),
	UFMin==Sf,

	% on supprime le noeud frère dans Pu est récupère au passage son père et l'action y menant
	suppress([UFMin, [FUFMin,HUFMin,GUFMin], PereUFMin, ActionUFMin], Pu, _),

	% on ajoute l'état final dans Q
	insert([UFMin, [FUFMin,HUFMin,GUFMin], PereUFMin, ActionUFMin],Q,Q1),

	affiche_solution(Q1, Sf, _),!.

aetoile(Pf, Pu, Q) :-
	% on cherche et supprime l'état UFMin de F minimum dans Pf
	suppress_min([_,UFMin] , Pf, Pf2),
	
	% on supprime le noeud frère dans Pu
	suppress([UFMin, [FUFMin,HUFMin,GUFMin], PereUFMin, ActionUFMin], Pu, Pu2),
	
	% on cherche les successeurs
	successeursEtActions(UFMin, SetA),
	splitSetA(SetA,Successeurs,Actions), 

	% on calcule les valeurs [F, H, G] pour chaque successeur
	expand(Successeurs, [Fs,Hs,Gs] , GUFMin),

	% on parcourt chaque successeur pour les traiter, et on récupère les nouveaux AVL Pu3 et Pf3
	loop_successors(Successeurs, [Fs, Hs, Gs], Pu2, Pf2, Pu3, Pf3, Q, UFMin, Actions),
	
	% U ayant été développé et supprimé de P, on insère son noeud dans 
	insert([UFMin,[FUFMin,HUFMin,GUFMin], PereUFMin, ActionUFMin], Q, Q2),
	
	% on rappelle aetoile avec les nouveaux AVL
	aetoile(Pf3,Pu3,Q2).

   
