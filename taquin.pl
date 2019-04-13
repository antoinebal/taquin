%:- lib(listut).       % Placer cette directive en commentaire si vous utilisez Swi-Prolog 
   
                      % Sinon ne pas modifier si vous utilisez ECLiPSe Prolog :
                      % -> permet de disposer du predicat nth1(N, List, E)
                      % -> permet de disposer du predicat sumlist(List, S)
                      % (qui sont predefinis en Swi-Prolog)

                      
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
   % format :  initial_state(+State) ou State est une matrice (liste de listes)
   
initial_state(InitState) :- initial_state4(InitState).

initial_state1([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5





% AUTRES EXEMPLES POUR LES TESTS DE  A*


initial_state2([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2

initial_state3([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10
			
initial_state4([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
			
initial_state5([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30 

initial_state6([ [a, b, c],
                [g,vide,d],
                [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)
 




/*pour tester les matrices de taille 4*4 (on prend un exemple simple car le cas
sans solution est fréquent
  ---------------
 | a | b | c | d | 
  ---------------
 | e | f | g | h | 
  ---------------
 | i | j | k |   | 
  ---------------
 | m | n | o | l | 
  ---------------
*/


initial_state4x4([[1, 2, 3, 4],
             [5, 6, 7, 8],
             [9, 10, 11, vide],
			[13, 14, 15, 12]]).

   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)


final_state(FinalState) :- final_state3x3(FinalState).

   
final_state3x3([[a, b,  c],
             [h,vide, d],
             [g, f,  e]]).


/*pour tester les matrices de taille 4*4
*/
final_state4x4([[1, 2, 3, 4],
             [5, 6, 7, 8],
             [9, 10, 11, 12],
			[13, 14, 15, vide]]).

			 
   %********************
   % AFFICHAGE D'UN ETAT
   %********************
   % format :  write_state(?State) ou State est une liste de lignes a afficher

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

/*
Nous avons rajouté cette fonction pendant l'écriture d'A* : 
elle permet d'obtenir une liste de la forme [Successeur, Action_pour_y_parvenir]
pour un état S1
*/
successeursEtActions(S1, L2) :-
	findall([S2, Action], rule(Action, 1, S1, S2), L2).

   %***********************
   % Deplacement horizontal            
   %***********************
    % format :   horizontal_permutation(?Piece1,?Piece2,+Current_State, ?Next_State)
	
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical            
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait d'une occurrence X en position N dans une liste L (resultat R) 
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.


   
   
   %*******************
   % PARTIE A COMPLETER
   %*******************
   
   %*******************************************************************
   % Coordonnees X(colonne),Y(Ligne) d'une piece P dans une situation U
   %*******************************************************************
	% format : coordonnees(?Coord, +Matrice, ?Element)
	% Définit la relation entre des coordonnees [Ligne, Colonne] et un element de la matrice
	/*
	Exemples
	
	?- coordonnees(Coord, [[a,b,c],[d,e,f]],  e).        % quelles sont les coordonnees de e ?
	Coord = [2,2]
	yes
	
	?- coordonnees([2,3], [[a,b,c],[d,e,f]],  P).        % qui a les coordonnees [2,3] ?
	P=f
	yes
	*/

	
	%coordonnees([L,C], Mat, Elt) :- true.    %********
											 % A FAIRE
											%********

											 
   %*************
   % HEURISTIQUES
   %*************


heuristique(U,H) :-
   heuristique1(U, H).  % choisir l'heuristique 
%   heuristique2(U, H).   utilisee ( 1 ou 2) 
   


   %****************
   %HEURISTIQUE no 1
   %****************

/*fonction pour évaluer l'égalité 
	entre deux éléments : 
	> si E1\=E2 et E1\=vide => R=1
	> sinon R=0 
*/
diff_element(E1,E2,R) :- 
	( E1\=E2 ->
 		(E1\=vide -> 
			R=1 
		;
			R=0)
	;
		R=0
	).
		
%les deux lignes sont vides (cas trivial)	
diff_ligne([], [], 0).
diff_ligne([E1|L1], [E2|L2], X) :-
	diff_element(E1, E2, X1),
	diff_ligne(L1, L2, X2),
	X is X2+X1.
	
%les deux matrices sont vides (cas trivial)
diff_matrice([], [], 0).
diff_matrice([L1|M1], [L2|M2], X):-
	diff_ligne(L1, L2, X1),
	diff_matrice(M1, M2, X2),
	X is X2+X1.


%clause utile pour l'affichage des tests unitaires
printTest(Resultat, Resultat, NumeroTest) :-
	write("TEST No"),
	write(NumeroTest),
	write(" : SUCCES ; Calculé = Attendu ; "),
	writeln(Resultat).
printTest(ResultatCalcule, ResultatAttendu, NumeroTest) :-
	ResultatCalcule\=ResultatAttendu,
	write("TEST No"),
	write(NumeroTest),
	write(" : ECHEC ; Calculé != Attendu ; "),
	write(ResultatCalcule),
	write("!="),
	writeln(ResultatAttendu).

testCoincidence(0, F) :-
	writeln("TEST Coïncidence : SUCCES ; l'heuristique"),
	writeln("est coïncidente avec comme état final : "),
	affiche_etat(F).

testCoincidence(H, F) :-
	H\=0,
	writeln("TEST Coïncidence : ECHEC ; l'heuristique n'est pas coïncidente avec comme état final : "),
	affiche_etat(F).

%TEST UNITAIRE HEURISTIQUE 1
test_heuristique1 :-
	writeln("*****TEST UNITAIRE DE L'HEURISTIQUE 1*****"),
	initial_state1(I1),
	initial_state2(I2),
	initial_state3(I3),
	initial_state4(I4),
	initial_state5(I5),
	initial_state6(I6),
	final_state(F),

	heuristique1(I1, HCalcul1),
	heuristique1(I2, HCalcul2),
	heuristique1(I3, HCalcul3),
	heuristique1(I4, HCalcul4),
	heuristique1(I5, HCalcul5),
	heuristique1(I6, HCalcul6),
	heuristique1(F, HCalculF),

	printTest(HCalcul1, 4, 1),
	printTest(HCalcul2, 2, 2),
	printTest(HCalcul3, 7, 3),
	printTest(HCalcul4, 6, 4),
	printTest(HCalcul5, 8, 5),
	printTest(HCalcul6, 2, 6),

	testCoincidence(HCalculF, F),
	writeln("******************************************").
     
   % Calcul du nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F

    heuristique1(U, H) :-    % Verifier qu'on doit bien trouver 3 , et pas 4 dépend du comptage de mouvement sur le vide ou non 
		final_state(F),
		diff_matrice(U, F, H).    

   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme sur l'ensemble des pieces des distances de Manhattan
   % entre la position courante de la piece et sa position dans l'etat final


/*
prédicat utile pour transformer une matrice en liste
ex. : 
[[b, h, c],
[a, f, d], 		-> [b, h, c, a, f, d, g, vide, e]
[g,vide,e]]
*/
construitliste([], []).
construitliste([H|M], L) :-
		construitliste(M, L1),
		append(H, L1, L).

% I et J sont les coordonnées de Value dans la matrice Matrix
matrix(Matrix, I, J, Value) :-
    nth1(I, Matrix, Row),
    nth1(J, Row, Value).

%la distance de Manhattan de vide est nulle peu importe les matrices	
d_manh(_, _, vide, 0).
d_manh(M1,M2,Value,DManh) :-
	Value\=vide, 
	% on récupère les coordonnées dans M1
	matrix(M1,X1,Y1,Value), 
	% on récupère les coordonnées dans M2
	matrix(M2,X2,Y2,Value),
	% on calcule la distance de Manhattan
	DManh is (abs(X1-X2)+abs(Y1-Y2)). 
	
heuristique2(U, H) :-
	% on transforme la matrice U en liste pour la parcourir 	
	construitliste(U, Liste),
	final_state(F),
	% on construit la liste des distances de Manhattan
	findall(DManh, (member(Piece, Liste), d_manh(U, F, Piece, DManh)), ListeDM),
	% on somme les éléments de la liste	
	sumlist(ListeDM, H).

%TEST UNITAIRE HEURISTIQUE 2
test_heuristique2 :-
	writeln("*****TEST UNITAIRE DE L'HEURISTIQUE 2*****"),
	initial_state1(I1),
	initial_state2(I2),
	initial_state3(I3),
	initial_state4(I4),
	initial_state5(I5),
	initial_state6(I6),
	final_state(F),

	heuristique2(I1, HCalcul1),
	heuristique2(I2, HCalcul2),
	heuristique2(I3, HCalcul3),
	heuristique2(I4, HCalcul4),
	heuristique2(I5, HCalcul5),
	heuristique2(I6, HCalcul6),
	heuristique1(F, HCalculF),

	printTest(HCalcul1, 5, 1),
	printTest(HCalcul2, 2, 2),
	printTest(HCalcul3, 10, 3),
	printTest(HCalcul4, 16, 4),
	printTest(HCalcul5, 24, 5),
	printTest(HCalcul6, 2, 6),

	testCoincidence(HCalculF, F),
	writeln("******************************************").	


affiche_ligne([]) :-
	writeln(" | ").
affiche_ligne([vide|Reste]) :-
	write(" | "),
	write(" "),
	affiche_ligne(Reste).
affiche_ligne([H|Reste]) :-
	H\=vide,
	write(" | "),	
	write(H),	
	affiche_ligne(Reste).

affiche_etat([]):-
	writeln("  ------------").
affiche_etat([Ligne|Reste]) :-
	writeln("  ------------"),
	affiche_ligne(Ligne),	
	affiche_etat(Reste).


%situation que l'on va développer ensuite (suppress_min existe dejà)
%recherche_suppression_min_p(P, Q

									
									
