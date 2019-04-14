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
   
initial_state(InitState) :- initial_state1(InitState).

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
initial_state4x4([[a, vide, c, d],
             	[e, b, f, h],
             	[i, j, g, k],
				[m, n, o, l]]).
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
final_state4x4([[a, b, c, d],
             	[e, f, g, h],
             	[i, j, k, l],
				[m, n, o, vide]]).

			 
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
	
		
coordonnees([L,C], Mat, Elt) :-
	nth1(L, Mat, Row),
    nth1(C, Row, Elt) .

											 
   %*************
   % HEURISTIQUES
   %*************


heuristique(U,H) :-
   heuristique2(U, H).  % choisir l'heuristique 
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


% Calcul du nombre de pieces mal placees dans l'etat courant U
% par rapport a l'etat final F
heuristique1(U, H) :-    
	final_state(F),
	diff_matrice(U, F, H).    

   %****************
   %HEURISTIQUE no 2
   %****************
   
   


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


%la distance de Manhattan de vide est nulle peu importe les matrices	
d_manh(_, _, vide, 0).
d_manh(M1,M2,Value,DManh) :-
	Value\=vide, 
	% on récupère les coordonnées dans M1
	coordonnees([X1, Y1], M1, Value),
	% on récupère les coordonnées dans M2
	coordonnees([X2, Y2], M2, Value),
	% on calcule la distance de Manhattan
	DManh is (abs(X1-X2)+abs(Y1-Y2)). 
	
% Somme sur l'ensemble des pieces des distances de Manhattan
% entre la position courante de la piece et sa position dans l'etat final
heuristique2(U, H) :-
	% on transforme la matrice U en liste pour la parcourir 	
	construitliste(U, Liste),
	final_state(F),
	% on construit la liste des distances de Manhattan
	findall(DManh, (member(Piece, Liste), d_manh(U, F, Piece, DManh)), ListeDM),
	% on somme les éléments de la liste	
	sumlist(ListeDM, H).



% AFFICHAGE
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

% Notre prédicat pour afficher un état
affiche_etat([]):-
	writeln("  ---------------").
affiche_etat([Ligne|Reste]) :-
	writeln("  ---------------"),
	affiche_ligne(Ligne),	
	affiche_etat(Reste).




									
									
