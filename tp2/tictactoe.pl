	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre, soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp pr�c�dent, pour mod�liser une case libre
	dans une matrice on n'utilise pas une constante sp�ciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t une variable libre (_), c'est�-dire un terme non instanci� ('_').
	La situation initiale est donc une matrice 3x3 composee uniquement de variables libres (_). 
	Ceci est possible car le jeu consiste � instancier la grille avec des symboles et non � d�placer les symbles d�j� affect�s.
	
	
	
	Jouer un coup, c-a-d placer un symbole dans une grille S1 ne consiste pas � g�n�rer une nouvelle grille S2 obtenue 
	en copiant d'abord S1 puis en remplacant le symbole de case libre par le symbole du joueur, mais plus simplement
	� INSTANCIER (au sens Prolog) la variable libre qui repr�sentait la case libre par la valeur associ�e au joueur, ex :
	Case = Joueur, ou a realiser indirectement cette instanciation par unification via un pr�dicat comme member/2, select/3, nth1/3 ...
	
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer � s'appeler S (on n'a pas besoin de la d�signer
	par un nouvel identificateur).
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chacun des 9 termes est une variable libre.	
	*/

situation_initiale([ [_,_,_],
                     [_,_,_],
                     [_,_,_] ]).

egalite_state([
	[x,x,o], 
	[x,x,o], 
	[_, _, _]
	]).

	% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


	% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


	/****************************************************
	 DEFINIR ICI � l'aide du pr�dicat ground/1 comment
	 reconnaitre une situation terminale dans laquelle il
	 n'y a aucun emplacement libre : aucun joueur ne peut
	 continuer � jouer (quel qu'il soit).
	 ****************************************************/

terminale_ligne([]).
terminale_ligne([Case|Reste]) :-
	ground(Case),
	terminale_ligne(Reste).

 %TEST FAIT
situation_terminale(_Joueur, []).
situation_terminale(_Joueur, [Ligne|Reste]) :-
	terminale_ligne(Ligne),
	situation_terminale(_, Reste).
	   

/***************************
 DEFINITIONS D'UN ALIGNEMENT
 ***************************/

 %TEST FAIT
alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

	/********************************************
	 DEFINIR ICI chaque type d'alignement maximal 
 	 existant dans une matrice carree NxN.
	 ********************************************/

 %TEST FAIT
ligne(Ligne, [Ligne|_]).	
ligne(Ligne, [_|Reste]) :-
	ligne(Ligne, Reste).

 %TEST FAIT
colonne(Colonne, Matrix) :-
	colonne2(Colonne, Matrix, _).

colonne2([], [], _).
colonne2([H|ResteColonne],[Ligne|Reste], N) :-
	nth1(N, Ligne, H),
	colonne2(ResteColonne, Reste, N).

	/* D�finition de la relation liant une diagonale D � la matrice M dans laquelle elle se trouve.
		il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
		- la premiere diagonale (principale) (descendante) : (A I)
		- la seconde diagonale  (ascendante)  : (R Z)
		A . . . . . . . Z
		. \ . . . . . / .
		. . \ . . . / . .
		. . . \ . / . . .
		. . . . X . . .
		. . . / . \ . . . 
		. . / . . . \ . .
		. / . . . . . \ .
		R . . . . . . . I
	*/

 %TEST FAIT		
diagonale(D, M) :- premiere_diag(1,D,M).
diagonale(D, M) :- seconde_diag(_,D,M).  

premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

% definition de la seconde diagonale A COMPLETER

 %TEST FAIT
seconde_diag(K, D, M) :-
	seconde_diag2(K, D2, M),
	reverse(D2, D).

seconde_diag2(0,[],[]). 
seconde_diag2(K1,[E|D],[Ligne|M]) :- 		
	seconde_diag2(K,D,M),
	K1 is K+1, 
	nth1(K1,Ligne,E).
	


	/***********************************
	 DEFINITION D'UN ALIGNEMENT POSSIBLE
	 POUR UN JOUEUR DONNE
	 **********************************/

 %TEST FAIT
possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible([   ], _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/

 %TEST FAIT
unifiable(X,_) :- var(X). % Soit elle est libre, 
unifiable(E,J) :- ground(E), E==J. % soit elle est marqué par J
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/

	/*
	Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
Un alignement perdant pour J est gagnant
pour son adversaire.
	*/

% TEST FAIT 
alignement_gagnant([],_).
alignement_gagnant([E|T], J) :- ground(E), E=J, alignement_gagnant(T,J). 

% TEST FAIT 
alignement_perdant(Ali, J) :- adversaire(J,A), 
							alignement_gagnant(Ali,A).


	/******************************
	DEFINITION D'UN ETAT SUCCESSEUR
	*******************************/

     /*Il faut definir quelle op�ration subit une matrice M representant la situation courante
	lorsqu'un joueur J joue en coordonnees [L,C]
     */	

% TEST FAIT
% attention vérifier si la case est non liée
% sinon il compte un successeur pour chaque J déjà présent
successeur(J,Etat,[L,C]) :- 
nth1(L,Etat,Lig), 
nth1(C,Lig,Case),
var(Case),
Case=J.

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

/*
1/ l'heuristique est +infini si la situation J est gagnante pour J
2/ l'heuristique est -infini si la situation J est perdante pour J
3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/


heuristique(J,Situation,H) :-		% cas 1
   H = 10000,				% grand nombre approximant +infini
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !.
	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approximant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J),!.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.
% Le cut permet de ne pas rentrer dans les autres cas si on y rentre! 
% Dnas le cas , false, ne change rien 
% 					cas 3
heuristique(J,Situation,H) :-

	% on calcule le nombre d'align possibles pour J
	findall(Align, (alignement(Align, Situation), possible(Align, J)), AlignPossiblesJ),
	length(AlignPossiblesJ, NombreAPossJ),

	% on calcule le nombre d'align possibles pour son adversaire
	adversaire(J, Adv),
	findall(Align, (alignement(Align, Situation), possible(Align, Adv)), AlignPossiblesAdv),
	length(AlignPossiblesAdv, NombreAPossAdv),
	write("NombreAPossJ : "),
	writeln(NombreAPossJ),
	write("NombreAPossaDV " ),
	writeln(NombreAPossAdv),
	H is NombreAPossJ - NombreAPossAdv.
	



