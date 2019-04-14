%*******************************************************************************
:- ['taquin.pl'].   
:- ['aetoile.pl'].  
%*******************************************************************************

%clause utile pour l'affichage des tests unitaires
printTest(Resultat, Resultat, NumeroTest) :-
	write("Test No"),
	write(NumeroTest),
	write(" : SUCCES ; Calculé = Attendu ; "),
	writeln(Resultat).
printTest(ResultatCalcule, ResultatAttendu, NumeroTest) :-
	ResultatCalcule\=ResultatAttendu,
	write("Test No"),
	write(NumeroTest),
	write(" : ECHEC ; Calculé != Attendu ; "),
	write(ResultatCalcule),
	write("!="),
	writeln(ResultatAttendu).


%***TESTS UNITAIRES HEURISTIQUES***
testCoincidence(0, F) :-
	writeln("TEST Coïncidence : SUCCES ; l'heuristique"),
	writeln("est coïncidente avec comme état final : "),
	affiche_etat(F).

testCoincidence(H, F) :-
	H\=0,
	writeln("TEST Coïncidence : ECHEC ; l'heuristique n'est pas coïncidente avec comme état final : "),
	affiche_etat(F).

% HEURISTIQUE 1
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
     
   
% HEURISTIQUE 2
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

%***TESTS UNITAIRES TRAITER SUCCESSEUR***
test_traiter_successeur :-
	writeln("*****TEST UNITAIRE DE TRAITER SUCCESSEUR*****"),
	empty(Q1),
	empty(PuVide),
	empty(PfVide),
	
	writeln(">Traitement successeur quand Q vide, Pu vide et Pf vide"),
	traiter_successeur(x, [3, 2, 1], PuVide, PfVide, Pu2, Pf2, Q1, p, up),
	% résultat attendu : avec un seul élément
	insert([x, [3, 2, 1], p, up], PuVide, PuAttendu2),
	insert([[3, 2, 1], x], PfVide, PfAttendu2),
	printTest(Pu2, PuAttendu2, 1),
	printTest(Pf2, PfAttendu2, 2),

	writeln(">Traitement noeud successeur déjà connu mais moins intéressant"),
	writeln("  avec FNew > FOld"),
	traiter_successeur(x, [8, 0, 0], Pu2, Pf2, Pu3, Pf3, Q1, p, up),
	printTest(Pu3, PuAttendu2, 3),
	printTest(Pf3, PfAttendu2, 4),
	writeln("  avec FNew==FOld et HNew > HOld"),	
	traiter_successeur(x, [6, 6, 0], Pu2, Pf2, Pu4, Pf4, Q1, p, up),
	printTest(Pu4, PuAttendu2, 5),
	printTest(Pf4, PfAttendu2, 6),

	writeln(">Traitement noeud successeur déjà connu et plus intéressant"),
	writeln("  avec FNew < FOld"),
	traiter_successeur(x, [2, 0, 2], Pu2, Pf2, Pu5, Pf5, Q1, p, up),
	insert([x, [2, 0, 2], p, up], PuVide, PuAttendu3),
	insert([[2, 0, 2], x], PfVide, PfAttendu3),
	printTest(Pu5, PuAttendu3, 7),
	printTest(Pf5, PfAttendu3, 8),
	writeln("  avec FNew==FOld et HNew < HOld"),	
	traiter_successeur(x, [3, 0, 3], Pu2, Pf2, Pu6, Pf6, Q1, p, up),
	insert([x, [3, 0, 3], p, up], PuVide, PuAttendu4),
	insert([[3, 0, 3], x], PfVide, PfAttendu4),
	printTest(Pu6, PuAttendu4, 9),
	printTest(Pf6, PfAttendu4, 10),

	writeln(">Traitement noeud successeur déjà connu par Q"),
	insert([y, [2, 1, 1], p, up], Q1, Q2),
	traiter_successeur(y, [2, 1, 1], PuVide, PfVide, Pu7, Pf7, Q2, p, up),
	printTest(Pu7, PuVide, 11),
	printTest(Pf7, PfVide, 12),
	
	writeln("**********************************************").









