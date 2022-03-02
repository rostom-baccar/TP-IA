/* Fichier du probleme. 

Doit contenir au moins 4 predicats qui seront utilises par A*

   etat_initial(I)                                         % definit l'etat initial

   etat_final(F)                                           % definit l'etat final  

   rule(Rule_Name, Rule_Cost, Before_State, After_State)   % règles applicables

   heuristique(Current_State, Hval)				           % calcul de l'heuristique 


Les autres prédicats sont spécifiques au Taquin.
*/


%:- lib(listut).      % Laisser cette directive en commentaire si vous utilisez Swi-Prolog 
   
                      % Sinon décommentez la ligne si vous utilisez ECLiPSe Prolog :
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
   

initial_state([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5



% AUTRES EXEMPLES POUR LES TESTS DE  A*

/*
initial_state([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2

initial_state([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10
			
initial_state([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
			
initial_state([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30 

initial_state([ [a, b, c],
                [g,vide,d],
                [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)
*/  


   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)
   
final_state([[a, b,  c],
             [h,vide, d],
             [g, f,  e]]).

			 
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

	
	coordonnees([L,C], Mat, Elt) :-
    	nth1(L,Mat,Ligne), nth1(C,Ligne, Elt).

	mal_placee(Element,InitialState,NextState) :-
    	coordonnees([L,C],InitialState,Element),
    	coordonnees([L1,C1],NextState,Element),
    	[L,C]\=[L1,C1].    
    
    %********
	% A FAIRE
	%********

											 
   %*************
   % HEURISTIQUES
   %*************
   
heuristique(U,H) :-
    %heuristique1(U, H).  % au debut on utilise l'heuristique 1 
    heuristique2(U, H).  % ensuite utilisez plutot l'heuristique 2  
   
   
   %****************
   %HEURISTIQUE no 1
   %****************
   % Nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F
   
   
   % Suggestions : définir d'abord le prédicat coordonnees(Piece,Etat,Lig,Col) qui associe à une pièce présente dans Etat
   % ses coordonnees (Lig= numero de ligne, Col= numero de Colonne)
   
   % Definir ensuite le predicat malplace(P,U,F) qui est vrai si les coordonnes de P dans U et dans F sont differentes.
   % On peut également comparer les pieces qui se trouvent aux mêmes coordonnees dans U et dans H et voir s'il sagit de la
   % même piece.
   
    % Definir enfin l'heuristique qui détermine toutes les pièces mal placées (voir prédicat findall) 
	% et les compte (voir prédicat length)
   
    heuristique1(InitialState,NbrMalPlacees) :- 
    	final_state(FinalState),
    	findall(Element, (mal_placee(Element,InitialState,FinalState), Element\= vide  ),Liste),
    	length(Liste,NbrMalPlacees).
             
                
   
   
   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme des distances de Manhattan à parcourir par chaque piece
   % entre sa position courante et sa positon dans l'etat final

	manhattan_distance(InitialState,Manhattan) :-
        final_state(FinalState),
    	coordonnees([L,C],InitialState,Element),
    	coordonnees([L1,C1],FinalState,Element),
    	Element\= vide,
    	Manhattan is (abs(L-L1)+abs(C-C1)).
   
    heuristique2(InitialState,SumManhattan) :- 
		findall(Manhattan,manhattan_distance(InitialState,Manhattan),Liste),
    	sumlist(Liste,SumManhattan).
    	
    	
    
									
									




%***************************
% Gestion d'un AVL en Prolog
%***************************

%***************************
% INSA TOULOUSE - P.ESQUIROL
% mars 2017
%***************************

%*************************
% unit tests          : OK
% integration aetoile : OK
%*************************

% Les AVL sont des arbres BINAIRES DE RECHERCHE H-EQUILIBRES : 
% La hauteur de l'avl A est définie par :
%  -1, si A est vide (A=nil)
%  1 + max( hauteur(ss_arbre_gauche(A)), hauteur(ss_arbre_droitee(A)) ) sinon

% Tout noeud de l'arbre est soit :
% - une feuille
% - un noeud interne tel que la différence de hauteur entre le sous-arbre droit
% 	et le sous-arbre gauche appartient à  [-1,0,+1]


%***********************************************
% PREDICATS EXPORTES ET COMPLEXITE ALGORITHMIQUE
%***********************************************
% soit N = nombre de noeuds de l'arbre				%   UTILITE POUR A*
%													%   ----------------													
% empty(?Avl)						O(1)			%<<< initialisation de P et Q
% height(+Avl, ?Height)             O(1)			
% put_flat(+Avl)                    O(N)			
% put_90(+Avl)                      O(N)			
% belongs(+Elem, +Avl)              O(log N)		%<<< appartenance d'un noeud à Q
% subtree(+Elem, +Avl, Ss_Avl)      O(log N)
% insert(+Elem, +Avant, ?Apres)     O(log N)		%<<< insertion d'un nouveau noeud dans P ou dans Q
% suppress(+Elem,+Avant,?Apres)     O(log N)		%<<< mise  à jour <=> suppression puis insertion
% suppress_min(?Min,+Avant,?Apres)  O(log N)		%<<< supression du noeud minimal
% suppress_max(?Max,+Avant,?Apres)  O(log N)

%****************************
% Prédicats internes (prives)
%****************************

% left_rotate(+Avant, ?Apres)		O(1)
% right_rotate(+Avant, ?Apres)		O(1)
% left_balance(+Avant, ?Apres)      O(1)
% right_balance(+Avant, ?Apres)     O(1)



	%------------------------------
	% Constructeur et test AVL vide
	%------------------------------

empty(nil).

	%-----------------
	% Hauteur d'un AVL
	%-----------------
	% par convention, un avl vide a une hauteur de -1
	% sinon la hauteur est enregistree au meme niveau que la racine de l'avl
	% elle n'est pas calculee recursivement "from scratch"
	% elle est mise à jour de façon incrémentale, apres chaque insertion ou suppression
	% d'ou sa complexité en O(1)  :-)

height(nil,             -1).
height(avl(_G,_R,_D, H), H).

	%-------------------
	% Affichage d'un AVL
	%-------------------
	% dans l'ordre croissant (lexicographique)

put_flat(nil).
put_flat(avl(G,R,D,_H)) :-
	put_flat(G),
	nl, write(R), 
	put_flat(D).

	%----------------------------
	% Affichage (couché) d'un AVL
	%----------------------------

put_90(Avl) :-
	nl, writeln('----------------------------------'),
	put_90(Avl,"").

put_90(nil,Str) :-
	write(Str), write('.').
put_90(avl(G,R,D,_H),Str) :-
	append_strings(Str, "   ", Str2),
	put_90(D,Str2),
	nl, write(Str), write(R),nl,
	put_90(G,Str2).

	%-----------------------------------------
	% Appartenance d'un element donne a un AVL	
	%-----------------------------------------

belongs(Elem, avl(G,Racine,D,_Hauteur)) :-
	(Elem = Racine ->
		true
	;
		(Elem @< Racine ->
			belongs(Elem, G)
		;
			belongs(Elem, D) 		%Racine @< Elem
		)
	).
	
	%------------------------------------------------------------
	% Recherche du sous-arbre qui a comme racine un element donne	
	%------------------------------------------------------------

subtree(Elem, avl(G,Racine,D,H), A) :-
	(Elem = Racine ->
		A = avl(G,Racine,D,H)
	;
		(Elem @< Racine ->
			subtree(Elem,G,A)
		;
			subtree(Elem,D,A) 		%Racine @< Elem
		)
	).
	
	%----------------------
	% Rotations dans un avl
	%----------------------
	% Les rotations ci-dessous décrivent uniquement les cas ou la rotation est possible.
	% Dans les autres cas, ces relations échouent ; plus précisément :
	% a/ si l'arbre est un avl vide, alors aucune rotation n'est possible ;
	% b/ si l'arbre est un avl non vide mais si son ss-arbre gauche est un avl vide
	%    alors la rotation droite n'est pas possible ;
	% c/ si l'arbre est un avl non vide mais si son ss-arbre droite est un avl vide
	%    alors la rotation gauche n'est pas possible.

right_rotate(avl(G,R,D,_H), A_Apres) :-
	height(D,HD),
	G       = avl(SG,RG,SD,_HG),
	height(SD,HSD),
	H_Inter is 1 + max(HSD, HD),
	Inter   = avl(SD,R,D,H_Inter),
	height(SG,HSG),
	H_Apres is 1 + max(HSG,H_Inter),
	A_Apres = avl(SG,RG,Inter,H_Apres).
	
left_rotate(avl(G,R,D,_), A_Apres) :-
	height(G,HG),
	D       = avl(SG,RD,SD,_),
	height(SG,HSG),
	H_Inter is 1 + max(HSG, HG),
	Inter   = avl(G,R,SG,H_Inter),
	height(SD,HSD),
	H_Apres is 1 + max(H_Inter,HSD),
	A_Apres = avl(Inter,RD,SD,H_Apres).	

	%---------------------------------
	% Insertion equilibree dans un avl
	%---------------------------------
	% On suppose que l'arbre avant insertion est equilibré (difference de hauteur
	% entre les ss-arbres gauche et droite de 1 au maximum)
	% L'insertion doit assurer qu'apres insertion l'arbre est toujours equilibre
	% sinon les rotations necessaires sont effectuees.

	% On suppose que les noeuds contiennent des informations que l'on peut comparer
	% a l'aide d'une relation d'ordre lexicographique (la cle c'est l'info elle-meme)
	% En prolog, c'est la relation '@<'
	% On peut comparer par exemple des integer, des string, des constantes,
	% des listes d'entiers, des listes de constantes, etc ... bref, des termes clos
	% T1 @< T2 est vrai si T1 est lexicographiquement inférieur a T2.

insert(Elem, nil, avl(nil,Elem,nil,0)).
insert(Elem, AVL, NEW_AVL) :-
	AVL = avl(Gauche,Racine,Droite,_Hauteur),
	(Elem = Racine ->
			% l'élément est déjà present, pas d'insertion possible
		fail
	;
		(Elem @< Racine ->
			% insertion dans le ss-arbre gauche
			insert(Elem, Gauche, New_Gauche),
			height(New_Gauche, New_HG),
			height(Droite, HD),
			H_Int is 1+max(New_HG, HD),
			AVL_INT = avl(New_Gauche, Racine, Droite, H_Int), 
			right_balance(AVL_INT, NEW_AVL)
		;
	    % Elem @> Racine
			% insertion dans le ss-arbre droite
			insert(Elem, Droite, New_Droite),
			height(New_Droite, New_HD),
			height(Gauche, HG),
			H_Int is 1+max(New_HD, HG),
			AVL_INT =avl(Gauche, Racine,New_Droite, H_Int),
			left_balance(AVL_INT, NEW_AVL)
		)
	).
	
	%------------------------------------------------
	% Suppression d'un element quelconque dans un avl
	%------------------------------------------------
	% On suppose que l'élément à supprimer appartient bien à l'AVL,
	% sinon le predicat échoue (en particulier si l'AVL est vide).
	
suppress(Elem, AVL, NEW_AVL) :-
	AVL = avl(Gauche, Racine, Droite, _Hauteur),
	(Elem = Racine ->
		% cas de la suppression de la racine de l'avl
		(Gauche = nil -> % cas simple d'une feuille ou d'un avl sans fils gauche
			NEW_AVL = Droite
		; 
			(Droite = nil -> % cas simple d'un avl avec fils gauche mais sans fils droit
				NEW_AVL = Gauche
			;
				% cas d'un avl avec fils gauche ET fils droit 
				%Gauche \= nil
				%Droite \= nil
				suppress_max(Max, Gauche, New_Gauche),
				AVL_INT = avl(New_Gauche,Max,Droite,_),
				left_balance(AVL_INT, NEW_AVL)
			)
		)
	;
		% cas des suppressions d'un element autre que la racine 
		(Elem @< Racine ->
			% suppression dans le ss-arbre gauche
			suppress(Elem, Gauche, New_Gauche),
			AVL_INT = avl(New_Gauche, Racine, Droite,_),
			left_balance(AVL_INT, NEW_AVL)
		;
		%Racine @< Droite
			% suppression dans le ss-arbre droite 
			suppress(Elem, Droite, New_Droite),
			AVL_INT = avl(Gauche, Racine, New_Droite,_),
			right_balance(AVL_INT, NEW_AVL)
		)
	).
	
	%-------------------------------------------------------
	% Suppression du plus petit element dans un avl non vide
	%-------------------------------------------------------
	% Si l'avl est vide, le prédicat échoue

suppress_min(Min, AVL, NEW_AVL) :-
	AVL = avl(Gauche,Racine,Droite, _Hauteur),
	(Gauche = nil ->
		Min = Racine,
		NEW_AVL = Droite
	;
		% Gauche \= nil
		suppress_min(Min, Gauche, New_Gauche),
		AVL_INT = avl(New_Gauche, Racine, Droite,_),
		left_balance(AVL_INT, NEW_AVL)
	).

	%-------------------------------------------------------
	% Suppression du plus grand element dans un avl non vide
	%-------------------------------------------------------
	% Si l'avl est vide, le prédicat échoue

suppress_max(Max, AVL, NEW_AVL) :-
	AVL = avl(Gauche,Racine,Droite, _Hauteur),
	(Droite = nil ->
		Max = Racine,
		NEW_AVL = Gauche
	;
		% Droite \= nil
		suppress_max(Max, Droite, New_Droite),
		AVL_INT = avl(Gauche, Racine, New_Droite,_),
		right_balance(AVL_INT, NEW_AVL)
	).
	
	%----------------------------------------
	% Re-equilibrages d'un avl vers la gauche
	%----------------------------------------
	% - soit apres insertion   d'un element dans le sous-arbre droite
	% - soit apres suppression d'un élément dans le sous-arbre gauche
	%----------------------------------------------------------------

left_balance(Avl, New_Avl) :-
	Avl = avl(Gauche, Racine, Droite, _Hauteur),
	height(Gauche, HG),
	height(Droite, HD),
	(HG is HD-2 ->
	% le sous-arbre droite est trop haut 
		Droite = avl(G_Droite, _R_Droite, D_Droite, _HD),
		height(G_Droite, HGD),
		height(D_Droite, HDD),
		(HDD > HGD ->
		% une simple rotation gauche suffit
			left_rotate(Avl, New_Avl)
		;
		% il faut faire une rotation droite_gauche
			right_rotate(Droite, New_Droite),
			height(New_Droite, New_HD),
			H_Int is 1+ max(HG, New_HD),
			Avl_Int = avl(Gauche, Racine, New_Droite, H_Int),
			left_rotate(Avl_Int, New_Avl)
		)
	;
	% la suppression n'a pas desequilibre l'avl
		New_Hauteur is 1+max(HG,HD),
		New_Avl = avl(Gauche, Racine, Droite, New_Hauteur)
	).

	%----------------------------------------
	% Re-equilibrages d'un avl vers la droite
	%----------------------------------------
	% - soit apres insertion   d'un element dans le sous-arbre gauche
	% - soit apres suppression d'un élément dans le sous-arbre droite
	%----------------------------------------------------------------
	
right_balance(Avl, New_Avl) :-
	Avl = avl(Gauche, Racine, Droite, _Hauteur),
	height(Gauche, HG),
	height(Droite, HD),
	(HD is HG-2 ->
	% le sous-arbre gauche est trop haut 
		Gauche = avl(G_Gauche, _R_Gauche, D_Gauche, _HG),
		height(G_Gauche, HGG),
		height(D_Gauche, HDG),
		(HGG > HDG ->
		% une simple rotation droite suffit
			right_rotate(Avl, New_Avl)
		;
		% il faut faire une rotation gauche_droite
			left_rotate(Gauche, New_Gauche),
			height(New_Gauche, New_HG),
			H_Int is 1+ max(New_HG, HD),
			Avl_Int = avl(New_Gauche, Racine, Droite, H_Int),
			right_rotate(Avl_Int, New_Avl)
		)
	;
	% la suppression n'a pas desequilibre l'avl
		New_Hauteur is 1+max(HG,HD),
		New_Avl = avl(Gauche, Racine, Droite, New_Hauteur)
	).
	
%-----------------------------------------
% Arbres utilises pour les tests unitaires
%-----------------------------------------
avl_test(1, nil).
avl_test(2, avl(nil, 1, nil,              0)).
avl_test(3, avl(nil, 1, avl(nil,2,nil,0), 1)).
avl_test(4, avl(avl(nil,1,nil,0),2, nil,  1)).
avl_test(5, avl(avl(nil,1,nil,0), 2, avl(nil,3,nil,0),1)	).
avl_test(6, avl(avl(nil,5,nil,0), 6, avl(nil,7,nil,0),1)	).
avl_test(7,  avl(G,4,D,2)) :-
	avl_test(5,G),
	avl_test(6,D).
avl_test(8, avl(G,5,D,2)) :-
	D = avl(nil,6,nil,0),
	avl_test(3,G).
avl_test(9, avl(G,3,D,2)) :-
	G = avl(nil,1,nil,0),
	avl_test(4,D).
	
/* Test uniquement valable avec ECLiPSe

avl_test(10, Final) :-
   empty(Init),
   (for(I,1,20), fromto(Init,In,Out,Final) do
     insert(I,In,Out)
   ).
*/

main:-
    initial_state(S0),
    heuristique(S0,H0),
    G0 is 0,
    F0 is G0 + H0,
    empty(Pf),
    empty(Pu),
    empty(Q),
    insert([[F0,H0,G0],S0],Pf,Pf1),
    insert([S0,[F0,H0,G0],nil,nil],Pu,Pu1),
    aetoile(Pf1,Pu1,Q).

aetoile(nil,nil,_Q) :-
    nl, write("PAS de SOLUTION : L’ETAT FINAL N’EST PAS ATTEIGNABLE !"),
    fail.
    
aetoile(Pf,Pu,Q) :-
    suppress_min([_,State],Pf,_),
    belongs([State,_Fhg,Pere,A], Pu, _),  
    final_state(State),
    affiche_solution(Pere,Q),nl,
    write(A),nl,
    write_state(State).

affiche_solution(Pere,Q):-.


