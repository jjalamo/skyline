%Predicados a implementar

%------------------------------------------
% Implementa el predicado principal de la practica.
% Toma una lista de edificios y devuelve su linea de horizinte.
resuelveSkyline([],[]).
resuelveSkyline([Ed],LineaH) :- edificioAskyLine(Ed,LineaH).
resuelveSkyline(ListaEd,LineaH) :- divide(ListaEd,ListaEd1,ListaEd2),
				   resuelveSkyline(ListaEd1,LineaH1),
				   resuelveSkyline(ListaEd2,LineaH2),
				   combina(LineaH1,LineaH2,LineaH).
%------------------------------------------

%------------------------------------------
% Transforma un edificio a su linea de horizonte.
%  Toma un edificio de la forma ed(X1,X2,H) y devuelve la linea de
%  horizonte del edificio en la forma [c(X1,H),c(X2,0)]
edificioAskyLine(ed(X1,X2,H),[c(X1,H),c(X2,0)]).
%------------------------------------------

%------------------------------------------
% Divide una lista de edificios en dos listas de edificios
divide([],[],[]).
divide(ListaEd,ListaEd1,ListaEd2) :- append(ListaEd1,ListaEd2,ListaEd),
				     length(ListaEd,Long0),
				     length(ListaEd1,Long1),
				     Mitad is Long0//2,
				     Long1==Mitad.
%------------------------------------------

%------------------------------------------
% Combina dos lineas de horizonte en una sola linea de horizonte
combina(LineaH1,LineaH2,LineaH) :- subCombina(LineaH1,LineaH2,0,0,0,LH),
				   limpiarLH(LH,LineaH).
%------------------------------------------

% Predicados Auxiliares

%------------------------------------------
% Combina dos lineas de horizonte en una sola linea de horizonte.
% Toma como parametros las dos lineas de horizonte a combinar, la altura
% del primer punto de cada linea de horizonte tratados en el ciclo
% anterior y la ultima altura que se ha extraido de una de las lineas de
% horizonte. La combinacion de las dos lineas de horizonte se realiza
% segun el proceso descrito den la seccion 2.1.1 del enunciado de la
% practica. En caso de que en un ciclo no sea necesario añadir ningun
% punto nuevo a la linea de horizonte resultante, se añadira un punto
% nulo de la forma c(0,0), de esta forma, se evita que prolog genere una
% evaluacion false y aborte el programa. Posteriormente, se eliminaran
% de la linea de horizonte resultante estos puntos nulos.
subCombina([],L2,_,_,_,L2).
subCombina(L1,[],_,_,_,L1).
subCombina([],[],_,_,_,[]).
subCombina(L1,L2,Hanterior1,Hanterior2,UltimaHextraida,R2) :-
        L1=[Cab1|Col1], L2=[Cab2|Col2],
	obtenerX(Cab1,CX1),
	obtenerX(Cab2,CX2),
	(
	    (   CX1<CX2 ->
	             alturaMasGrande(Cab1,c(0,0),Hanterior2,HmasGrande),
		     (
			 (   UltimaHextraida==HmasGrande ->
			          obtenerH(Cab1,H1),
			          subCombina(Col1,L2,H1,Hanterior2,UltimaHextraida,R),
			          añadir(c(0,0),R,R2)
			  );
			 (   UltimaHextraida\=HmasGrande ->
			          obtenerH(Cab1,H1),
			          subCombina(Col1,L2,H1,Hanterior2,HmasGrande,R),
			          obtenerCoordenadaXmasPequeña(Cab1,Cab2,CoordenadaX),
			          crearPunto(CoordenadaX,HmasGrande,Punto),
			          añadir(Punto,R,R2)
			  )
		     )
	    );
	    (   CX1>CX2 ->
	             alturaMasGrande(c(0,0),Cab2,Hanterior1,HmasGrande),
		     (
			 (   UltimaHextraida==HmasGrande ->
			          obtenerH(Cab2,H2),
			          subCombina(L1,Col2,Hanterior1,H2,UltimaHextraida,R),
			          añadir(c(0,0),R,R2)
			  );
			 (   UltimaHextraida\=HmasGrande ->
			          obtenerH(Cab2,H2),
			          subCombina(L1,Col2,Hanterior1,H2,HmasGrande,R),
			          obtenerCoordenadaXmasPequeña(Cab1,Cab2,CoordenadaX),
			          crearPunto(CoordenadaX,HmasGrande,Punto),
			          añadir(Punto,R,R2)
			  )
		     )
	    );
	    (	CX1==CX2 ->
	             alturaMasGrande(Cab1,Cab2,0,HmasGrande),
		     (
			 (   UltimaHextraida==HmasGrande ->
			          obtenerH(Cab1,H1),
			          obtenerH(Cab2,H2),
			          subCombina(Col1,Col2,H1,H2,UltimaHextraida,R),
			          añadir(c(0,0),R,R2)
			  );
			 (   UltimaHextraida\=HmasGrande ->
			          obtenerH(Cab1,H1),
			          obtenerH(Cab2,H2),
			          subCombina(Col1,Col2,H1,H2,HmasGrande,R),
				  obtenerCoordenadaXmasPequeña(Cab1,Cab2,CoordenadaX),
			          crearPunto(CoordenadaX,HmasGrande,Punto),
			          añadir(Punto,R,R2)
			  )
		     )
	    )
	).
%------------------------------------------

%------------------------------------------
% Elimina de la lina de horizonte los puntos nulos de la forma c(0,0)
% introducidos por el predicado subcombina.
limpiarLH([],[]).
limpiarLH([Cab|Col],LH) :-    (   Cab\=c(0,0) -> limpiarLH(Col,R),
			                         añadir(Cab,R,LH)
			      );
                              (   Cab==c(0,0) -> limpiarLH(Col,R),
			                         LH=R
			      ).
%------------------------------------------

%------------------------------------------
% Crea un nuevo punto, coordenada, de la forma c(X,H), siendo X la
% coordenada del eje X y H la altura.
crearPunto(Cx,Ch,c(Cx,Ch)).
%------------------------------------------

%------------------------------------------
% Dados 2 puntos en la forma c(X,H), devuelve la coordenada X mas
% pequeña de los dos puntos.
obtenerCoordenadaXmasPequeña(c(X1,_),c(X2,_),X) :- (X1>X2 -> X=X2);
                                                   (X1<X2 -> X=X1);
						   (X1==X2 -> X=X1).
%------------------------------------------

%------------------------------------------
% Dados dos puntos en la forma c(X,H) y una altura H, devuelve la altura
% mas grande de los dos puntos y la altura H.
alturaMasGrande(C1,C2,H,Hmax) :- C1=c(_,H1), C2=c(_,H2),
				 valorMayor(H1,H2,Haux),
				 valorMayor(Haux,H,Hmax).
%------------------------------------------

%------------------------------------------
% Dados 2 valores X Y, devuelve el mayor de los dos
valorMayor(X,Y,R) :- (X>Y -> R=X);
                     (X<Y -> R=Y);
		     (X==Y -> R=X).
%------------------------------------------

%------------------------------------------
% Dado un punto en la forma c(X,H), devuelve el valor de la coordenada X
% del punto
obtenerX(E,CX) :- E=c(CX,_).
%------------------------------------------

%------------------------------------------
%Dado un punto en la forma c(X,H), devuelve la altura H del punto
obtenerH(E,H) :- E=c(_,H).
%------------------------------------------

%------------------------------------------
% Dado un elemento E y una lista L1, añade el elemento E al principio de
% la lista L1
añadir(E,[],[E]).
añadir(E,L1,[E|L1]).
%------------------------------------------

% Cuestiones sobre la practica. Parte Optativa.
% Punto 1a. Dibuja Skyline.

%------------------------------------------
% Muestra en pantalla un dibujo de la la linea de horizonte dada
% Para ello transforma la linea de horizonte en una lista de alturas,
% segun lo especificado en el punto 3 del enunciado de la practica.
% Obtiene la mayor altura de la linea de horizonte y la dibuja en
% pantalla
dibujaSkyline(LineaH) :- obtenerListaAlturas(LineaH,0,c(0,0),LA),
	                 obtenerAlturaMaxima(LineaH,AlturaSkyline),
                         dibuja(LA,AlturaSkyline).
%------------------------------------------

%------------------------------------------
% Dibuja en pantalla la linea de horizonte dada, linea por linea,
% comenzando por la linea de mayor altura hasta llegar a la linea de
% altura 0 en la que dibujara el suelo
dibuja(ListaH,0) :- dibujaSuelo(ListaH).
dibuja(ListaH,Altura) :- Alt is Altura-1,
	                 dibujaLinea(ListaH,Altura),
			 dibuja(ListaH,Alt).
%------------------------------------------

%------------------------------------------
% Dibuja la linea de la altura especificada por Altura
dibujaLinea([],_) :- write_ln(" ").
dibujaLinea(ListaH,Altura) :- ListaH = [Cab|Col],
			      obtenerH(Cab,H),
			      dibujaPunto(H,Altura),
			      dibujaLinea(Col,Altura).
%------------------------------------------

%------------------------------------------
% Dibuja el suelo del skyline en forma de - en la altura 0
dibujaSuelo([]) :- write_ln(" ").
dibujaSuelo(ListaH) :- ListaH = [_|Col],
		       write("-"),
		       dibujaSuelo(Col).
%------------------------------------------

%------------------------------------------
% Dibuja un punto en pantalla, muestra un * si hay edificio y un espacio
% blanco si no lo hay
dibujaPunto(P,Alt) :- (   P > Alt -> write("*"));
		      (	  P == Alt -> write("*"));
		      (	  P < Alt -> write(" ")).
%------------------------------------------

%------------------------------------------
% Transforma la linea de horizonte en una lista de alturas, segun lo
% especificado en el apartado 3 del eneunciado de la practica.
% En la lista de alturas, todas las coordenadas del eje X tienen un
% punto. En la linea de horizonte solo tiene un punto aquellas
% coordenadas del eje X que coincidan con un cambio de altura.
% En la linea de alturas, las coordenadas el eje X que no coincidan con
% un cambio de altura, tendran la misma altura que la ultima coordenada
% X que tenga un cambio de altura
obtenerListaAlturas([],_,_,[]).
obtenerListaAlturas(LineaH,CoX,PuntoAnterior,R2) :-
	   LineaH = [Cab|Col],
	   obtenerX(Cab,CX),
	   X is CoX+1,
	   (
	       (   CoX==CX ->
	                      obtenerListaAlturas(Col,X,Cab,R),
	                      añadir(Cab,R,R2)
	        );
	       (   CoX\=CX ->
			      obtenerListaAlturas(LineaH,X,PuntoAnterior,R),
		              obtenerH(PuntoAnterior,Hanterior),
		              crearPunto(CoX,Hanterior,Punto),
	                      añadir(Punto,R,R2)
	        )
	   ).
%------------------------------------------

%------------------------------------------
% Obtiene la altura mayor de una linea de horizonte.
obtenerAlturaMaxima([],0).
obtenerAlturaMaxima(LineaH,Hmax) :-
	LineaH = [Cab|Col],
        obtenerH(Cab,H),
	obtenerAlturaMaxima(Col,Haux),
	valorMayor(H,Haux,Hmax).
%------------------------------------------

























































