module Skyline where

-- TIPOS DE DATOS

type Edificio = (Int,Int,Int)
type Coordenada = (Int,Int)
type Skyline = [Coordenada]

-- FUNCIONES PRINCIPALES

-- resuelveSkyline
-- Toma como entrada una lista de edificios y devuelve un Skyline, una lista de coordenadas que forman la linea de horizonte
-- Para ello, divide la lista de edificios en dos partes (funcion divide) y hace una llamada recursiva con cada una de las partes
-- combinando las soluciones obtenidas de cada parte (funcion combina)
-- si el resultado de dividir la lista de edificios es un solo edificio, transforma el edificio a Skyline (funcion edificioAskyline)
-- seria el caso base de la recursividad de la funcion resuelveSkyline
resuelveSkyline :: [Edificio] -> Skyline
resuelveSkyline [] = []
resuelveSkyline [x] = edificioAskyline(x)
resuelveSkyline listaEdificios = combina (resuelveSkyline (fst (divide listaEdificios))) (resuelveSkyline (snd (divide listaEdificios))) 

-- edificioAskyline
-- Toma como entrada un edificio y devuelve su Skyline
edificioAskyline :: Edificio -> Skyline
edificioAskyline (x1,x2,h) = [(x1,h),(x2,0)]

-- divide
-- Toma como entrada una lista de edificios y devuelve una tupla formada por dos listas de edificios
-- Cada una de las 2 listas de edificios de salida es la mitad de la lista de edificios de entrada
divide :: [Edificio] -> ([Edificio],[Edificio])
divide [] = ([],[])
divide edificios = splitAt ((length edificios) `div` 2) edificios

-- combina
-- Toma como entrada 2 lineas de horizonte Skyline y devuelve la linea de hotrizonte Skyline como resultado de combinar
-- las 2 lineas de horizonte de entrada
-- Para ello hace uso de la funcion subcombina
combina :: Skyline -> Skyline -> Skyline
combina lineaHorizonte1 lineaHorizonte2 = subcombina lineaHorizonte1 lineaHorizonte2 0 0 0

-- FUNCIONES AUXILIARES

-- ***************************************************************************************
-- subcombina
-- Toma como parametros de entrada 2 lineas de horizonte Skyline, la altura del punto anterior de la linea de horizonte 1,
-- la altura del punto anterior de la linea de horizonte 2 y la altura del ultimo punto extraido de las lineas de horizonte.
-- Devuelve como resultado una linea de horizonte Skyline que es la combinacion de las 2 lineas de horizonte de entrada.
-- Para ello utiliza el proceso descrito en la seccion 2.1.1 del enunciado de la practica.
subcombina :: Skyline -> Skyline -> Int -> Int -> Int -> Skyline
subcombina [] [] _ _ _= []
subcombina [] lineaHorizonte2 _ _ _= lineaHorizonte2 
subcombina lineaHorizonte1 [] _ _ _= lineaHorizonte1
subcombina lineaHorizonte1 lineaHorizonte2 hAnterior1 hAnterior2 ultimaHextraida
     | obtenerX (primerElemento lineaHorizonte1) < obtenerX (primerElemento lineaHorizonte2) = 
        if (ultimaHextraida == (alturaMasGrande lineaHorizonte1 lineaHorizonte2 hAnterior2) ) then
            subcombina (tail lineaHorizonte1) lineaHorizonte2 
                       (obtenerH (primerElemento lineaHorizonte1)) hAnterior2 ultimaHextraida
        else
            [(puntoXmasPequeño lineaHorizonte1 lineaHorizonte2,
            alturaMasGrande lineaHorizonte1 lineaHorizonte2 hAnterior2)] ++ 
            subcombina (tail lineaHorizonte1) lineaHorizonte2 
                       (obtenerH (primerElemento lineaHorizonte1)) hAnterior2 (alturaMasGrande lineaHorizonte1 lineaHorizonte2 hAnterior2)
          
     | obtenerX (primerElemento lineaHorizonte1) > obtenerX (primerElemento lineaHorizonte2) = 
        if (ultimaHextraida == (alturaMasGrande lineaHorizonte1 lineaHorizonte2 hAnterior1) ) then
            subcombina lineaHorizonte1 (tail lineaHorizonte2) 
                       hAnterior1 (obtenerH (primerElemento lineaHorizonte2)) ultimaHextraida
        else
            [(puntoXmasPequeño lineaHorizonte1 lineaHorizonte2,
            alturaMasGrande lineaHorizonte1 lineaHorizonte2 hAnterior1)] ++ 
            subcombina lineaHorizonte1 (tail lineaHorizonte2) 
                       hAnterior1 (obtenerH (primerElemento lineaHorizonte2)) (alturaMasGrande lineaHorizonte1 lineaHorizonte2 hAnterior1)

     |otherwise =
        if (ultimaHextraida == (alturaMasGrande lineaHorizonte1 lineaHorizonte2 0) ) then
            subcombina (tail lineaHorizonte1) (tail lineaHorizonte2) 
                       (obtenerH (primerElemento lineaHorizonte1)) (obtenerH (primerElemento lineaHorizonte2)) ultimaHextraida
        else
            [(puntoXmasPequeño lineaHorizonte1 lineaHorizonte2,
            alturaMasGrande lineaHorizonte1 lineaHorizonte2 0)] ++ 
            subcombina (tail lineaHorizonte1) (tail lineaHorizonte2) 
                       (obtenerH (primerElemento lineaHorizonte1)) (obtenerH (primerElemento lineaHorizonte2)) (alturaMasGrande lineaHorizonte1 lineaHorizonte2 0)
-- ***************************************************************************************

-- ***************************************************************************************
-- puntoXmasPequeño
-- devuelve el la coordenada x del punto mas pequeño en la coordenada x
-- para ello, obtiene el primer elemento de cada linea de horizonte
-- obtiene el menor de los dos elementos anteriores en su coordenada x
-- y devuelve la coordenada x 
puntoXmasPequeño :: Skyline -> Skyline -> Int
puntoXmasPequeño lHorizonte1 lHorizonte2 = obtenerX (seleccionaElemento (primerElemento lHorizonte1) (primerElemento lHorizonte2))
-- ***************************************************************************************


-- ***************************************************************************************
-- selecciona el elemento con la coordenada x mas pequeña
-- si ambos tienen la misma coordenada, selecciona el elemento de mayor altura
seleccionaElemento :: Coordenada -> Coordenada -> Coordenada
seleccionaElemento ele1 ele2 
     | obtenerX ele1 < obtenerX ele2 = ele1
     | obtenerX ele2 < obtenerX ele1 = ele2
     | otherwise = elementoAlturaMasGrande ele1 ele2
-- ***************************************************************************************


-- ***************************************************************************************
-- elementoAlturaMasGrande
-- selecciona el elemento con la altura mas grande
elementoAlturaMasGrande :: Coordenada -> Coordenada -> Coordenada
elementoAlturaMasGrande ele1 ele2 
     | obtenerH ele1 > obtenerH ele2 = ele1
     | obtenerH ele2 > obtenerH ele1 = ele2
     | otherwise = ele1
-- ***************************************************************************************

-- ***************************************************************************************
-- alturaMasGrande
-- devuelve la altura mas grande de dos alturas dadas
-- para ello, obtiene el primer elemento de cada lista de horizonte
-- obtiene el menor de los dos elementos anteriores en su coordenada x
-- compara la altura del elemento obtenido con la altura del ultimo elemento consumido
-- y devuelve la mayor de las alturas
alturaMasGrande :: Skyline -> Skyline -> Int -> Int
alturaMasGrande lHorizonte1 lHorizonte2 hAnterior = maximo (obtenerH (seleccionaElemento (primerElemento lHorizonte1) (primerElemento lHorizonte2))) hAnterior
-- ***************************************************************************************

-- ***************************************************************************************
-- Toma como entrada 2 valores enteros y devuelve el mayor de los 2
maximo :: Int -> Int -> Int
maximo hActual hAnterior 
     | hActual > hAnterior = hActual
     | hAnterior > hActual = hAnterior
     | otherwise = hActual
-- ***************************************************************************************

-- ***************************************************************************************
-- obtenerX devuelve la X de una coordenada
obtenerX (x,_) = x
-- obtenerH devuelve la altura de una coordenada
obtenerH (_,h) = h
-- ***************************************************************************************


-- ***************************************************************************************
-- primerElemento
-- devuelve el primer elemento de una linea de horizonte
-- devuelve la primera coordenada
primerElemento :: Skyline -> Coordenada
primerElemento x = head x
-- ***************************************************************************************

-- CUESTIONES SOBRE LA PRACTICA
-- 1a. FUNCION DIBUJA SKYLINE

-- dibujaSkyline 
-- Toma como entrada un Skiline y lo transforma en un string de * y caracteres en blanco
-- el * representa la presencia de edificio y el caracter blanco la ausencia de este
-- el string termina con una linea de guiones que representan el suelo
dibujaSkyline :: Skyline -> String
dibujaSkyline listaSkyline =
     "" ++ (subDibujaSkyline (listaDeAlturas listaSkyline 0 0) (alturaMaximaSkyline listaSkyline 0))


-- subDibujaSkyline
-- Toma como entrada un Skyline transformado en una lista de alturas y la linea a dibujar
-- y devuelve un string con la linea a mostrar en pantalla
subDibujaSkyline :: Skyline -> Int -> String
subDibujaSkyline listaH 0 = dibujaGuiones listaH
subDibujaSkyline listaH altura =
     (dibujaLinea listaH altura) ++ (subDibujaSkyline listaH (altura - 1))

-- listaDeAlturas
-- Transforma un Skyline en una lista (tambien Skyline) con las alturas
-- de todas las coordenadas x del Skyline
-- para ello, para ello, comprueba si cada coordenada x es un punto del Skiline
-- si lo es, añade el punto del skyline a la lista de alturas
-- si no lo es, añade a la lista de alturas la coordena x que se esta comprobando junto con la altura del ultimo
-- punto encontrado del skyline
listaDeAlturas :: Skyline -> Int -> Int -> Skyline
listaDeAlturas [] _ _= []
--listaDeAlturas [x] _= [x]
listaDeAlturas listaH coordenadaX alturaH= 
     if (obtenerX (primerElemento listaH) == coordenadaX) then
          [primerElemento listaH] ++ listaDeAlturas (tail listaH) (coordenadaX + 1) (obtenerH(primerElemento listaH)) 
     else
          [(coordenadaX,alturaH)] ++ listaDeAlturas listaH (coordenadaX + 1) alturaH


-- alturaMaximaSkyline
-- devuelve la altura maxima del skyline
-- para ello, almacena la altura maxima encontrada y la compara con la altura del punto x del skyline que se esta examinando
-- si el punto tiene una altura mayor, actualiza la altura almacenada con la altura del punto examinado
-- y continua con el siguiente punto del skyline
alturaMaximaSkyline :: Skyline -> Int -> Int
alturaMaximaSkyline [] alturaMax = alturaMax
alturaMaximaSkyline listaH alturaMax =
     if (alturaMax < obtenerH(primerElemento listaH)) then
          alturaMaximaSkyline (tail listaH) (obtenerH(primerElemento listaH))
     else
          alturaMaximaSkyline (tail listaH) alturaMax

-- dibujaLinea
-- Toma como entrada un Skyline y la altura de este a dibujar
-- y devuelve un string con la linea a mostrar en pantalla formado por * para las zonas donde hay edificio
-- y espacios en blanco para las zonas donde no hay edificios
-- muestra una linea de guiones para la altura 0, representando el suelo sobre el que se alza el edificio
dibujaLinea :: Skyline -> Int -> String
dibujaLinea listaSkyline 0 = dibujaGuiones listaSkyline
dibujaLinea [] _ = "\n"
dibujaLinea listaSkyline altura =
     if ( (obtenerH(primerElemento listaSkyline)) >= altura) then
          "*" ++ (dibujaLinea (tail listaSkyline) altura)
     else
          " " ++ (dibujaLinea (tail listaSkyline) altura)

-- dibujaGuiones
-- dibuja una linea de guiones representando el suelo sobre el que se alza el edificio
dibujaGuiones :: Skyline -> String
dibujaGuiones [] = ""
dibujaGuiones listaSkyline = "-" ++ (dibujaGuiones (tail listaSkyline))

-- muestra en pantalla un string
write :: String -> IO()
write [] = putChar '\n'
write (x:xs) = putChar x >> write xs





