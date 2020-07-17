import System.IO
import System.Exit
import Control.Monad
import System.Process

-- Autor: Andrea Vázquez Varela
-- Login: andrea.vazquez.varela
-- Correo: andrea.vazquez.varela@udc.es

-- Definición de la transición y el autómata
data Transicion = Transicion { origen :: Char, destino :: Char, simbolo :: Char} deriving (Show)
data Automata = Automata { estados :: String, alfabeto :: String, inicio :: Char, finales :: String, transiciones :: [Transicion]} deriving (Show)

-- Función que elimina los blancos de un String
eliminarBlancos:: String -> String
eliminarBlancos st = [ c | c <- st, c /= ' ']
											
-- Corta la cadena por el principio un determinado número de elementos
cortarCadena :: Int -> String -> String
cortarCadena n st
						| n == 0 = st
						|otherwise = do (cortarCadena (n-1) (tail st))

-- Rellena un String hasta encontrar un ;, esta función sirve para rellenar los estados, el alfabeto, los estados finales...
rellenar:: String -> String -> String
rellenar e st
						| head st == ';' = e
						| otherwise = do
							rellenar (e++[(head st)]) (tail st)

-- Rellena una transición, rellena su origen, su destino y su símbolo
rellenarT:: String -> Transicion
rellenarT st = do
						let o = head(st)
						let d = head(tail st)
						let s = head(tail (tail st))
						Transicion o d s --Devuelve una transición

--Rellena un array de transiciones
rellenarTransiciones:: Int -> String -> [Transicion]
rellenarTransiciones n st
						| n == 0 = []
						| otherwise = do
						  let t = [rellenarT st]
						  t++(rellenarTransiciones (n-1) (cortarCadena 4 st)) --Utiliza la recursión hasta llenar todas las transiciones

--Obtiene la transición n de un array de transiciones
getTransicion:: Int -> [Transicion] -> Transicion
getTransicion n t
						| n == 0 = head (t)
						| otherwise = do
							getTransicion (n-1) (tail t)


getOrigen :: Transicion -> Char
getOrigen t = origen(t)
getDestino :: Transicion -> Char
getDestino t = destino(t)
getSimbolo :: Transicion -> Char
getSimbolo t = simbolo(t)

--Rellena todos los elementos de un autómata
rellenarAutomata:: String -> Automata
rellenarAutomata st = do
						 let e = rellenar "" st
						 let a = rellenar "" (cortarCadena ((length e) + 1) st) --Se le suma 1 para saltarse el ;
						 let i = head (rellenar "" (cortarCadena ((length a) + (length e) + 2) st)) --Cada vez que se rellena una parte del autómata se le suman enteros para saltarse los ;
						 let f = rellenar "" (cortarCadena ((length a) + (length e) + 4) st)
						 let trans = (cortarCadena ((length a) + (length e) + length(f) + 5) st)
						 let tamT = quot (length trans)  4
						 let t = rellenarTransiciones tamT trans
						 let afd = Automata e a i f t
						 afd

--Añade un estado a la lista de estados visitados
addVisitados::Char -> Int -> String
addVisitados visit tam
						|tam == 0 = "" --Solo se añade el estado si el array siguientes no es vacio
						|otherwise = do 
								[visit]

--Mientras la cola no esté vacía, comprueba si el elemento "head" ha sido visitado y posteriormente añade sus elementos siguientes cuyo destino no haya sido visitado a la cola.
comprobarSiguientes :: String -> [Transicion] -> [Transicion] -> Int -> String
comprobarSiguientes visitados trans cola tamC
						| tamC== 0 = visitados
						| otherwise = do
								let c = cola
								let actual = head cola
								let siguientes =  [ t | t <- trans,(getOrigen t) == (getDestino actual) ,((getDestino actual) `elem` visitados) == False]
								let destino = getDestino actual
								let v = visitados++(addVisitados destino (length siguientes))
								comprobarSiguientes v trans ((tail c)++siguientes) (length ((tail c)++siguientes))
	
--Primero añade a la cola las transiciones del elemento incial (añadiendo éste a visitados) y después comprueba los estados siguientes
estadosConexos :: Automata -> String
estadosConexos afd = do
						let trans = transiciones(afd)
						let i = inicio(afd)
						let iniciales = [ t | t <- trans, (i == (getOrigen t))]
						let cola = iniciales
						let tamC = 1
						let visitados= [i]
						comprobarSiguientes visitados trans cola tamC

--Elimina los estados, estados finales y transiciones que no están en la lista de visitados.
eliminarEstadosNC:: Automata -> String -> Automata
eliminarEstadosNC afd visitados = do
						let e = [c | c <-estados(afd),c `elem` visitados]
						let a = alfabeto(afd)
						let f = [c | c <-finales(afd),c `elem` visitados]
						let i = inicio(afd)
						let t = [tr | tr <-transiciones(afd),(getOrigen tr) `elem` visitados,(getDestino tr) `elem` visitados]
						Automata e a i f t

--Devuelve el subAutómata conexo				
subAutomataConexo:: Automata -> Automata
subAutomataConexo afd = do
						let visitados = estadosConexos afd
						eliminarEstadosNC afd visitados

--Crea una cadena de estados con el formato especificado para el fichero .dot
imprimirEstados n e st
						|n == 0 = e
						|otherwise = do
							"q"++[head(st)]++"; "++(imprimirEstados (n-1) e (tail st))					

--Crea una de transiciones con el formato especificado para el fichero .dot
imprimirTransiciones n e t
							|n == 0 = e
							|otherwise = do
								"q"++([getOrigen (getTransicion (n-1) t)])++" -> "++"q"++([getDestino (getTransicion (n-1) t)])++" [label=\""++([getSimbolo (getTransicion (n-1) t)])++"\"];\n" ++ (imprimirTransiciones (n - 1) e t)

--Crea el fichero .dot pasandole una cadena con un formato determinado y los estados y transiciones
dibujarAutomata afd nombre= do
						let f = finales(afd)
						let t = transiciones(afd)
						let imprimirE = "digraph example {\nnode [style=invis]; 00; \nnode [shape=doublecircle, height=0.5, width=0.5, fixedsize=true, style=solid]; "++(imprimirEstados (length f) "" f)++"\nnode [shape=circle,height=0.5,width=0.5,fixedsize=true, style=solid]; \nrankdir = LR;\n\n00 -> q"++[inicio(afd)]++";\n"++(imprimirTransiciones (length t) "" t)++"\n}"
						writeFile nombre imprimirE

--Se repite esta secuencia siempre, primero imprime el menú, luego selecciona una opción y luego realiza una accón. Se repite hasta que la opción sea 5
main = forever (imprimirMenu >> opcion >>= accion)
imprimirMenu = putStr "\n\nSUB AUTOMATA CONEXO\n\nIntroduzca una opción:\n\n1 -------- Paso 1 Introducir el autómata no conexo en el fichero de texto\n\n2 -------- Paso 2 Crear el automata no conexo y eliminar los estados inaccesibles\n\n3 ---------Dibujar el automata no conexo\n\n4 ---------Dibujar el automata conexo\n\n5 ---------Salir" >> hFlush stdout

opcion = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar

accion '1' = do 
				ExitSuccess <- system "clear"
				ExitSuccess <- system "xdg-open automata.txt"
  				return ()
accion '2' = do
				ExitSuccess <- system "clear"
				cadena <- readFile "automata.txt"
				let sinBlancos = eliminarBlancos cadena
				let afd = rellenarAutomata sinBlancos
				putStr "\nEl automata ha sido rellenado con exito\n"
				dibujarAutomata afd "noconexo.dot"	
				ExitSuccess <- system "dot -Tps -o noconexo.eps noconexo.dot"
				let conexo = (subAutomataConexo afd)
				dibujarAutomata conexo "conexo.dot"
				ExitSuccess <- system "dot -Tps -o conexo.eps conexo.dot"
				return ()
accion '3' = do
  			ExitSuccess <- system "clear"
			ExitSuccess <- system "display noconexo.dot"
  			return ()
accion '4' = do
  			ExitSuccess <- system "clear"
			ExitSuccess <- system "display conexo.dot"
			return ()
accion '5' = exitSuccess
accion _ = hPutStrLn stderr "\nInvalid choice."


