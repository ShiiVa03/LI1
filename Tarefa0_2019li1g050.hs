-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g050 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo
    deriving(Show,Eq)

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
transformaEmCartesiano :: Vetor -> Vetor
transformaEmCartesiano (Polar r a) = (Cartesiano (r * cos((a*pi)/180)) (r * sin((a*pi)/180)))

somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1)(Cartesiano x2 y2) = (Cartesiano (x1+x2) (y1+y2))
somaVetores (Polar r1 a1)(Polar r2 a2) = somaVetores cp1 cp2
    where cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)

somaVetores (Polar r3 a3)(Cartesiano x3 y3) = somaVetores cp3 (Cartesiano x3 y3)
    where cp3 = transformaEmCartesiano (Polar r3 a3)

somaVetores (Cartesiano x4 y4)(Polar r4 a4) = somaVetores (Cartesiano x4 y4) cp4
    where cp4 = transformaEmCartesiano (Polar r4 a4)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1)(Cartesiano x2 y2) = (Cartesiano (x1-x2) (y1-y2))
subtraiVetores (Polar r1 a1)(Polar r2 a2) = subtraiVetores cp1 cp2
    where cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)

subtraiVetores (Polar r3 a3)(Cartesiano x3 y3) = subtraiVetores cp3 (Cartesiano x3 y3)
    where cp3 = transformaEmCartesiano (Polar r3 a3)

subtraiVetores (Cartesiano x4 y4)(Polar r4 a4) = subtraiVetores (Cartesiano x4 y4) cp4
    where cp4 = transformaEmCartesiano (Polar r4 a4)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor escalar (Cartesiano x1 y1) = (Cartesiano (escalar*x1) (escalar*y1)) 
multiplicaVetor escalar (Polar r1 a1) = multiplicaVetor escalar cp1
    where cp1 = transformaEmCartesiano (Polar r1 a1)
          

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))((Cartesiano x3 y3),(Cartesiano x4 y4)) = if 0 <= t1 && t1 <= 1 && 0 <= t2 && t2 <= 1
                                                                                            then True
                                                                                            else False
    where 
          t1 = ((y3 - y4) * (x1 - x3) + (x4 - x3) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))
          t2 = ((y1 - y2) * (x1 - x3) + (x2 - x1) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))

intersetam ((Polar r1 a1),(Polar r2 a2))((Polar r3 a3),(Polar r4 a4)) = intersetam (cp1,cp2)(cp3,cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Polar r1 a1),(Polar r2 a2))((Polar r3 a3),(Cartesiano x1 y1)) = intersetam (cp1,cp2)(cp3,(Cartesiano x1 y1))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersetam ((Polar r1 a1),(Polar r2 a2))((Cartesiano x1 y1),(Polar r4 a4)) = intersetam (cp1,cp2)((Cartesiano x1 y1),cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Polar r1 a1),(Cartesiano x1 y1))((Polar r3 a3),(Polar r4 a4)) = intersetam (cp1,(Cartesiano x1 y1))(cp3,cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Cartesiano x1 y1),(Polar r2 a2))((Polar r3 a3),(Polar r4 a4)) = intersetam ((Cartesiano x1 y1),cp2)(cp3,cp4)
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))((Polar r3 a3),(Polar r4 a4)) = intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))(cp3,cp4)
    where    
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Polar r1 a1),(Polar r2 a2))((Cartesiano x1 y1),(Cartesiano x2 y2)) = intersetam (cp1,cp2)((Cartesiano x1 y1),(Cartesiano x2 y2))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)

intersetam ((Polar r1 a1),(Cartesiano x1 y1))((Polar r3 a3),(Cartesiano x2 y2)) = intersetam (cp1,(Cartesiano x1 y1))(cp3,(Cartesiano x2 y2))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersetam ((Polar r1 a1),(Cartesiano x1 y1))((Cartesiano x2 y2),(Polar r4 a4)) = intersetam (cp1,(Cartesiano x1 y1))((Cartesiano x2 y2),cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Polar r1 a1),(Cartesiano x1 y1))((Cartesiano x2 y2),(Cartesiano x3 y3)) = intersetam (cp1,(Cartesiano x1 y1))((Cartesiano x2 y2),(Cartesiano x3 y3))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)

intersetam ((Cartesiano x1 y1),(Polar r2 a2))((Polar r3 a3),(Cartesiano x2 y2)) = intersetam ((Cartesiano x1 y1),cp2)(cp3,(Cartesiano x2 y2))
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersetam ((Cartesiano x1 y1),(Polar r2 a2))((Cartesiano x2 y2),(Polar r4 a4)) = intersetam ((Cartesiano x1 y1),cp2)((Cartesiano x2 y2),cp4)
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))((Polar r3 a3),(Cartesiano x3 y3)) = intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))(cp3,(Cartesiano x3 y3))
    where    
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersetam ((Cartesiano x1 y1),(Polar r2 a2))((Cartesiano x2 y2),(Cartesiano x3 y3)) = intersetam ((Cartesiano x1 y1),cp2)((Cartesiano x2 y2),(Cartesiano x3 y3))
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)

intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))((Cartesiano x3 y3),(Polar r4 a4)) = intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2))((Cartesiano x3 y3),cp4)
    where    
          cp4 = transformaEmCartesiano (Polar r4 a4)


-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))((Cartesiano x3 y3),(Cartesiano x4 y4)) | 0 <= t1 && t1 <= 1 && 0 <= t2 && t2 <= 1 = (Cartesiano (x1 + t1 * (x2 - x1))(y1 + t1 *(y2 - y1)))                                                                                
    where 
          t1 = ((y3 - y4) * (x1 - x3) + (x4 - x3) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))
          t2 = ((y1 - y2) * (x1 - x3) + (x2 - x1) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))

intersecao ((Polar r1 a1),(Polar r2 a2))((Polar r3 a3),(Polar r4 a4)) = intersecao (cp1,cp2)(cp3,cp4)                                                                       
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Polar r1 a1),(Polar r2 a2))((Polar r3 a3),(Cartesiano x1 y1)) = intersecao (cp1,cp2)(cp3,(Cartesiano x1 y1))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersecao ((Polar r1 a1),(Polar r2 a2))((Cartesiano x1 y1),(Polar r4 a4)) = intersecao (cp1,cp2)((Cartesiano x1 y1),cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Polar r1 a1),(Cartesiano x1 y1))((Polar r3 a3),(Polar r4 a4)) = intersecao (cp1,(Cartesiano x1 y1))(cp3,cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Cartesiano x1 y1),(Polar r2 a2))((Polar r3 a3),(Polar r4 a4)) = intersecao ((Cartesiano x1 y1),cp2)(cp3,cp4)
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))((Polar r3 a3),(Polar r4 a4)) = intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))(cp3,cp4)
    where    
          cp3 = transformaEmCartesiano (Polar r3 a3)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Polar r1 a1),(Polar r2 a2))((Cartesiano x1 y1),(Cartesiano x2 y2)) = intersecao (cp1,cp2)((Cartesiano x1 y1),(Cartesiano x2 y2))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp2 = transformaEmCartesiano (Polar r2 a2)

intersecao ((Polar r1 a1),(Cartesiano x1 y1))((Polar r3 a3),(Cartesiano x2 y2)) = intersecao (cp1,(Cartesiano x1 y1))(cp3,(Cartesiano x2 y2))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersecao ((Polar r1 a1),(Cartesiano x1 y1))((Cartesiano x2 y2),(Polar r4 a4)) = intersecao (cp1,(Cartesiano x1 y1))((Cartesiano x2 y2),cp4)
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Polar r1 a1),(Cartesiano x1 y1))((Cartesiano x2 y2),(Cartesiano x3 y3)) = intersecao (cp1,(Cartesiano x1 y1))((Cartesiano x2 y2),(Cartesiano x3 y3))
    where    
          cp1 = transformaEmCartesiano (Polar r1 a1)

intersecao ((Cartesiano x1 y1),(Polar r2 a2))((Polar r3 a3),(Cartesiano x2 y2)) = intersecao ((Cartesiano x1 y1),cp2)(cp3,(Cartesiano x2 y2))
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersecao ((Cartesiano x1 y1),(Polar r2 a2))((Cartesiano x2 y2),(Polar r4 a4)) = intersecao ((Cartesiano x1 y1),cp2)((Cartesiano x2 y2),cp4)
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)
          cp4 = transformaEmCartesiano (Polar r4 a4)

intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))((Polar r3 a3),(Cartesiano x3 y3)) = intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))(cp3,(Cartesiano x3 y3))
    where    
          cp3 = transformaEmCartesiano (Polar r3 a3)

intersecao ((Cartesiano x1 y1),(Polar r2 a2))((Cartesiano x2 y2),(Cartesiano x3 y3)) = intersecao ((Cartesiano x1 y1),cp2)((Cartesiano x2 y2),(Cartesiano x3 y3))
    where    
          cp2 = transformaEmCartesiano (Polar r2 a2)

intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))((Cartesiano x3 y3),(Polar r4 a4)) = intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2))((Cartesiano x3 y3),cp4)
    where    
          cp4 = transformaEmCartesiano (Polar r4 a4)


-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l = x <= (length l  - 1) && x >= 0

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz (h:t) = if length h > 0 then (linhas,colunas)
                       else (0,0)
    where colunas = length h
          linhas = (length t) + 1

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (a,b) [] = False
ePosicaoMatrizValida (a,b) m = a >= 0 && a < c && b >= 0 && b < d
    where (c,d) = dimensaoMatriz m

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo angulo = if angulo >= 0 && angulo < 360
                         then angulo
                         else if angulo < 0
                              then normalizaAngulo (angulo+360)
                              else normalizaAngulo (angulo-360)

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista indice (h:t) =  if (length(h:t) - indice) == length(h:t) && indice < length(h:t)
                                    then h
                                    else encontraIndiceLista (indice-1) t
                                    

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista indice elemento l = if indice < length l && indice >= 0
                                        then let (l1,h:t) = splitAt indice l
                                             in l1 ++ [elemento] ++ t
                                        else l

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (l,c) m = encontraIndiceLista c linhas
    where linhas = encontraIndiceLista l m

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) elemento m = if (l,c) < dimensaoMatriz m && l >= 0 && c >= 0
                                         then atualizaIndiceLista l  colunaAtualizada m 
                                         else m
    where linha = encontraIndiceLista l m
          colunaAtualizada = atualizaIndiceLista c elemento linha

