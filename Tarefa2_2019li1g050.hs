-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g050 where

import LI11920
import Testes
import Tarefa0_2019li1g050

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(3,play1,e0),(4,play0,e0),(5,play1,e0),(6,play0,e0),(1,play0,e0),(6,play1,e0),(1,play1,e0),(0,play0,e0),(2,play1,e0),(6,play0,e0),(7,play0,e0),(8,play1,e0),(0,play4,e0),(9,play4,e0),(10,play4,e0),(11,play4,e0),(0,play5,e0),(9,play5,e0),(10,play5,e0),(11,play5,e0),(12,play6,e0),(8,play6,e0),(10,play6,e0),(11,play6,e0),(15,play6,e0),(8,play3,e0),(8,play2,e0),(10,play3,e0),(10,play2,e0),(11,play3,e0),(11,play2,e0),(13,play3,e0),(14,play2,e0)]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -> Jogada -> Estado -> Estado    
jogada jogadorID jogada e@(Estado m jogadores) = jogadaJogadorEstado jogadorID (devolveEstadoJogador(encontraIndiceLista jogadorID jogadores)) jogada e

-- | Função dado um 'Jogador' verifica se ele pode efetuar a 'Jogada'
jogadaJogadorEstado :: Int -> EstadoJogador -> Jogada -> Estado -> Estado
jogadaJogadorEstado jogadorID (Chao _) jogada estadoDoJogo = jogadaNoChao jogadorID jogada estadoDoJogo
jogadaJogadorEstado jogadorID (Morto _) jogada estadoDoJogo = jogadaMorto jogadorID jogada estadoDoJogo
jogadaJogadorEstado jogadorID (Ar _ _ _) jogada estadoDoJogo = jogadaNoAr jogadorID jogada estadoDoJogo

-- | Função que , se o 'Jogador' estiver no 'Chao' , altera o 'Estado' conforme a 'Jogada' 
jogadaNoChao :: Int -> Jogada -> Estado -> Estado
jogadaNoChao jogadorID (Movimenta E) estadoDoJogo = estadoDoJogo
jogadaNoChao jogadorID (Movimenta D) estadoDoJogo = estadoDoJogo
jogadaNoChao jogadorID (Movimenta C) e@(Estado mapa jogadores) | limiteSuperior jogador && comparaAlturas (pecaAdjacenteSuperior jogador mapa) jogador = (Estado mapa (insereJogador jogadorID (alteraPista jogador (Movimenta C)) jogadores))
                                                               | limiteSuperior jogador && not(comparaAlturas (pecaAdjacenteSuperior jogador mapa) jogador) && cai (pecaAdjacenteSuperior jogador mapa) jogador = (Estado mapa (insereJogador jogadorID (alteraPistaAr jogador (Movimenta C) mapa) jogadores))
                                                               | limiteSuperior jogador && not(comparaAlturas (pecaAdjacenteSuperior jogador mapa) jogador) && not (cai (pecaAdjacenteSuperior jogador mapa) jogador) = (Estado mapa (insereJogador jogadorID (jogadorMorre jogador) jogadores))
                                                               | otherwise = e
    where
        jogador = encontraIndiceLista jogadorID jogadores
jogadaNoChao jogadorID (Movimenta B) e@(Estado mapa jogadores) | limiteInferior jogador mapa && comparaAlturas (pecaAdjacenteInferior jogador mapa) jogador = (Estado mapa (insereJogador jogadorID (alteraPista jogador (Movimenta B)) jogadores))
                                                               | limiteInferior jogador mapa && not(comparaAlturas (pecaAdjacenteInferior jogador mapa) jogador) && cai (pecaAdjacenteInferior jogador mapa) jogador = (Estado mapa (insereJogador jogadorID (alteraPistaAr jogador (Movimenta B) mapa) jogadores))
                                                               | limiteInferior jogador mapa && not(comparaAlturas (pecaAdjacenteInferior jogador mapa) jogador) && not (cai (pecaAdjacenteInferior jogador mapa) jogador) = (Estado mapa (insereJogador jogadorID (jogadorMorre jogador) jogadores))
                                                               | otherwise = e
    where
        jogador = encontraIndiceLista jogadorID jogadores
jogadaNoChao jogadorID (Acelera) e@(Estado mapa jogadores) = (Estado mapa (insereJogador jogadorID (aceleraJogadorTrue(encontraIndiceLista jogadorID jogadores)) jogadores))
jogadaNoChao jogadorID (Desacelera) e@(Estado mapa jogadores) = (Estado mapa (insereJogador jogadorID (desaceleraJogador(encontraIndiceLista jogadorID jogadores)) jogadores))
jogadaNoChao jogadorID (Dispara) e@(Estado mapa jogadores) | podeDisparar (encontraIndiceLista jogadorID jogadores) = (Estado (alteraPisoAnterior posM peca mapa) (insereJogador jogadorID (perdeBala (encontraIndiceLista jogadorID jogadores)) jogadores))
                                                           | otherwise = e
    where
        posM = posicaoPeca (encontraIndiceLista jogadorID jogadores)
        peca = devolvePeca mapa posM

-- | Função que determina o efeito de uma 'Jogada' quando o jogador está 'Morto'
jogadaMorto :: Int -> Jogada -> Estado -> Estado
jogadaMorto _ _ estadoDoJogo = estadoDoJogo

-- | Função que determina o efeito de uma 'Jogada' quando o jogador está no 'Ar'
jogadaNoAr :: Int -> Jogada -> Estado -> Estado
jogadaNoAr jogadorID (Movimenta E) e@(Estado mapa jogadores) = (Estado mapa (insereJogador jogadorID (movimentaE(encontraIndiceLista jogadorID jogadores)) jogadores))
jogadaNoAr jogadorID (Movimenta D) e@(Estado mapa jogadores) = (Estado mapa (insereJogador jogadorID (movimentaD(encontraIndiceLista jogadorID jogadores)) jogadores)) 
jogadaNoAr _ _ estadoDoJogo = estadoDoJogo


-------------------- M O V I M E N T A  C I M A  O U  B A I X O -------------------------------------

-- * Funções principais das instruções 'Movimenta' __cima__ ou __baixo__
-- | Função que verifica se o jogador pode efetuar a 'Jogada' tendo em conta o limite superior
limiteSuperior :: Jogador -> Bool                                                        
limiteSuperior (Jogador pista distancia velocidade cola estado) | pista >= 1 = True
                                                                | otherwise = False

-- | Função que verifica se o jogador pode efetuar a 'Jogada' tendo em conta o limite inferior
limiteInferior :: Jogador -> Mapa -> Bool                                                
limiteInferior (Jogador pista distancia velocidade cola estado) m | pista < (fst(dimensaoMatriz m)) - 1 = True
                                                                  | otherwise = False

-- | Função que determina a 'Peca' que está na mesma posição do 'Jogador' mas na pista por cima dele
pecaAdjacenteSuperior :: Jogador -> Mapa -> (Peca,Peca)
pecaAdjacenteSuperior (Jogador pista distancia v c e) m = (encontraPosicaoMatriz (pista-1,truncate distancia) m,encontraPosicaoMatriz (pista,truncate distancia) m)

-- | Função que determina a 'Peca' que está na mesma posição do 'Jogador' mas na pista por baixo dele
pecaAdjacenteInferior :: Jogador -> Mapa -> (Peca,Peca) 
pecaAdjacenteInferior (Jogador pista distancia v c e) m = (encontraPosicaoMatriz (pista+1,truncate distancia) m,encontraPosicaoMatriz (pista,truncate distancia) m)

{- | Função que dada uma 'Peca' e uma certaa distância percorrida, calcula a altura exata em que se encontra

De notar que os cálculos só são "efetuados" caso a 'Peca' seja uma __Rampa__, pois se se encontrar numa reta , então a sua altura é dada pela própria 'Peca'
-}
alturaJogadorF :: Peca -> Double -> Double
alturaJogadorF (Rampa p x y) dist | x < y = realToFrac x + distP * realToFrac (y-x) 
                                  | otherwise = realToFrac x - distP * realToFrac (x-y)
    where
        distP = dist - realToFrac(truncate dist)
alturaJogadorF (Recta p a) dist = realToFrac a


-- | Função que dada a 'Peca' atual e a 'Peca' adjacente ao 'Jogador' , verifica se a altura é ou não maior a /0,2/.
comparaAlturas :: (Peca,Peca) -> Jogador -> Bool
comparaAlturas (pAdjacente,pAtual) (Jogador p dist v c e) | abs((alturaJogadorF pAdjacente dist) - (alturaJogadorF pAtual dist)) <= 0.2 = True
                                                          | otherwise = False


-- | Função que de acordo com a jogada 'Movimenta' __cima__ ou __baixo__ altera a pista em que o 'Jogador' se encontra.
alteraPista :: Jogador -> Jogada -> Jogador
alteraPista (Jogador pista d v c e) (Movimenta C) = (Jogador (pista-1) d v c e)
alteraPista (Jogador pista d v c e) (Movimenta B) = (Jogador (pista+1) d v c e)

-- | Função que altera o estado do 'Jogador' quando este , ao tentar mudar de pista, esbarra
alteraPistaAr :: Jogador -> Jogada -> Mapa -> Jogador
alteraPistaAr (Jogador pista d v c e) (Movimenta C) m = (Jogador (pista-1) d v c (Ar alturaAnterior inclinacaoAnterior 0))
    where
        alturaAnterior = alturaJogadorF(encontraPosicaoMatriz(pista,truncate d) m) d
        inclinacaoAnterior = calcInclinacao (encontraPosicaoMatriz(pista,truncate d) m)
alteraPistaAr (Jogador pista d v c e) (Movimenta B) m = (Jogador (pista+1) d v c (Ar alturaAnterior inclinacaoAnterior 0))
    where
        alturaAnterior = alturaJogadorF (encontraPosicaoMatriz(pista,truncate d) m) d
        inclinacaoAnterior = calcInclinacao (encontraPosicaoMatriz(pista,truncate d) m)

-- | Função auxiliar que calcula a inclinação de uma 'Peca'
calcInclinacao :: Peca -> Double
calcInclinacao (Recta _ _) = 0
calcInclinacao (Rampa _ ai af) | ai < af = (atan (realToFrac (af-ai))) * 180 / pi
                               | otherwise = (atan (realToFrac (af-ai))) * 180 / pi

-- | Função que verifica se o jogador esbarra contra a outra 'Peca' e , consequentemente, cai.
cai :: (Peca,Peca) -> Jogador -> Bool
cai (pAdjacente,pAtual) (Jogador p dist v c e) | alturaJogadorF pAdjacente dist < alturaJogadorF pAtual dist = True
                                               | otherwise = False

-- | Função que altera o estado do 'Jogador' para 'Morto' caso ele esbarre contra a 'Peca'
jogadorMorre :: Jogador -> Jogador
jogadorMorre (Jogador p d v c e) = (Jogador p d 0 c (Morto 1))


-----------------------------------------------------------------------------------------------------
    
-------------------- M O V I M E N T A  I N C L I N A Ç Ã O -----------------------------------------

-- * Funções auxiliares para realizar a jogada 'Movimenta' __E__ e __D__

-- | Função que altera o estado do 'Jogador' de acordo com a jogada 'Movimenta' __E__
movimentaE :: Jogador -> Jogador
movimentaE (Jogador pista distancia velocidade cola estado) = (Jogador pista distancia velocidade cola (alteraInclinacaoE estado))

-- | Função que verifica se o jogador pode efetuar a 'Jogada'
alteraInclinacaoE :: EstadoJogador -> EstadoJogador
alteraInclinacaoE (Ar altura inclinacao gravidade) | inclinacao + 15 < 90 = (Ar altura (inclinacao+15) gravidade)
                                                   | otherwise = (Ar altura 90 gravidade)

-- | Função que altera o estado do 'Jogador' de acordo com a jogada 'Movimenta' __D__
movimentaD :: Jogador -> Jogador
movimentaD (Jogador pista distancia velocidade cola estado) = (Jogador pista distancia velocidade cola (alteraInclinacaoD estado))

-- | Função que verifica se o jogador pode efetuar a 'Jogada'
alteraInclinacaoD :: EstadoJogador -> EstadoJogador
alteraInclinacaoD (Ar altura inclinacao gravidade) | inclinacao - 15 > (-90) = (Ar altura (inclinacao-15) gravidade)
                                                   | otherwise = (Ar altura (-90) gravidade)

-----------------------------------------------------------------------------------------------------

-------------------- A C E L E R A  E  D E S A C E L E R A ------------------------------------------

-- * Funções principais das instruções 'Acelera' e 'Desacelera'

-- | Função que altera o estado do 'Jogador' para a instrução 'Acelera'
aceleraJogadorTrue :: Jogador -> Jogador
aceleraJogadorTrue (Jogador p d v c estado) = (Jogador p d v c (aceleraT2 estado))

-- | Função que altera o estado do 'Jogador' para a instrução 'Desacelera'
desaceleraJogador :: Jogador -> Jogador
desaceleraJogador (Jogador p d v c estado) = (Jogador p d v c (desacelera estado))

-- *** Funções auxiliares das instruções 'Acelera' e 'Desacelera'

-- | Função que dado um 'EstadoJogador' determina se está a desacelerar
desacelera :: EstadoJogador -> EstadoJogador
desacelera estado = (Chao False)

-- | Função que dado um 'EstadoJogador' determina se está a acelerar
aceleraT2 :: EstadoJogador -> EstadoJogador
aceleraT2 estado = (Chao True)

------------------------------------------------------------------------------------------------------

--------------------- D I S P A R A ------------------------------------------------------------------

-- * Funções principais à realização do __Disparo__

-- | Função que retira uma 'Cola' depois de o jogador ter disparado
perdeBala :: Jogador -> Jogador
perdeBala (Jogador pista distancia velocidade cola estado) = (Jogador pista distancia velocidade (cola-1) estado)

-- | Função que após o __disparo__ altera o piso da 'Peca' anterior.
alteraPisoAnterior :: PosicaoMatriz -> Peca -> Mapa -> Mapa
alteraPisoAnterior pos (Recta p a) m = atualizaPosicaoMatriz pos (Recta Cola a) m 
alteraPisoAnterior pos (Rampa p a1 a2) m = atualizaPosicaoMatriz pos (Rampa Cola a1 a2) m 

-- | Função que determina se o 'Jogador' pode efetuar o __disparo__
podeDisparar :: Jogador -> Bool
podeDisparar (Jogador pista distancia velocidade cola estado) | distancia >= 1 && cola > 0 && isChao estado = True
                                                              | otherwise = False

-- *** Funções auxiliares à realização do __Disparo__
-- | Função que recebe um 'Mapa' e encontra uma determinada 'Peca' dada uma __Posição na Matriz__.

devolvePeca :: Mapa -> PosicaoMatriz -> Peca
devolvePeca m p = encontraPosicaoMatriz p m

-- | Função que verifica se o 'Jogador' se encontra no 'Chao' para realizar a jogada. 
isChao :: EstadoJogador -> Bool
isChao (Chao _) = True
isChao (Morto _) = False
isChao (Ar _ _ _) = False

{-| Função que determina a 'Peca' em que o jogador se encontra.

== Exemplo : 

Se o jogador estiver na distância 2,6, então encontra-se na 'Peca' 2
-}
posicaoPeca :: Jogador -> PosicaoMatriz
posicaoPeca (Jogador pista distancia velocidade cola estado) = (pista,truncate(distancia-1))

------------------------------------------------------------------------------------------------------
-- * Funções auxiliares à Tarefa 2

{- |
Esta função é importante para ,depois de ter o 'Jogador' com o seu 'Estado' alterado , inseri-lo na lista de jogadores.


-}
insereJogador :: Int -> Jogador -> [Jogador] -> [Jogador]
insereJogador jogadorID jogador jogadores = if jogadorID < length jogadores && jogadorID >= 0
                                            then let (l1,h:t) = splitAt jogadorID jogadores
                                                 in l1 ++ [jogador] ++ t
                                            else jogadores


-- | Função que devolve o 'EstadoJogador'
devolveEstadoJogador :: Jogador -> EstadoJogador
devolveEstadoJogador (Jogador p d v c estado) = estado
