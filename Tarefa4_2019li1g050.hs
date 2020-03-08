-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g050 where

import LI11920
import Tarefa0_2019li1g050
import Tarefa1_2019li1g050
import Tarefa2_2019li1g050

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(2,gera 2 5 1,(Jogador 1 2.3 1 5 (Chao True))),(2,gera 2 5 1,(Jogador 1 2.3 1 5 (Chao False))),
            (2,gera 2 5 1,(Jogador 1 2.3 0 5 (Morto 1))),(2,gera 2 5 1,(Jogador 1 2 0.75 5 (Ar 2 15 4))),(2,gera 2 5 1,(Jogador 1 4.7 0.75 5 (Ar 3 15.0 0))),
            (5, gera 5 5 400 ,(Jogador 4 4.5 10 4 (Ar 4 (-30) 10))),(4 , gera 5 5 400 , (Jogador 4 3.9 5 3 (Ar 2 0 5))),
            (5, gera 6 6 112, (Jogador 0 4.5 20 4 (Ar 6 (-45) 6 ))),(10, gera 4 4 1234 , (Jogador 3 2.7 20 7 (Ar 7 (-67) 10))),
            (0.1,gera 2 5 1,(Jogador 1 4.3 2 5 (Chao True))),(10,gera 4 4 1234,(Jogador 3 2.7 0.0 7 (Ar 7 (-67) 20))),
            (2,gera 2 5 1,(Jogador 1 3.4 2 5 (Ar 3 30 8))),(2,gera 2 5 1,(Jogador 1 1.4 10 5 (Ar 1 30 8))),
            (5, gera 2 5 1 , (Jogador 1 3.9 5 5 (Chao True))),
            (2,gera 2 5 1,(Jogador 1 3.9 2 5 (Chao True)))]

                

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera tempo mapa j@(Jogador p d v c e) = atualizaVelocidade j mapa tempo


-- | Atualiza a velocidade do jogador de acordo com as fórmulas físicas disponibilizadas
atualizaVelocidade :: Jogador -> Mapa -> Double -> Jogador
atualizaVelocidade (Jogador p d v c e@(Chao _)) mapa t | novaVel >= 0 = (Jogador p d novaVel c e)
                                                       | otherwise = (Jogador p d 0 c e)
    where
        novaVel = (v + ((accelMota e v) - (atrito * v)) * t)
        atrito = atribuiAtrito(encontraPosicaoMatriz (p,truncate d) mapa)
atualizaVelocidade (Jogador p d v c e@(Ar alt i grav)) mapa t | novaVel >= 0 = (Jogador p d novaVel c (Ar alt i novaGrav))
                                                              | otherwise = (Jogador p d 0 c (Ar alt i novaGrav))
    where
        novaVel = v - (0.125 * v * t)
        novaGrav = grav + t
atualizaVelocidade j@(Jogador _ _ _ _ (Morto _)) mapa t = j

-- | Função que calcula a aceleração da mota de acordo com as fórmulas disponibilizadas
accelMota :: EstadoJogador -> Double -> Double
accelMota (Chao True) velocidade | velocidade < 2 = 1
                                 | otherwise = 0
accelMota (Chao False) velocidade = 0

-- * Funçõs auxilares à função __acelera__

-- | Função que determina o piso de uma 'Peca' em que o jogador se encontra.
pisoPeca :: Peca -> Piso
pisoPeca (Recta piso x) = piso
pisoPeca (Rampa piso x y) = piso

-- | Função que dado um piso de uma 'Peca' atribui o valor do atrito de acordo com os números disponibilizados pelos docentes

atribuiAtrito :: Peca -> Double
atribuiAtrito peca = case pisoPeca peca of
    Terra -> 0.25
    Relva -> 0.75
    Lama -> 1.50
    Boost -> -0.50
    Cola -> 3.00

-- * Funções principais da função __move__

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t mapa j = verificaEstado t mapa j


{- | Função que verifica o 'EstadoJogador' para determinar qual a movimenntação a ser realizada

== Exemplo : 

Se o jogador estiver 'Morto' então irá ser chamada a função __moveMorto__
-}
verificaEstado :: Double  -- ^ O tempo decorrido
                -> Mapa  -- ^ O mapa utilizado  
                -> Jogador -- ^ O estado anterior do 'Jogador'
                -> Jogador -- ^ O estado do 'Jogador' após se movimentar
verificaEstado t mapa j@(Jogador p d v c e) = case e of
    (Morto x) -> moveMorto t j
    (Chao x) -> moveChao t mapa j
    (Ar a i g) -> moveAr t mapa j


{- | Função que realiza a movimentação do 'Jogador' quando este se encontra 'Morto'

De notar que esta função decrementa o timeout do jogador se a diferença entre o timeout e o tempo decorrido for maior que 0.
-}
moveMorto :: Double -> Jogador -> Jogador
moveMorto t (Jogador p d v c (Morto timeout)) | (timeout - t) > 0 =(Jogador p d v c (Morto(timeout-t)))
                                              | otherwise = (Jogador p d 0 c (Chao False))


{- | Função que realiza a movimentação no chão 

DE notar que esta função verifica se após a movimentação o jogador atravessa o limite da 'Peca' e de acordo com as indicações dadas /coloca/ 'Jogador' no síto certo.

-}

moveChao :: Double -> Mapa -> Jogador -> Jogador
moveChao t mapa j@(Jogador p d v c e) | calcInclinacao pecaJogador == 0 = moveChaoRecta t mapa j
                                      | otherwise = moveChaoRampa t mapa j
    where
        pecaJogador = encontraPosicaoMatriz (p,truncate d) mapa

-- *** Funções auxiliares à realização da movimentação no chão
 
-- | Função que movimenta o jogador se este se encontrar numa 'Rampa'
moveChaoRampa :: Double -> Mapa -> Jogador -> Jogador
moveChaoRampa t mapa j@(Jogador p d v c e) | novaD < realToFrac (truncate d)+1 = (Jogador p novaD v c e)
                                           | otherwise = verificaInclinacaoPeca pecaJogador (encontraPosicaoMatriz (p,(truncate d)+1) mapa) j --verificaInclinacaoPeca (pecaJogador) (encontraPosicaoMatriz (p,(truncate d)+1) mapa) j
    where
        (Cartesiano novaD y) = somaVetores (Cartesiano d (alturaJogadorF (pecaJogador) d)) (Polar (v*t) (calcInclinacao pecaJogador))
        pecaJogador = encontraPosicaoMatriz (p,truncate d) mapa


-- | Função que movimenta o jogador se este se encontrar numa 'Reta'
moveChaoRecta :: Double -> Mapa -> Jogador -> Jogador
moveChaoRecta t mapa j@(Jogador p d v c e) | d + (t * v) < realToFrac(truncate d) + 1 = (Jogador p (d+(t*v)) v c e)
                                           | otherwise = verificaInclinacaoPeca pecaJogador pecaSeguinte j 
    where 
        pecaJogador = encontraPosicaoMatriz (p,truncate d) mapa
        pecaSeguinte = encontraPosicaoMatriz (p,(truncate d) + 1) mapa


{- | Função que realiza a verificação da inclinação do jogador e da 'Peca' seguinte

Esta função recorre a uma função utilizada na __Tarefa 2__ para o cálculo da inclinação de uma 'Peca'

-}

verificaInclinacaoPeca :: Peca -> Peca -> Jogador -> Jogador
verificaInclinacaoPeca pAtual pSeguinte (Jogador p d v c e@(Chao x)) | calcInclinacao pAtual <= calcInclinacao pSeguinte = (Jogador p (realToFrac(truncate d)+1) v c e)
                                                                     | otherwise = (Jogador p (realToFrac(truncate d)+1) v c (Ar altura (calcInclinacao pAtual) 0))
    where
        altura = alturaJogadorF pSeguinte 0

{- | Função que realiza a movimentação do jogador no ar



Nesta função ha a contemplação de 3 casos : o caso em que o 'Jogador' interseta o chão, o caso em que o 'Jogador' interseta o limite da 'Peca' , e ,por fim, o caso em 
que a movimentação não interseta nada e mantém-se no Ar

== Exemplo : 

Se o 'Jogador' intersetar o limite da Peca então irá ficar no início da próxima 'Peca' no ponto de interseção 
-}
moveAr :: Double -> Mapa -> Jogador -> Jogador
moveAr t mapa j@(Jogador p d v c (Ar alt inc g)) | intersetam retaJogador retaPeca && d /= realToFrac(truncate d) = comparaInc t retaJogador j (encontraPosicaoMatriz (p,(truncate d)) mapa)
                                                 | intersetam retaJogador retaVertical = (Jogador p (realToFrac(truncate d)+1) v c (Ar novaAlt inc g))
                                                 | otherwise = (Jogador p x v c (Ar y inc g))
    where 
        vetVel = somaVetores (Polar (v*t) inc) (Polar (g*t) (-90)) 
        posJogador = (Cartesiano d alt)
        retaJogador = (posJogador,somaVetores posJogador vetVel)
        retaPeca = criaRetaPeca (encontraPosicaoMatriz (p,(truncate d)) mapa) d
        (Cartesiano x y) = somaVetores posJogador vetVel 
        retaVertical = ((Cartesiano (realToFrac(truncate d)+1) 0),(Cartesiano (realToFrac(truncate d)+1) (100)))
        (Cartesiano novaD novaAlt) = intersecao retaVertical retaJogador


-- | Função que cria uma reta de acordo com a 'Peca' em questão , ou seja, uma Reta ou uma Rampa

criaRetaPeca :: Peca -> Double -> Reta
criaRetaPeca (Recta x a) d = ((Cartesiano (realToFrac(truncate d)) (realToFrac a)),(Cartesiano (realToFrac(truncate d)+1) (realToFrac a)))
criaRetaPeca (Rampa x a1 a2) d = ((Cartesiano (realToFrac(truncate d)) (realToFrac a1)),(Cartesiano (realToFrac(truncate d)+1) (realToFrac a2))) 


-- | Função que compara as inclinações do jogador e da 'Peca'

comparaInc :: Double -> Reta -> Jogador -> Peca -> Jogador
comparaInc t rJogador j@(Jogador p d v c (Ar alt inc g)) peca@(Recta x a) | abs(inc) >= 45 = (Jogador p novDist 0 c (Morto 1))
                                                                          | otherwise = (Jogador p novDist v c (Chao False))
    where
        rPeca = ((Cartesiano (realToFrac(truncate d)) (realToFrac a)),(Cartesiano (realToFrac(truncate d)+1) (realToFrac a)))
        (Cartesiano novDist y) = intersecao rJogador rPeca
comparaInc t rJogador j@(Jogador p d v c (Ar alt inc g)) peca@(Rampa x a1 a2) | abs(inc - calcInclinacao peca) >= 45 = (Jogador p novDist 0 c (Morto 1))
                                                                              | otherwise = (Jogador p novDist novaV c (Chao False))
    where

        rPeca = ((Cartesiano (realToFrac(truncate d)) (realToFrac a1)),(Cartesiano (realToFrac(truncate d)+1) (realToFrac a2)))
        (Cartesiano novDist y) = intersecao rJogador rPeca
        novaV = v * ((cos((abs(((calcInclinacao peca) - inc))) * pi / 180)))

