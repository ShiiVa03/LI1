-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2019li1g050 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(5,10,1),(1,1,2),(10,10,12366),(5,5,6),(5,5,0),(1,10,13)]

-- * Funções pré-definidas da Tarefa 1.
-- | Função que gera números aleatórios necessários para a construção do 'Mapa'.
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.
-- | Função que dado um número de pistas , um comprimento e uma semente , cria um 'Mapa'.

gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = adicionaPrimeiraPeca(geraMapa comprimento npistas (geraPares(separaPistas comprimento (geraAleatorios elementos semente))))
    where elementos = ((npistas * comprimento) * 2) - (npistas * 2) 


{- | Separa uma lista equitativamente pelo comprimento.

Esta função é necessária para determinar corretamento o número de peças a serem criadas.

-}
separaPistas :: Int -> [Int] -> [[Int]]
separaPistas comprimento [] = []
separaPistas comprimento l@( h : t ) = l1 : separaPistas comprimento l2 
    where (l1,l2) = splitAt elemetos l 
          elemetos = (comprimento - 1) * 2

-- | Função que dada uma matriz dá uma matriz de pares ordenados. 
geraPares :: [[Int]] -> [[(Int,Int)]]
geraPares [] = []
geraPares m = map geraPares' m

{- | Função que gera um 'Mapa' dado um comprimento da 'Pista' , o número de 'Pista's e uma matriz de pares ordenados.

De notar que esta função é muito parecida à função __Gera__, porém , a função __GeraMapa__ é uma função genérica , ou seja, dados uns valores ela converte num 'Mapa', ao contrário da função __Gera__ que depende dos números aleatórios.


-}
geraMapa :: Int -> Int -> [[(Int,Int)]] -> Mapa
geraMapa 1 npistas l = geraVazia npistas
geraMapa comprimento npistas l = map (geraPista (Recta Terra 0)) l

-- | Função que cria uma 'Pista' , dada uma 'Peca' anterior e uma lista de pares ordenados.

geraPista :: Peca -> [(Int,Int)] -> [Peca]
geraPista peca [] = []
geraPista peca (h:t) = (geraPeca peca h) : geraPista (geraPeca peca h) t

-- | Função que cria uma 'Peca' , dada uma 'Peca' anterior e um par ordenado.

geraPeca :: Peca -> (Int,Int) -> Peca
geraPeca (Recta piso altura) (x,y) = geraTipo altura (geraPiso piso x) y
geraPeca (Rampa piso alturaI alturaF) (x,y) = geraTipo alturaF (geraPiso piso x) y


-- | Função que determina o 'Piso'.
geraPiso :: Piso -> Int -> Piso
geraPiso pisoAnterior gama | 0 <= gama && gama <= 1 = Terra
                           | 2 <= gama && gama <= 3 = Relva
                           | gama == 4 = Lama
                           | gama == 5 = Boost
                           | 6 <= gama && gama <= 9 = pisoAnterior

-- |Função que contrói uma 'Peca'

geraTipo :: Int -- ^ Altura anterior 
          -> Piso -- ^ Piso Atual
          -> Int -- ^ Gama
          -> Peca -- ^ Peça Resultante de acordo com os argumentos anteriores
geraTipo alturaAnterior pisoAtual gama | 0 <= gama && gama <= 1 = constroiPecaSobe alturaAnterior (gama+1) pisoAtual
                                       | 2 <= gama && gama <= 5 = constroiPecaDesce alturaAnterior (gama-1) pisoAtual
                                       | 6 <= gama && gama <= 9 = (Recta pisoAtual alturaAnterior)

-- * Funções auxiliares da Tarefa 1

-- | Função que dada uma lista de inteiros cria uma lista de pares odenados.
geraPares' :: [Int] -> [(Int,Int)]
geraPares' [] = []
geraPares' (h : h2 : t) = (h,h2) : geraPares' t 

-- | Função auxiliar que cria uma 'Rampa' de declive __positivo__.

constroiPecaSobe :: Int -> Int -> Piso -> Peca
constroiPecaSobe alturaAnterior diferenca piso = (Rampa piso alturaAnterior (alturaAnterior+diferenca))

-- | Função auxiliar que cria uma 'Rampa' de declive __negativo__.
constroiPecaDesce :: Int -> Int -> Piso -> Peca
constroiPecaDesce alturaAnterior diferenca piso | alturaAnterior - diferenca <= 0 = rectaOuRampa alturaAnterior diferenca piso
                                                | otherwise = (Rampa piso alturaAnterior (alturaAnterior-diferenca))

-- |Função que auxilia na criação de uma 'Rampa' ou 'Recta'.
rectaOuRampa :: Int -> Int -> Piso -> Peca
rectaOuRampa alturaAnterior diferenca piso | alturaAnterior == 0 = (Recta piso 0)
                                           | otherwise = (Rampa piso alturaAnterior 0)

-- | Função que garante que a primeira 'Peca' do 'Mapa' é sempre /Recta Terra 0/.
adicionaPrimeiraPeca :: Mapa -> Mapa
adicionaPrimeiraPeca mapa = map ((Recta Terra 0):) mapa

-- | Função que cria um 'Mapa' com só __1__ de /comprimento/.
geraVazia :: Int -> [[a]]
geraVazia pistas | pistas /= 0 = [] : geraVazia (pistas-1)
                 | otherwise = []