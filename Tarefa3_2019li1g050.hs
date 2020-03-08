{-|
--  Introdução à Tarefa 3
-- 	 	Objetivo da Tarefa

	Nesta Tarefa foi nos proposto que dado um mapa o pudessemos codificar numa sequência de instruções que, quando executadas produzissem o mesmo mapa.
Estas instruções seriam dadas a um grupo de bulldozers (um por pista ) que avançavam a partir da primeira 'Peca' para construir o mapa em questão.
O objetivo mais simples da Tarefa era descobrir que tipo de padrões existiam num mapa : Verticais, horizontais e verticais desfasados.

Numa primeira fase começamos por implementar os padrões mais fáceis , ou seja, os horizontais que consistiam em usar o construtor 'Repete' sempre que na mesma
pista se encontravam 'Peca's iguais.



-- Desenvolvimento da Tarefa 3
-- 	Começo da aplicação dos padrões verticais


	Após a aplicação dos padrões horizontais reparamos que a taxa de compressão do mapa era pouca, ou seja, ainda tinhamos trabalho a fazer.
Por conseguinte começamos a codificar os padrões verticais. 
Um dos grandes desafios destes padrões verticais era que tínhamos que "operar" em duas partes do mapa ao mesmo tempo.
A solução adotada por nós foi primeiramente codificar cada 'Peca' para 'Instrucoes' e por recursividade obtinhamos uma pista e, por último, um mapa.
A grande diferença era que obtinhamos um 'Mapa' através de uma matriz de 'Instrucoes' em vez de uma lista de instruções.
Foi através desta matriz de instruções que conseguimos então implementar os padrões verticais , pois ao realizarmos a transposta desta podiamos operar numa só "linha"
e não em vários pontos desfasados.

	--  Finalização dos padrões verticais

Após termos transposto a matriz de instruções era completamente trivial a resolução do problema. Começamos por comparar instruções iguais e, 
depois, agrupamos numa lista. Depois nesta lista só tinhamos de retirar os elementos a mais que estavam nela, ou seja, aquele que tinhasm sido agrupados continuavam lá 
na forma não otimizada.




--  Conclusão 
-- 		Conclusão e principais problemas encontrados

 Depois da aplicação dos padrões verticais , reparamos que a taxa de compressão tinha aumentado bastante , mas ainda podiam ser feitas pequenas otimizações como 
 a aplicação de padrões horizontais a estes padrões verticais, mas como o prazo de entrega estava próximo decidimos que não iríamos acabar a tempo.

 Os grandes problemas encontrados nesta Tarefa foram perceber como iríamos aplicar os padrões verticais ao mapa dado que tínhamos de operar em duas pistas ao mesmo tempo, 
 mas após termos descoberto que podiamos transpor a lista de instruções tudo se tornou evidentemente mais fácil.

 -}
module Tarefa3_2019li1g050 where

import Data.List
import Tarefa0_2019li1g050
import Tarefa1_2019li1g050
import LI11920

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [gera 5 5 2, gera 6 6 9, gera 8 8 23]



-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi:: Mapa -> Instrucoes
desconstroi m = concat (desconstroi2 m)


-- * Funções principais da Tarefa 3

{- | Função que recebe dois pares de inteiros, sendo um a posição onde nos encontramos na matriz , e , outro que é a dimensão da matriz.

Esta função é importante pois é a que nos permite transformar um 'Mapa' em instruções
-}
desconstroiMapa :: DimensaoMatriz -> DimensaoMatriz -> Mapa -> [Instrucoes]
desconstroiMapa (ca,cb) (a,b) [] = []
desconstroiMapa (ca,cb) (a,b) ((peca : lPecas) : pistas) = pistaParaInstrucoes ca lPecas : desconstroiMapa ((ca+1),cb) (a,b) pistas

-- | Função que transforma uma pista em instruções
pistaParaInstrucoes :: Int -> Pista -> Instrucoes
pistaParaInstrucoes pista [] = []
pistaParaInstrucoes pista (h:t) = pecaParaInstrucao pista h : pistaParaInstrucoes pista t

-- | Função que transforma cada 'Peca' numa 'Instrucao'
pecaParaInstrucao :: Int-> Peca -> Instrucao
pecaParaInstrucao pista (Recta piso a) = (Anda [pista] piso)
pecaParaInstrucao pista (Rampa piso a b) | a < b = (Sobe [pista] piso (b-a))
                                         | otherwise = (Desce [pista] piso (a-b))

-- | Função que transpõe uma matriz de instruções para ser mais fácil detetar padrões verticais
transpoeInstrucoes :: [Instrucoes] -> [Instrucoes]
transpoeInstrucoes [] = []
transpoeInstrucoes m = transpose m 

-- | Função que descontroi um mapa numa lista de 'Instrucoes' para depois ser transposta
desconstroi1 :: Mapa -> [Instrucoes]
desconstroi1 m = (desconstroiMapa (0,1) (dimensaoMatriz m)  m)

-- | Função que compara instruções consecutivas para verificar se são iguais
comparaInstrucoes2 :: Instrucao -> Instrucao -> Bool
comparaInstrucoes2 (Anda pista piso1) (Anda pista2 piso2) = piso1==piso2
comparaInstrucoes2 (Sobe pista piso1 altura) (Sobe pista2 piso2 altura2) = piso1==piso2 && altura == altura2
comparaInstrucoes2 (Desce pista piso1 altura) (Desce pista2 piso2 altura2) = piso1==piso2 && altura==altura2
comparaInstrucoes2 _ _ = False

-- | Função que agrupa instrucões iguais e consecutivas
agrupaInstrucoesiguais :: Instrucoes-> Instrucoes
agrupaInstrucoesiguais [i1] = [i1]
agrupaInstrucoesiguais (i1:i2:insts) | (comparaInstrucoes2 i1 i2)  =  agrupaInstrucoesiguais ((getindice i1 i2):insts)
                                     | otherwise = agrupaInstrucoesiguais(i1:insts)


-- | Função que verifica se numa lista de 'Instrucoes' há funções iguais para então retirar as retirar                                    
deitafora::Instrucoes -> Instrucoes
deitafora [i1] = []
deitafora (i1:i2:insts) | (comparaInstrucoes2 i1 i2 ) = deitafora(i1 : insts) 
                        | otherwise = [i2]++deitafora(i1:insts)



-- | Função que vai buscar o indice de uma instrução para então realizar a otimização vertical
getindice :: Instrucao -> Instrucao->Instrucao
getindice (Anda x piso) (Anda [y] piso2) = (Anda (x++[y]) piso)
getindice (Sobe x piso h) (Sobe [y] piso2 h2) = (Sobe (x++[y]) piso h)
getindice (Desce x piso h ) (Desce [y] piso2 h2) = (Desce (x++[y]) piso h)


-- | Função que aplica a otimização dos padrões verticais a uma lista de instruções
padroesVerticais :: Instrucoes->Instrucoes
padroesVerticais [] = []
padroesVerticais insts = agrupaInstrucoesiguais insts ++ padroesVerticais(deitafora insts)

-- | Função que realizaçao a otimização à matriz de instruções
padroesVerticais2 :: [Instrucoes] ->[Instrucoes]
padroesVerticais2 [] = []
padroesVerticais2 (lista:listas) = padroesVerticais lista :padroesVerticais2 listas

-- | Função que descontroi o mapa numa lista de instrucoes , sendo esta função a que aplica a otimização em relação aos padrões verticais
desconstroi2 :: Mapa-> [Instrucoes]
desconstroi2 [] = [] 
desconstroi2 m =  padroesVerticais2(transpoeInstrucoes(desconstroi1 m ))

