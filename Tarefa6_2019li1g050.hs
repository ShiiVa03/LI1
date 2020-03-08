{-|
    -- Introdução à Tarefa 6
     -- Objetivos da Tarefa 6

    Nesta Tarefa foi-nos proposto que criássemos um bot que jogasse autonomamente o jogo criado por nós.
Com base num 'Int' associado ao 'Jogador ' e um estado do jogo , o bot deveria produzir uma 'Jogada' do tipo Maybe que inclui-a a opção de ele não a realizar.




    -- Desenvolvimento da Tarefa
    -- Criação do bot


    Numa fase inicial começamos por criar um Bot que simplesmente obtinha as peças superiores e inferiores e verificava se alguma delas fosse do tipo 'Boost' e se podia mudar de pista devido à diferença
    de alturas e então , em caso afirmativo, ele posicionava-se na 'Peca' onde o 'Boost' estava.
    Numa fase intermédia começamos não só a procurar por 'Boost' mas sim por 'Terra' pois caso não houvessem boosts , então ele optaria por 'Terra'.
Depois do bot ter sido posto à prova com os bots do professores reparamos que era  mais inteligente também procurarmos pela 'Peca' frontal , pois se o Bot estivesse perante uma 'Peca' de 'Cola' ou 'Lama' ,
então ele iria mudar de pista para um atrito mais efeciente.

Depois de analisadas as opções "terrestres" , então procuramos por melhorar o Bot enquanto este estava no Ar. Rapidamente chegamos à conclusão que a melhor opção era movimentar o bot de acordo com a inclinção da 'Peca'
onde ele estava.



    -- Conclusão da Tarefa
     -- Conclusão e dificuldades observadas



A grande dificuldade desta Tarefa foi perceber como podiamos otimizar o Bot de acordo com as 'Peca's em volta dele. Começamos por tentar obter o melhor caminho possível de todo o 'Mapa',mas 
rapidamente chegamos à conclusão que seria ineficiente pois o Mapa podia ser alterado a qualquer momento pela colocação de 'Cola'. Numa fase mais avançada encontramos o problema (que foi debatido com o profesor) que 
ao executar a mesma ação no terminal não obtinhamos os mesmos resultados nos testes dos professores, não conseguindo ainda resolver o problema, ou seja, ao testarmos o bot no terminal no mesmo mapa obtinhamos a solução devida
, mas quando corrido no programa dos professores ele apenas realizava algumas funções atribuidas de modo correto.





-}
module Tarefa6_2019li1g050 where

import Tarefa0_2019li1g050
import Tarefa4_2019li1g050
import Tarefa2_2019li1g050
import Tarefa1_2019li1g050
import LI11920









-- * Funções principais da Tarefa 6.


-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot x e@(Estado m j ) | verificaAcelera x (encontraJogador x e) = verificaEstado6 x (encontraJogador x e) e
                      | otherwise = Just Acelera
                              


pecaSuperior :: Jogador -> Mapa -> Peca
pecaSuperior (Jogador pista distancia _ _ _) m = (encontraPosicaoMatriz (pista-1,truncate distancia) m)

pecaInferior :: Jogador -> Mapa -> Peca
pecaInferior (Jogador pista distancia _ _ _) m = (encontraPosicaoMatriz (pista+1,truncate distancia) m)

botBoost :: Int -> Estado -> Maybe Jogada
botBoost _ (Estado _ []) = Nothing  
botBoost x e@(Estado m _ ) = decideJogar m  (encontraJogador x e) 

botAr :: Int-> Estado -> Maybe Jogada
botAr _ (Estado _ []) = Nothing
botAr x e@(Estado m _ ) = jogadorNoAr m (encontraJogador x e)

verificaAcelera :: Int -> Jogador ->  Bool 
verificaAcelera x j@(Jogador _ _ _ _ (Morto z)) = True 
verificaAcelera x j@(Jogador _ _ _ _ (Ar a inc g)) = True
verificaAcelera x j@(Jogador _ _ _ _  (Chao y ))| y  = True
                                                | otherwise = False 


verificaEstado6 :: Int -> Jogador -> Estado-> Maybe Jogada
verificaEstado6 x j@(Jogador _ _ _ _ e) est = case e of
    (Morto z ) -> Just Acelera
    (Ar a i g) ->botAr x est
    (Chao True) -> botBoost x est


encontraJogador :: Int->Estado -> Jogador
encontraJogador x (Estado m l@((Jogador p d v c e):jogadores)) = if (length l  - x) == length l && x < length l 
                                                              then (Jogador p d v c e )
                                                              else encontraIndiceLista (x-1) jogadores


decideJogar:: Mapa -> Jogador -> Maybe Jogada  
decideJogar m j@(Jogador p d _ c _ )| pisoJogador == Boost = Just Acelera
                                    | p==0 && (pisoJogador == Cola|| pisoJogador == Lama)  && (verificaAlturasbaixo m j ) = Just (Movimenta B)
                                    | p==0 && (d== realToFrac (col-1))= comparaPecasLimiteSuperior m j
                                    | p==0 = comparaPecasLimiteSuperiorFrente m j
                                    | p== l-1 && (pisoJogador == Cola || pisoJogador == Lama) && ( verificaAlturasCima m j ) = Just(Movimenta C )
                                    | p== l-1 && (d== realToFrac (col-1)) = comparaPecasLimiteInferior m j  
                                    | p == l-1  = comparaPecasLimiteInferiorFrente m j
                                    | (pisoJogador == Cola ||pisoJogador == Lama) && (p==1) = terceiracomparacao1 m j 
                                    |(pisoJogador == Cola || pisoJogador == Lama) && (p==2) = terceiracomparacao2 m j 
                                    | (d>= realToFrac (col-1))= comparaPecas m j 
                                    | otherwise = comparaPecasFrente m j 

    where 
        pisoAtras = pisoPeca  pecaAtras 
        pecaJogador = encontraPosicaoMatriz (p,truncate d ) m
        pisoJogador = pisoPeca pecaJogador
        (l,col)= dimensaoMatriz m 
        pecaFrente = encontraPosicaoMatriz (p,truncate d +1) m 
        pecaAtras = encontraPosicaoMatriz (p,truncate d -1 ) m 
        dist = realToFrac (truncate d )
terceiracomparacao2:: Mapa-> Jogador -> Maybe Jogada
terceiracomparacao2 m j@(Jogador p d _ _ _) | (pisoCima == Terra || pisoCima == Boost|| pisoCima == Relva) && (comparaAlturasMC j (pecaJogador,pecaCima)) = Just (Movimenta C) 
                                           | (pisoBaixo == Terra  || pisoBaixo == Boost|| pisoCima == Relva ) && ((comparaAlturasMB j (pecaJogador,pecaBaixo))) = Just (Movimenta B)
                                           | (pisoCima2 == Terra || pisoCima2 == Boost|| pisoCima2 == Relva) && (comparaAlturasMC j (pecaJogador,pecaCima))= Just(Movimenta C )
                                           | otherwise = Just Acelera
  where
    pisoJogador = pisoPeca (encontraPosicaoMatriz (p,truncate d) m )
    pisoCima = pisoPeca (encontraPosicaoMatriz ((p-1), truncate d ) m )
    pisoBaixo = pisoPeca (encontraPosicaoMatriz ((p+1), truncate d ) m )
    pisoCima2 = pisoPeca (encontraPosicaoMatriz ((p-2), truncate d ) m )
    pecaCima =  (pecaSuperior j m)
    pecaBaixo =  (pecaInferior j m )
    pecaJogador = (encontraPosicaoMatriz (p,truncate d) m)


terceiracomparacao1:: Mapa-> Jogador -> Maybe Jogada
terceiracomparacao1 m j@(Jogador p d _ _ _) | (pisoCima == Terra || pisoCima == Boost|| pisoCima == Relva) && (comparaAlturasMC j (pecaJogador,pecaCima)) = Just (Movimenta C) 
                                           | (pisoBaixo == Terra  || pisoBaixo == Boost|| pisoCima == Relva ) && ((comparaAlturasMB j (pecaJogador,pecaBaixo))) = Just (Movimenta B)
                                           | (pisoBaixo2 == Terra  || pisoBaixo2 == Boost|| pisoBaixo2 == Relva ) && (comparaAlturasMB j (pecaJogador,pecaBaixo))= Just (Movimenta B)
                                           | otherwise = Just Acelera
  where
    pisoJogador = pisoPeca (encontraPosicaoMatriz (p,truncate d) m )
    pisoCima = pisoPeca (encontraPosicaoMatriz ((p-1), truncate d ) m )
    pisoBaixo = pisoPeca (encontraPosicaoMatriz ((p+1), truncate d ) m )
    pisoBaixo2 = pisoPeca (encontraPosicaoMatriz ((p+2), truncate d ) m )
    pecaCima =  (pecaSuperior j m)
    pecaBaixo =  (pecaInferior j m )
    pecaJogador = (encontraPosicaoMatriz (p,truncate d) m)


comparaPecasLimiteSuperior :: Mapa->Jogador -> Maybe Jogada
comparaPecasLimiteSuperior m j@(Jogador p d _ _ _ ) | pisoJogador == Boost = Just Acelera
                                                   | (verificaAlturasbaixo m j) && pisoBaixo == Boost = Just (Movimenta B)
                                                   | pisoJogador == Terra = Just Acelera
                                                   | (verificaAlturasbaixo m j) && pisoBaixo == Terra = Just(Movimenta B)
                                                   | otherwise = Just Acelera
    where 
        pisoJogador = pisoPeca (encontraPosicaoMatriz (p,truncate d) m )
        pisoBaixo = pisoPeca (encontraPosicaoMatriz ((p+1), truncate d ) m ) 
        


         
comparaPecasLimiteSuperiorFrente :: Mapa->Jogador -> Maybe Jogada
comparaPecasLimiteSuperiorFrente m j@(Jogador p d _ _ _ ) | pisoJogador == Boost = Just Acelera
                                                         | (verificaAlturasbaixo m j) && pisoBaixo == Boost = Just (Movimenta B)
                                                         | (comparaFrente m j (pecaFrente)) && (verificaAlturasbaixo m j) = segundacomparacao m j
                                                         | pisoJogador == Terra = Just Acelera
                                                         | (verificaAlturasbaixo m j) && pisoBaixo == Terra = Just(Movimenta B)
                                                         | otherwise = Just Acelera
    where 
        pisoJogador = pisoPeca (encontraPosicaoMatriz (p,truncate d) m )
        pisoBaixo = pisoPeca (encontraPosicaoMatriz ((p+1), truncate d ) m ) 
        pecaFrente = encontraPosicaoMatriz(p,truncate d +1) m 
comparaPecasLimiteInferior :: Mapa-> Jogador-> Maybe Jogada
comparaPecasLimiteInferior  m j@(Jogador p d _ _ _ )| pisoJogador == Boost = Just Acelera
                                                   |  (verificaAlturasCima m j) && pisoCima == Boost = Just (Movimenta C)
                                                   | pisoJogador == Terra = Just Acelera
                                                   | (verificaAlturasCima m j) && pisoCima == Terra = Just(Movimenta C)
                                                   | otherwise = Just Acelera
    where 
        pisoJogador = pisoPeca (encontraPosicaoMatriz (p,truncate d) m )
        pisoCima = pisoPeca (encontraPosicaoMatriz ((p-1), truncate d ) m )
        

comparaPecasLimiteInferiorFrente :: Mapa-> Jogador-> Maybe Jogada
comparaPecasLimiteInferiorFrente  m j@(Jogador p d _ _ _ )| pisoJogador == Boost = Just Acelera
                                                         |  (verificaAlturasCima m j) && pisoCima == Boost = Just (Movimenta C)
                                                         | (comparaFrente m j (pecaFrente)) && (verificaAlturasCima m j) = segundacomparacao m j
                                                         | pisoJogador == Terra = Just Acelera
                                                         | (verificaAlturasCima m j) && pisoCima == Terra = Just(Movimenta C)
                                                         | otherwise = Just Acelera
    where 
        pisoJogador = pisoPeca (encontraPosicaoMatriz (p,truncate d) m )
        pisoCima = pisoPeca (encontraPosicaoMatriz ((p-1), truncate d ) m )
        pecaFrente = encontraPosicaoMatriz(p,truncate d +1) m 

comparaPecas:: Mapa-> Jogador-> Maybe Jogada
comparaPecas  m j@(Jogador p d _ _ _ ) |pisoJogador == Boost = Just Acelera
                                       | (comparaAlturasMC j (pecaJogador,pecaCima)) && (pisoCima == Boost) = Just (Movimenta C)
                                       | (comparaAlturasMB j (pecaJogador,pecaBaixo)) && (pisoBaixo == Boost ) = Just (Movimenta B)
                                       |  pisoJogador ==Terra = Just Acelera
                                       | (comparaAlturasMC j (pecaJogador,pecaCima)) && (pisoCima == Terra) = Just (Movimenta C)
                                       | comparaAlturasMB j (pecaJogador,pecaBaixo) && (pisoBaixo == Terra) = Just (Movimenta B)
                                       | otherwise = Just (Acelera)
    where 
        pecaCima =  (pecaSuperior j m)
        pecaBaixo =  (pecaInferior j m )
        pecaJogador = (encontraPosicaoMatriz (p,truncate d) m)
        pisoCima = pisoPeca pecaCima
        pisoBaixo = pisoPeca pecaBaixo
        pisoJogador = pisoPeca pecaJogador
        

comparaPecasFrente:: Mapa->Jogador-> Maybe Jogada
comparaPecasFrente m j@(Jogador p d _ _ _ )   |pisoJogador == Boost = Just Acelera
                                              | (comparaAlturasMC j (pecaJogador,pecaCima)) && (pisoCima == Boost) = Just (Movimenta C)
                                              | (comparaAlturasMB j (pecaJogador,pecaBaixo)) && (pisoBaixo == Boost ) = Just (Movimenta B)
                                              |  (comparaFrente m j (pecaFrente)) && (comparaAlturasMC j (pecaJogador,pecaCima)) = segundacomparacao m j 
                                              | (comparaFrente m j (pecaFrente)) && (comparaAlturasMB j (pecaJogador,pecaBaixo)) = segundacomparacao m j
                                              |  pisoJogador ==Terra = Just Acelera
                                              | (comparaAlturasMC j (pecaJogador,pecaCima)) && (pisoCima == Terra) = Just (Movimenta C)
                                              | comparaAlturasMB j (pecaJogador,pecaBaixo) && (pisoBaixo == Terra) = Just (Movimenta B)
                                              | otherwise = Just (Acelera)
    where 
        pecaCima =  (pecaSuperior j m)
        pecaBaixo =  (pecaInferior j m )
        pecaJogador = (encontraPosicaoMatriz (p,truncate d) m)
        pisoCima = pisoPeca pecaCima
        pisoBaixo = pisoPeca pecaBaixo
        pisoJogador = pisoPeca pecaJogador
        pecaFrente = encontraPosicaoMatriz(p,truncate d +1) m 

comparaFrente :: Mapa->Jogador-> (Peca)-> Bool
comparaFrente m j@(Jogador p d _ _ _ ) (pecaFrente ) |  pecaFrente == Cola = True
                                                     |  pecaFrente == Lama = True 
                                                     | otherwise = False
    where
        pecaFrente = pisoPeca( encontraPosicaoMatriz(p,truncate d +1) m )
        

segundacomparacao:: Mapa-> Jogador -> Maybe Jogada
segundacomparacao m j@(Jogador p d _ _ _ ) |  p==0 && (pisoBaixo == Cola || pisoBaixo == Lama)  = Just Acelera
                                           | p==0 && (pisoBaixo == Terra) && (verificaAlturasbaixo m j) = Just (Movimenta B)
                                           | p ==0 = Just Acelera
                                           | p == l-1 && (pisoCima == Cola||pisoCima == Lama)  = Just Acelera
                                           | p == l-1 && (pisoCima == Terra) && (verificaAlturasCima m j) = Just(Movimenta C)
                                           | p == l-1 = Just Acelera
                                           | (pisoCima == Terra) && (comparaAlturasMC j (pecaJogador,pecaCima)) = Just(Movimenta C)
                                           |  (pisoBaixo==Terra) && (comparaAlturasMB j (pecaJogador,pecaBaixo)) = Just (Movimenta B)
                                           |otherwise = Just Acelera
    where
        pecaCima =  (pecaSuperior j m)
        pecaBaixo =  (pecaInferior j m )
        pecaJogador = (encontraPosicaoMatriz (p,truncate d) m)
        pisoCima = pisoPeca pecaCima
        pisoBaixo = pisoPeca pecaBaixo
        (l,col) =dimensaoMatriz m

verificaAlturasbaixo :: Mapa->Jogador -> Bool
verificaAlturasbaixo m j@(Jogador p d _ _ _ )| alturaJogador > alturaPeca = True  
                                             | abs(alturaPeca-alturaJogador) <= 0.2 = True 
                                             | otherwise = False
    where
        alturaJogador = alturaJogadorF (encontraPosicaoMatriz (p,truncate d ) m ) d 
        alturaPeca = alturaJogadorF(encontraPosicaoMatriz (p+1,truncate d) m) d 

verificaAlturasCima :: Mapa -> Jogador -> Bool
verificaAlturasCima m j@(Jogador p d _ _ _ )| alturaJogador > alturaPeca = True  
                                            | abs(alturaPeca-alturaJogador) <= 0.2= True
                                            | otherwise = False
    where
        alturaJogador = alturaJogadorF (encontraPosicaoMatriz (p,truncate d ) m ) d 
        alturaPeca = alturaJogadorF(encontraPosicaoMatriz (p-1,truncate d) m) d
        

comparaAlturasMC :: Jogador -> (Peca,Peca)-> Bool
comparaAlturasMC j@(Jogador _  d _ _ _ ) (pecaJogador,pecaCima)|alturaJogador > alturaPecaCima = True 
                                                               | abs(alturaPecaCima-alturaJogador) <= 0.2 = True
                                                               | otherwise = False
    where
        alturaJogador = alturaJogadorF pecaJogador d
        alturaPecaCima= alturaJogadorF pecaCima d

comparaAlturasMB ::Jogador -> (Peca,Peca)-> Bool
comparaAlturasMB j@(Jogador _ d _ _ _ ) (pecaJogador,pecaBaixo) | alturaJogador > alturaPecaBaixo = True 
                                                                | abs(alturaPecaBaixo-alturaJogador) <= 0.2= True
                                                                | otherwise = False

    where
        alturaJogador = alturaJogadorF pecaJogador d
        alturaPecaBaixo= alturaJogadorF pecaBaixo d



jogadorNoAr:: Mapa-> Jogador -> Maybe Jogada
jogadorNoAr m j@(Jogador _ d _ _ (Ar alt inc g)) = difincs m j 

difincs ::Mapa -> Jogador -> Maybe  Jogada
difincs m j@(Jogador p d _ _ (Ar a inc g))| incPeca == inc = Just Acelera
                                          | incPeca > inc = Just (Movimenta E)
                                          | otherwise = Just(Movimenta D)
    where 
        incPeca = calcInclinacao (encontraPosicaoMatriz (p,truncate d ) m) 




