{-|
--  Introdução à Tarefa 5
-- 	 	Objetivo da Tarefa
O  objetivo desta Tarefa era implementarmos todas as tarefas realizadas até agora numa interface gráfica. Para esse efeito foi usado o Gloss , uma biblioteca que permite criar
jogos , tal como o Excite Bike que nós desenvolvemos.

Numa primeira fase começamos por representar o nosso mapa como quadrados sem cores , e o nosso jogador como um circulo.

-- Desenvolvimento da Tarefa
-- Jogo a ganhar forma

Após conseguirmos desenhar um mapa , tratamos de conseguir por o mapa "bonito" de modo a que fosse percetível que era realmente um jogo. Para isso decidimos atribuir cores ao quadrados
que representavam as 'Peca's e logo reparamos que surgiu uma dificuldade que era a sobreposição das pistas. Para a resolução desta dificuldade optamos por criar um outliner que delimita 
cada 'Peca' com uma linha preta, assim sendo mais percetível em que pista o Jogador se encontrava. 

Para representarmos o Jogador usamos uma mota do estilo do filme "Tron" pois achavamos que era mais atrativo . 

Inicialmente o jogo era desenvolvido para um só Player mas decidimos acrescentar um menu onde podemos optar por jogar sozinhos ou então jogarmos contra um colega , o chamado 1vs1.

-- Conclusão 
--  Conclusão e principais problemas encontrados

Um dos grandes problemas encontrados foi aprender a usar esta biblioteca , o Gloss, sendo que é quase como se fosse uma nova "linguagem" , mas após termos percebido a realização das partes básicas
foi bastante simples.

Outro dos grandes problemas encontrados foi o facto de trabalharmos com imagens e termos de ampliar, rodar, transforma-las de modo a deixar o jogo "jogável".

-}



module Main where

import LI11920
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Tarefa1_2019li1g050
import Tarefa4_2019li1g050
import Tarefa0_2019li1g050
import Tarefa2_2019li1g050
import Graphics.Gloss.Juicy
import GHC.Float

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do 
    Just motaAzul <- loadJuicy "MotaAzul.png"
    Just motaVerde <- loadJuicy "MotaVerde.png"
    Just menu <- loadJuicy "Menu.png"
    Just background <- loadJuicy "Background.png"
    Just seta <- loadJuicy "Selecao.png"
    Just verdeVence <- loadJuicy "VerdeVencedor.png"
    Just azulVence <- loadJuicy "AzulVencedor.png"
    Just morto <- loadJuicy "CaveiraPreta.png"
    Just instrucoes <- loadJuicy "Instrucoes.png"
    play dm
        (greyN 0.5) 
        fr 
        estadoInicial 
        (\e -> desenhaEstados e [motaAzul,motaVerde] menu background seta verdeVence azulVence morto instrucoes)
        reageEvento
        reageTempo

type EstadoBloss = (Estado,[Picture])

data EstadoGloss 
    = EstadoGlossEmJogoTreino {estadoJogo :: Estado}
    | EstadoGlossEmJogo1vs1 {estadoJogo :: Estado}
    | EstadoGlossEmMenu {opcao :: Int}
    | EstadoGlossVencedor {jogador :: Int}
    | EstadoGlossInstrucoes 


dmEstadoInicial :: EstadoGloss -> DimensaoMatriz
dmEstadoInicial (EstadoGlossEmJogoTreino (Estado m j)) = dimensaoMatriz m
dmEstadoInicial (EstadoGlossEmJogo1vs1 (Estado m j)) = dimensaoMatriz m

estadoInicial :: EstadoGloss
estadoInicial = (EstadoGlossEmMenu 1) 

translateMapa :: DimensaoMatriz -> Picture -> Picture
translateMapa (x,y) mapa = Translate (-mC*100) (mL*50) mapa
    where
        mC = (realToFrac (y) / 2)
        mL = (realToFrac (x) / 2)


-- | Função que consoante o estado atual gloss devolve o desenho correspondente 
desenhaEstados :: EstadoGloss -> [Picture] -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
desenhaEstados e@(EstadoGlossEmJogo1vs1 (Estado m j)) motas _ background _ _ _ morto _ = Pictures [background, desenhaEstadoJogo e (desenhaMapa e) (desenhaJogadores e motas morto)]
desenhaEstados e@(EstadoGlossEmJogoTreino (Estado m j)) motas _ background _ _ _ morto _ = Pictures [background, desenhaEstadoJogo e (desenhaMapa e) (desenhaJogadores e motas morto)]
desenhaEstados e@(EstadoGlossEmMenu 1) _ menu _ seta _ _ _ _ = Pictures [menu, (Translate (-420) (-65) seta)]
desenhaEstados e@(EstadoGlossEmMenu 2) _ menu _ seta _ _ _ _ = Pictures [menu, (Translate (-420) (-285) seta)]
desenhaEstados e@(EstadoGlossEmMenu _) _ menu _ seta _ _ _ _ = menu
desenhaEstados e@(EstadoGlossVencedor 0) _ _ _ _ _ azulVence _ _ = azulVence
desenhaEstados e@(EstadoGlossVencedor 1) _ _ _ _ verdeVence _ _ _ = verdeVence 
desenhaEstados e@(EstadoGlossInstrucoes) _ _ _ _ _ _ _ instrucoes = instrucoes

-- | Função que desenha o estados de jogo, treino e 1vs1. 
desenhaEstadoJogo :: EstadoGloss -> [Picture] -> [Picture] -> Picture
desenhaEstadoJogo eg@(EstadoGlossEmJogoTreino (Estado m j)) pistas jogadores = translateMapa (dmEstadoInicial eg) (desenhaEstado' (zip pistas jogadores))
desenhaEstadoJogo eg@(EstadoGlossEmJogo1vs1 (Estado m j)) pistas jogadores = translateMapa (dmEstadoInicial eg) (desenhaEstado' (zip pistas jogadores))

desenhaEstado' :: [(Picture,Picture)] -> Picture
desenhaEstado' [] = Blank
desenhaEstado' ((p1,p2):t) = Pictures [p1,p2,Translate 0 (-100) $ (desenhaEstado' t)]

-- | Função que desenha o Mapa pista a pista 
desenhaMapa :: EstadoGloss -> [Picture]
desenhaMapa eg@(EstadoGlossEmJogoTreino (Estado [] j) ) = []
desenhaMapa eg@(EstadoGlossEmJogoTreino (Estado (pista : t) j)) = desenhaPista pista : desenhaMapa (EstadoGlossEmJogoTreino (Estado t j))
desenhaMapa eg@(EstadoGlossEmJogo1vs1 (Estado [] j) ) = []
desenhaMapa eg@(EstadoGlossEmJogo1vs1 (Estado (pista : t) j)) = desenhaPista pista : desenhaMapa (EstadoGlossEmJogo1vs1 (Estado t j))

desenhaPista :: Pista -> Picture 
desenhaPista [] = Blank
desenhaPista (peca : t) = Pictures [desenhaPeca peca,Translate 100 0 $ desenhaPista t]

-- | Interpreta o tipo de peça para saber qual o desenho a fazer
desenhaPeca :: Peca -> Picture
desenhaPeca p@(Recta _ a) = Pictures [(tipoPeca p quadrado),(tipoPeca p fillerRecta),outLineRecta]
    where
        quadrado = Polygon [(0,-100+aR*100),(100,-100+aR*100),(100,aR*100),(0,aR*100)]
        fillerRecta = Polygon [(0,-100*aR),(100,-100*aR),(100,0),(0,0)]
        outLineRecta = Line [(0,-100+aR*100),(100,-100+aR*100),(100,aR*100),(0,aR*100),(0,-100+aR*100)] 
        aR = realToFrac a
desenhaPeca p@(Rampa _ a1 a2) | a1 < a2 = Pictures [(tipoPeca p trapezioSobe),(tipoPeca p fillerRampa),outLineTrapezioSobe]
                              | otherwise = Pictures [(tipoPeca p trapezioDesce),(tipoPeca p fillerRampa),outLineTrapezioDesce]
    where
        trapezioSobe = Polygon [(0,-100+aD*100),(100,aS*100-100),(100,aS*100),(0,aD*100)]
        trapezioDesce = Polygon [(0,aD*100),(0,aD*100-100),(100,-100+aS*100),(100,aS*100)]
        fillerRampa = Polygon [(0,-100),(100,-100),(100,(-100)+100*aS),(0,(-100)+100*aD)]
        outLineTrapezioSobe = Line [(0,-100+aD*100),(100,aS*100-100),(100,aS*100),(0,aD*100),(0,-100+aD*100)]
        outLineTrapezioDesce = Line [(0,aD*100),(0,aD*100-100),(100,-100+aS*100),(100,aS*100),(0,aD*100)]
        aS = realToFrac a2
        aD = realToFrac a1

-- | TIpos de peça definidos
tipoPeca :: Peca -> Picture -> Picture
tipoPeca peca@(Recta piso _) quadrado = case pisoPeca peca of
    Terra -> Color (dark orange) quadrado
    Relva -> Color (dark (dark green)) quadrado
    Boost -> Color (greyN 0.2) quadrado
    Cola -> Color (greyN 0.5) quadrado
    Lama -> Color (dark red) quadrado
tipoPeca peca@(Rampa piso _ _) trapezio = case pisoPeca peca of
    Terra -> Color (dark orange) trapezio
    Relva -> Color (dark (dark green)) trapezio
    Boost -> Color (greyN 0.2) trapezio
    Cola -> Color (greyN 0.5) trapezio
    Lama -> Color (dark red) trapezio


desenhaJogadores :: EstadoGloss -> [Picture] -> Picture -> [Picture]
desenhaJogadores (EstadoGlossEmJogoTreino e@(Estado m jogadores)) motas morto = map snd $ desenhaJogadores' e motas pics jogadores morto
    where
        pics = [(i,Blank) | i <- [0..pistas]] 
        (pistas,colunas) = dimensaoMatriz m
desenhaJogadores (EstadoGlossEmJogo1vs1 e@(Estado m jogadores)) motas morto = map snd $ desenhaJogadores' e motas pics jogadores morto
    where
        pics = [(i,Blank) | i <- [0..pistas]]
        (pistas,colunas) = dimensaoMatriz m



inserePic :: (Int,Picture) -> [(Int,Picture)] -> [(Int,Picture)]
inserePic p@(pista,pic) pL@((pistaL,picL) : t) = inserePic' p pL []


inserePic' :: (Int,Picture) -> [(Int,Picture)] -> [(Int,Picture)] -> [(Int,Picture)]
inserePic' p@(pista,pic) pL@((pistaL,picL) : t) l | pista == pistaL = reverse l ++ (pista,novaPic) : t
                                                  | otherwise = inserePic' p t ((pistaL,picL) : l)
    where
        novaPic = Pictures [picL,pic]

desenhaJogadores' :: Estado -> [Picture] -> [(Int,Picture)] -> [Jogador] -> Picture -> [(Int,Picture)]
desenhaJogadores' e motas pics [] morto = pics
desenhaJogadores' e (mota:t) pics (j:js) morto = inserePic (desenhaJogador e j mota morto) (desenhaJogadores' e t pics js morto)


desenhaJogador :: Estado -> Jogador -> Picture -> Picture -> (Int,Picture)
desenhaJogador (Estado m jogs) (Jogador p d v c (Ar altura inc g)) mota morto = (p,Translate ((double2Float d)*100) (double2Float((-50)-((realToFrac p)-(altura*100)))) (Rotate (-(double2Float inc)) (Scale 0.3 0.3 mota)))                                                                              
desenhaJogador (Estado m jogs) (Jogador p d v c (Chao x)) mota morto = (p,Translate ((double2Float d)*100) (double2Float((-50)-((realToFrac p)-(alturaJogador*100)))) (Rotate (-(double2Float inc)) (Scale 0.3 0.3 mota)))                                                                          
    where 
        alturaJogador = alturaJogadorF (encontraPosicaoMatriz (p,truncate d) m) d
        inc = calcInclinacao (encontraPosicaoMatriz (p,truncate d) m)
desenhaJogador (Estado m jogs) (Jogador p d v c (Morto t)) mota morto = (p,Translate ((double2Float d)*100) (double2Float((-50)-((realToFrac p)-(alturaJogador*100)))) (Rotate (-(double2Float inc)) (Scale 0.05 0.05 morto)))
    where 
        alturaJogador = alturaJogadorF (encontraPosicaoMatriz (p,truncate d) m) d
        inc = calcInclinacao (encontraPosicaoMatriz (p,truncate d) m)

-- | Dado um estado e um input altera o estado gloss ou altera o estado de jogo 
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyUp) Down _  _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Movimenta C) e))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Movimenta B) e))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Movimenta E) e))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Movimenta D) e))
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Acelera) e))
reageEvento (EventKey (Char 'c') Down _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Dispara) e))
reageEvento (EventKey (SpecialKey KeySpace) Up _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmJogoTreino (jogada 0 (Desacelera) e))
reageEvento (EventKey (Char 'm') Down _ _) (EstadoGlossEmJogoTreino e) = (EstadoGlossEmMenu 1)
reageEvento _ eg@(EstadoGlossEmJogoTreino e@(Estado m j)) = eg
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (EstadoGlossEmMenu _) = (EstadoGlossEmMenu 1)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (EstadoGlossEmMenu _) = (EstadoGlossEmMenu 2)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EstadoGlossEmMenu 1) = (EstadoGlossEmJogoTreino (Estado (gera 3 15 1) [(Jogador 1 0 0 4 (Chao False))]))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EstadoGlossEmMenu 2) = (EstadoGlossEmJogo1vs1 (Estado (gera 4 15 1) [(Jogador 1 0 0 4 (Chao False)),(Jogador 2 0 0 4 (Chao False))]))
reageEvento (EventKey (Char 'i') Down _ _) (EstadoGlossEmMenu _) = (EstadoGlossInstrucoes)
reageEvento _ e@(EstadoGlossEmMenu _ ) = e
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Movimenta C) e))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Movimenta B) e))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Movimenta E) e))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Movimenta D) e))
reageEvento (EventKey (Char 'k') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Acelera) e))
reageEvento (EventKey (Char 'k') Up _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Desacelera) e))
reageEvento (EventKey (Char 'l') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 0 (Dispara) e))
reageEvento (EventKey (Char 't') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Movimenta C) e))
reageEvento (EventKey (Char 'g') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Movimenta B) e))
reageEvento (EventKey (Char 'f') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Movimenta E) e))
reageEvento (EventKey (Char 'h') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Movimenta D) e))
reageEvento (EventKey (Char 'z') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Acelera) e))
reageEvento (EventKey (Char 'x') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Dispara) e))
reageEvento (EventKey (Char 'z') Up _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmJogo1vs1 (jogada 1 (Desacelera) e))
reageEvento (EventKey (Char 'm') Down _ _) (EstadoGlossEmJogo1vs1 e) = (EstadoGlossEmMenu 1)
reageEvento _ e@(EstadoGlossEmJogo1vs1 (Estado m j)) = e
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EstadoGlossVencedor _) = (EstadoGlossEmMenu 1)
reageEvento _ e@(EstadoGlossVencedor _) = e
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EstadoGlossInstrucoes) = (EstadoGlossEmMenu 1)
reageEvento _ e@(EstadoGlossInstrucoes) = e





reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo n es@(EstadoGlossEmJogoTreino (Estado m ((Jogador p d v c ej):t))) | d > 14.9 = (EstadoGlossEmMenu 1)
reageTempo n es@(EstadoGlossEmJogoTreino (Estado m j)) = (EstadoGlossEmJogoTreino (Estado m (atualizaIndiceLista 0 passagemTempo j)))
    where
        passagemTempo = passo (realToFrac n) m (encontraIndiceLista 0 j)
reageTempo n es@(EstadoGlossEmJogo1vs1 (Estado m ((Jogador p1 d1 v1 c1 ej1):(Jogador p2 d2 v2 c2 ej2):t))) | d1 > 14.9 = (EstadoGlossVencedor 0)
                                                                                                           | d2 > 14.9 = (EstadoGlossVencedor 1)
reageTempo n es@(EstadoGlossEmJogo1vs1 (Estado m j)) = (EstadoGlossEmJogo1vs1 (Estado m (atualizaIndiceLista 0 passagemTempoJ1 (atualizaIndiceLista 1 passagemTempoJ2 j))))
    where
        passagemTempoJ1 = passo (realToFrac n) m (encontraIndiceLista 0 j)
        estadoAtualizadoJ1 = (EstadoGlossEmJogo1vs1 (Estado m (atualizaIndiceLista 0 passagemTempoJ1 j)))
        passagemTempoJ2 = passo (realToFrac n) m (encontraIndiceLista 1 j)
reageTempo n e@(EstadoGlossEmMenu o) = e
reageTempo n e@(EstadoGlossVencedor x) = e
reageTempo n e@(EstadoGlossInstrucoes) = e
                                                     

fr :: Int 
fr = 50



dm :: Display 
dm = InWindow "Novo Jogo" (1920,1080) (0,0)