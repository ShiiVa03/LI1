
module Testes where

import LI11920

player0 = (Jogador 1 1.1 1 5 (Chao True))
player1 = (Jogador 0 1.1 1 5 (Chao True))
player2 = (Jogador 0 1.8 1 5 (Chao True))
player3 = (Jogador 1 3.2 1 5 (Chao True))
player4 = (Jogador 0 3.2 1 5 (Chao True))
player5 = (Jogador 0 5.2 1 5 (Chao True))
player6 = (Jogador 1 5.2 1 5 (Chao True))
player7 = (Jogador 1 7.2 1 5 (Chao True))
player8 = (Jogador 0 7.2 1 5 (Chao True))
player9 = (Jogador 1 1.1 1 5 (Chao False))
player10 = (Jogador 1 2.2 1 5 (Morto 1))
player11 = (Jogador 1 3.2 1 5 (Ar 2 10 0))
player12 = (Jogador 0 0.3 1 5 (Chao True))
player13 = (Jogador 0 2.3 1 5 (Ar 3 80 0))
player14 = (Jogador 0 2.3 1 5 (Ar 3 (-80) 0))
player15 = (Jogador 0 2.1 1 0 (Chao True))


play0 = (Movimenta C)
play1 = (Movimenta B)
play2 = (Movimenta D)
play3 = (Movimenta E)
play4 = (Acelera)
play5 = (Desacelera)
play6 = (Dispara)

movRetasMesmaAlturaB = (3,play1,e0)
movRetasMesmaAlturaC = (4,play0,e0)
movRetaAltaParaBaixaB = (5,play1,e0)
movRetaAltaParaBaixaC = (7,play0,e0)
movRetaBaixaParaAltaC = (6,play0,e0)
movRetaBaixaParaAltaB = (8,play1,e0)
movLimiteSuperior = (1,play0,e0)
movLimiteInferior = (6,play1,e0)
movRampaPRetaPossivel = (1,play1,e0)
movRetaPRampaPossivel = (0,play0,e0)
movRampaPRetaPossivelAr = (2,play1,e0)
movRetaPRampaImpossivel = (6,play0,e0)
aceleraNoChaoQuandoTrue = (0,play4,e0)
aceleraNoChaoQuandoFalse = (9,play4,e0)
aceleraMorto = (10,play4,e0)
aceleraNoAr = (11,play4,e0)
desaceleraNoChaoQuandoTrue = (0,play5,e0)
desaceleraNoChaoQuandoFalse = (9,play5,e0)
desaceleraMorto = (10,play5,e0)
desaceleraNoAr = (11,play5,e0)
disparaNaPartida = (12,play6,e0)
disparaNoChao = (8,play6,e0)
disparaMorto = (10,play6,e0)
disparaNoAr = (11,play6,e0)
disparaSemCola = (15,play6,e0)
inclinaENoChao = (8,play3,e0)
inclinaDNoChao = (8,play2,e0)
inclinaEMorto = (10,play3,e0)
inclinaDMorto = (10,play2,e0)
inclinaENoAr = (11,play3,e0)
inclinaDNoAr = (11,play2,e0)
inclinaELimite = (13,play3,e0)
inclinaDLimite = (14,play2,e0)


-- jogadaNoAr :: Int -> Jogada -> Estado -> Estado

e0 = Estado   [[Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2,Rampa Terra 2 1,Recta Terra 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2,Recta Terra 2]] [player0,player1,player2,player3,player4,player5,player6,player7,player8,player9,player10,player11,player12,player13,player14,player15]