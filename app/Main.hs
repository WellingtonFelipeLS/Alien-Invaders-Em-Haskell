{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Alien
import Assets
import CodeWorld
import ConstantesGlobais
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import Direcao
import Estado
import Mundo
import Nave
import PixelProcessing
import System.Random (Random(randomR), StdGen, initStdGen, mkStdGen, newStdGen)

type World = Maybe Mundo

alien1 :: Alien
alien1 = picToAlien escala (alien1_1, alien1_2)

alien2 :: Alien
alien2 = picToAlien escala (alien2_1, alien2_2)

alien3 :: Alien
alien3 = picToAlien escala (alien3_1, alien3_2)

matrizInicial :: MatrizDeAliens
matrizInicial =
  MatrizDeAliens
    { aliens = posicionarAliens [alien2, alien2, alien1, alien1, alien3]
    , dir = Direita
    , timerTransicao = 1
    }

naveInicial :: Nave
naveInicial =
  Nave
    { spriteNave =
        SpriteAnimado
          (picToSprite escala navePic)
          (picToSprite escala explosaoPic)
          Ativo
    , xCoord = xNaveInicial
    , dirNave = Parado
    , vidas = 3
    }

projetilInicial :: ProjetilNave
projetilInicial =
  ProjetilNave
    { sprite = picToSprite escala projetilPic
    , posProjNave = (20, 20)
    , visivel = False
    , dir = Parado
    }

mundoInicial :: World
mundoInicial =
  Just
    Mundo
      { matriz = matrizInicial
      , nave = naveInicial
      , projNave = projetilInicial
      , projsAlien = []
      , sementeDeAleatoriedade = mkStdGen 0
      , timerProjeteis = tempoEntreProjeteis
      }

animacao :: Event -> World -> World
animacao (KeyPress "Left") m =
  fmap (animarMovimentacaoNave moverNaveParaAEsquerda) m
animacao (KeyRelease "Left") m
  | isJust m && (dirNave . nave . fromJust) m == Direita =
    animacao (KeyPress "Right") m
  | otherwise = fmap (animarMovimentacaoNave pararNave) m
animacao (KeyPress "Right") m =
  fmap (animarMovimentacaoNave moverNaveParaADireita) m
animacao (KeyRelease "Right") m
  | isJust m && (dirNave . nave . fromJust) m == Esquerda =
    animacao (KeyPress "Left") m
  | otherwise = fmap (animarMovimentacaoNave pararNave) m
animacao (KeyPress " ") (Just m)
  | not . visivel . projNave $ m = Just (m {projNave = novoProjetil})
  | otherwise = Just m
  where
    Mundo {nave = Nave {xCoord, dirNave}, projNave = projAtual} = m
    novoProjetil =
      projAtual {posProjNave = (xCoord, yNave), visivel = True, dir = dirNave}
animacao (TimePassing t) (Just m)
  | verificarDerrota m = Nothing
  | verificarVitoria m =
    Just (e {sementeDeAleatoriedade = sementeDeAleatoriedade m})
  | houveTransicao =
    Just (m {nave = naveInicial {vidas = vidas}, projNave = projetilInicial})
  | otherwise =
    case estadoAtual of
      EmInativacao {} -> Just (m {nave = novaNave})
      Ativo -> Just . snd . fluxoNormalDoJogo $ (t, m)
  where
    Just e = mundoInicial
    naveAtual@Nave {spriteNave, vidas} = nave m
    estadoAtual = estado spriteNave
    (novoEstado, houveTransicao) = atualizarEstado t estadoAtual
    novoSprite = alternaSprite spriteNave
    novaNave = naveAtual {spriteNave = novoSprite {estado = novoEstado}}
animacao _ m = m

visualizacao :: World -> Picture
visualizacao Nothing = dilated 3 . lettering $ "💀Derrota💀"
visualizacao (Just Mundo {matriz, nave, projNave, projsAlien}) =
  visualizarProjetilNave projNave
    & visualizarVidas nVidas img
    & visualizarMatrizDeAliens matriz
    & visualizarProjeteisAliens projsAlien
    & visualizarNave nave
  where
    img = spriteAtivo . spriteNave $ naveInicial
    nVidas = vidas nave

main :: IO ()
main = do
  g <- initStdGen
  let (Just e) = mundoInicial
  let estadoInicialAleatorio = Just e {sementeDeAleatoriedade = g}
  activityOf estadoInicialAleatorio animacao visualizacao
