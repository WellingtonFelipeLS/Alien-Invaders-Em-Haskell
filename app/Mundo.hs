{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mundo where

import Alien
import Assets
import CodeWorld
import ConstantesGlobais
import Data.Maybe
import Direcao
import Estado
import Nave
import PixelProcessing
import System.Random

data MatrizDeAliens = MatrizDeAliens
  { aliens :: [[Alien]]
  , dir :: Direcao
  , timerTransicao :: Double
  }

data ProjetilNave = ProjetilNave
  { sprite :: Sprite
  , posProjNave :: Point
  , visivel :: Bool
  , dir :: Direcao
  }

data ProjetilAlien = ProjetilAlien
  { sprite :: Sprite
  , posProjAlien :: Point
  }

data Mundo = Mundo
  { matriz :: MatrizDeAliens
  , nave :: Nave
  , projNave :: ProjetilNave
  , projsAlien :: [ProjetilAlien]
  , sementeDeAleatoriedade :: StdGen
  , timerProjeteis :: Double
  }

spriteProjetil :: Sprite
spriteProjetil = picToSprite escala projetilPic

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just . head $ xs

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just . last $ xs

animarTransicao :: (Double, Mundo) -> (Double, Mundo)
animarTransicao (t, m@Mundo {matriz})
  | novoTimer >= 0.0 = (t, m {matriz = novaMatriz})
  | otherwise =
    ( t
    , m
        { matriz =
            matriz
              { aliens = map (map alternaSpriteNoLugar) aliens
              , timerTransicao = tempoDeTransicao
              }
        })
  where
    MatrizDeAliens {aliens, timerTransicao} = matriz
    novaMatriz = matriz {timerTransicao = novoTimer}
    novoTimer = timerTransicao - t
    alternaSpriteNoLugar alien@Alien {spriteAlien} =
      alien {spriteAlien = alternaSprite spriteAlien}

animarMovimentacao :: (Double, Mundo) -> (Double, Mundo)
animarMovimentacao (t, m@Mundo {matriz})
  | limiteDaTela =
    ( t
    , m
        { matriz =
            matriz
              { aliens = movimentarCadaAlien animarCadaAlienEmXEY
              , dir = inverter dir
              }
        })
  | otherwise =
    (t, m {matriz = matriz {aliens = movimentarCadaAlien animarCadaAlienEmX}})
  where
    MatrizDeAliens {aliens, dir} = matriz
    movimentarCadaAlien fMov = map (map $ fMov t dir) aliens
    xAlienMaisAEsquerda =
      verificarX . foldl1 (verificarAlienNaExtremidade (<)) . map maybeHead
        $ aliens
    xAlienMaisADireita =
      verificarX . foldl1 (verificarAlienNaExtremidade (>)) . map maybeLast
        $ aliens
    limiteDaTela =
      xAlienMaisAEsquerda <= -limiteHorizontal && dir == Esquerda
        || xAlienMaisADireita >= limiteHorizontal && dir == Direita

animarAliens :: (Double, Mundo) -> (Double, Mundo)
animarAliens = animarMovimentacao . animarTransicao

animarNave :: (Double, Mundo) -> (Double, Mundo)
animarNave (t, m@Mundo {nave}) = (t, m {nave = nave {xCoord = novoXCoord}})
  where
    Nave {xCoord, dirNave} = nave
    novoXCoord = xCoord + dirToDouble dirNave * velNave * t

animarMovimentacaoNave :: (Nave -> Nave) -> Mundo -> Mundo
animarMovimentacaoNave acaoNave m@Mundo {nave} = m {nave = acaoNave nave}

animarProjetilNave :: (Double, Mundo) -> (Double, Mundo)
animarProjetilNave (t, m@Mundo {projNave = p@ProjetilNave {visivel = True}})
  | foraDaTela = (t, m {projNave = p {visivel = False}})
  | otherwise = (t, m {projNave = p {posProjNave = novaPos}})
  where
    ProjetilNave {posProjNave = (x, y), dir} = p
    foraDaTela = y >= 10 || x <= -10 || x >= 10
    novaPos = (x + dirToDouble dir * velNave * t, y + velProjetil * t)
animarProjetilNave p = p

criarProjetilAlien :: (Double, Mundo) -> (Double, Mundo)
criarProjetilAlien (t, m)
  | null aliensAtiradores = (t, m)
  | novoTimer >= 0 = (t, m {timerProjeteis = novoTimer})
  | otherwise =
    ( t
    , m
        { projsAlien = novoProjetil : projsAlien
        , sementeDeAleatoriedade = g
        , timerProjeteis = tempoEntreProjeteis
        })
  where
    Mundo { matriz = MatrizDeAliens {aliens}
          , projsAlien
          , sementeDeAleatoriedade
          , timerProjeteis
          } = m
    aliensAtiradores = last aliens
    maxIdx = length aliensAtiradores - 1
    (idx, g) = randomR (0, maxIdx) sementeDeAleatoriedade
    Alien {posAlien} = aliensAtiradores !! idx
    novoProjetil =
      ProjetilAlien {sprite = spriteProjetil, posProjAlien = posAlien}
    novoTimer = timerProjeteis - t

animarProjetilAlien :: (Double, Mundo) -> (Double, Mundo)
animarProjetilAlien (t, m@Mundo {projsAlien}) =
  (t, m {projsAlien = map f projsAlien})
  where
    f p@ProjetilAlien {posProjAlien = (x, y)} =
      p {posProjAlien = (x, y - velProjetil * t)}

removerProjeteisAliens :: (Double, Mundo) -> (Double, Mundo)
removerProjeteisAliens (t, m@Mundo {projsAlien}) =
  (t, m {projsAlien = novosProjs})
  where
    funcaoDeFiltro = (> -10) . snd . posProjAlien
    novosProjs = filter funcaoDeFiltro projsAlien

verificarColisaoPA :: ProjetilNave -> Alien -> Bool
verificarColisaoPA projNave alien =
  abs (xProj - xAlien) <= largura / 2 && abs (yProj - yAlien) <= altura / 2
  where
    (xProj, yProj) = posProjNave projNave
    (xAlien, yAlien) = posAlien alien
    Sprite {altura, largura} = spriteAtivo . spriteAlien $ alien

explodirAlien :: (Alien -> Bool) -> [Alien] -> [Alien]
explodirAlien _ [] = []
explodirAlien f (a:as)
  | f a && (estaAtivo . estado) spriteAtual = a {spriteAlien = novoSprite} : as
  | otherwise = a : explodirAlien f as
  where
    spriteDeExplosao = picToSprite escala explosaoPic
    spriteAtual = spriteAlien a
    novoSprite =
      spriteAtual
        {spriteAtivo = spriteDeExplosao, estado = EmInativacao tempoDeExplosao}

animarColisaoNosAliens :: (Double, Mundo) -> (Double, Mundo)
animarColisaoNosAliens (t, m@Mundo {matriz, projNave})
  | visivel projNave && numAliensAtivos novosAliens < numAliensAtivos aliens =
    (t, m {matriz = novaMatriz, projNave = projetilInvisivel})
  | otherwise = (t, m)
  where
    MatrizDeAliens {aliens} = matriz
    novosAliens = map (explodirAlien $ verificarColisaoPA projNave) aliens
    novaMatriz = matriz {aliens = novosAliens}
    projetilInvisivel = projNave {visivel = False}

verificarColisaoPN :: Nave -> ProjetilAlien -> Bool
verificarColisaoPN nave projAlien =
  abs (xProj - xNave) <= largura / 2 && abs (yProj - yNave) <= altura / 2
  where
    (xProj, yProj) = posProjAlien projAlien
    xNave = xCoord nave
    Sprite {altura, largura} = spriteAtivo . spriteNave $ nave

animarColisaoNaNave :: (Double, Mundo) -> (Double, Mundo)
animarColisaoNaNave (t, m@Mundo {nave, projsAlien})
  | any (verificarColisaoPN nave) projsAlien =
    (t, m {nave = novaNave, projsAlien = []})
  | otherwise = (t, m)
  where
    novaNave = nave {spriteNave = novoSprite, vidas = vidas nave - 1}
    spriteAtual = spriteNave nave
    novoSprite = spriteAtual {estado = EmInativacao tempoDeExplosao}

removerAliensInativos :: (Double, Mundo) -> (Double, Mundo)
removerAliensInativos (t, m@Mundo {matriz}) =
  (t, m {matriz = matriz {aliens = aliensAnimaveisEAtualizados}})
  where
    a = aliens matriz
    aliensAnimaveisEAtualizados =
      map (atualizarAliensEmInativacao t . filtrarAliensAnimaveis) a

fluxoNormalDoJogo :: (Double, Mundo) -> (Double, Mundo)
fluxoNormalDoJogo =
  animarNave
    . animarAliens
    . animarProjetilNave
    . animarProjetilAlien
    . animarColisaoNaNave
    . animarColisaoNosAliens
    . removerAliensInativos
    . removerProjeteisAliens
    . criarProjetilAlien

verificarVitoria :: Mundo -> Bool
verificarVitoria = all null . aliens . matriz

verificarDerrota :: Mundo -> Bool
verificarDerrota Mundo {matriz = MatrizDeAliens {aliens}, nave} =
  any (any ((<= yNave) . snd . posAlien)) aliens || vidas nave == 0

visualizarMatrizDeAliens :: MatrizDeAliens -> Picture
visualizarMatrizDeAliens MatrizDeAliens {aliens} =
  pictures $ concatMap (map visualizarAlien) aliens

visualizarProjetilNave :: ProjetilNave -> Picture
visualizarProjetilNave ProjetilNave {sprite, posProjNave = (x, y), visivel, dir}
  | visivel = transladar x y sprite
  | otherwise = blank

visualizarProjeteisAliens :: [ProjetilAlien] -> Picture
visualizarProjeteisAliens = pictures . map visualizarProjetilAlien
  where
    visualizarProjetilAlien ProjetilAlien {sprite, posProjAlien = (x, y)} =
      translated x y . imagem $ sprite
