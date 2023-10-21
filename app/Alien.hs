{-# LANGUAGE NamedFieldPuns #-}

module Alien where

import Assets
import CodeWorld
import ConstantesGlobais
import Direcao
import Estado
import PixelProcessing

data Alien = Alien
  { spriteAlien :: SpriteAnimado
  , posAlien :: Point
  }

picToAlien :: Double -> (Pic, Pic) -> Alien
picToAlien escala (a, b) =
  Alien
    (SpriteAnimado (picToSprite escala a) (picToSprite escala b) Ativo)
    (0, 0)

transladarAlien :: Double -> Double -> Alien -> Alien
transladarAlien x y alien = alien {posAlien = (x, y)}

transladarAlienEmX :: Double -> Alien -> Alien
transladarAlienEmX x a@Alien {posAlien = (_, y)} = transladarAlien x y a

transladarAlienEmY :: Double -> Alien -> Alien
transladarAlienEmY y a@Alien {posAlien = (x, _)} = transladarAlien x y a

criarLinhaDeAliens :: Alien -> [Alien]
criarLinhaDeAliens = replicate qntAliensPorLinha

criarMatrizDeAliens :: [Alien] -> [[Alien]]
criarMatrizDeAliens = map criarLinhaDeAliens

posicionarLinhas :: [Alien] -> [Alien]
posicionarLinhas = zipWith transladarAlienEmX [xReferencia,xProximo ..]

posicionarColunas :: [[Alien]] -> [[Alien]]
posicionarColunas = zipWith (map . transladarAlienEmY) [yReferencia,yProximo ..]

posicionarAliens :: [Alien] -> [[Alien]]
posicionarAliens =
  posicionarColunas . map posicionarLinhas . criarMatrizDeAliens

animarCadaAlienEmXEY :: Double -> Direcao -> Alien -> Alien
animarCadaAlienEmXEY t dir a@Alien {posAlien = (x, y)} =
  a {posAlien = (x + dirToDouble dir * velAliens * t, y - distanciaDeDescida)}

animarCadaAlienEmX :: Double -> Direcao -> Alien -> Alien
animarCadaAlienEmX t dir a@Alien {posAlien = (x, y)} =
  a {posAlien = (x + dirToDouble dir * velAliens * t, y)}

verificarX :: Maybe Alien -> Double
verificarX Nothing = -20
verificarX (Just a) = fst . posAlien $ a

visualizarAlien :: Alien -> Picture
visualizarAlien (Alien {spriteAlien, posAlien = (x, y)}) =
  transladar x y . spriteAtivo $ spriteAlien

verificarAlienNaExtremidade ::
     (Double -> Double -> Bool) -> Maybe Alien -> Maybe Alien -> Maybe Alien
verificarAlienNaExtremidade _ Nothing Nothing = Nothing
verificarAlienNaExtremidade _ Nothing a2 = a2
verificarAlienNaExtremidade _ a1 Nothing = a1
verificarAlienNaExtremidade f a1 a2
  | verificarX a1 `f` verificarX a2 = a1
  | otherwise = a2

filtrarAliensAtivos :: [Alien] -> [Alien]
filtrarAliensAtivos = filter $ estaAtivo . estado . spriteAlien

filtrarAliensAnimaveis :: [Alien] -> [Alien]
filtrarAliensAnimaveis = filter $ estaAnimavel . estado . spriteAlien

atualizarAliensEmInativacao :: Double -> [Alien] -> [Alien]
atualizarAliensEmInativacao _ [] = []
atualizarAliensEmInativacao t (a:as) =
  case (estado . spriteAlien) a of
    Ativo -> a : atualizarAliensEmInativacao t as
    e@EmInativacao {timer} ->
      if estaAnimavel e
        then a {spriteAlien = novoSprite} : atualizarAliensEmInativacao t as
        else atualizarAliensEmInativacao t as
      where spriteAtual = spriteAlien a
            novoSprite = spriteAtual {estado = EmInativacao (timer - t)}

numAliensAtivos :: [[Alien]] -> Int
numAliensAtivos aliens = length aliensAtivos * (sum . map length $ aliensAtivos)
  where
    aliensAtivos = filter (not . null) . map filtrarAliensAtivos $ aliens
