{-# LANGUAGE NamedFieldPuns #-}

module Nave where

import CodeWorld
import ConstantesGlobais
import Direcao
import Estado
import PixelProcessing

data Nave = Nave
  { spriteNave :: SpriteAnimado
  , xCoord :: Double
  , dirNave :: Direcao
  , vidas :: Int
  }

moverNaveParaAEsquerda :: Nave -> Nave
moverNaveParaAEsquerda n@Nave {spriteNave, xCoord}
  | xCoord - (largura . spriteAtivo) spriteNave <= limiteEsquerdo =
    n {dirNave = Parado}
  | otherwise = n {dirNave = Esquerda}

moverNaveParaADireita :: Nave -> Nave
moverNaveParaADireita n@Nave {spriteNave, xCoord}
  | xCoord + (largura . spriteAtivo) spriteNave >= limiteDireito =
    n {dirNave = Parado}
  | otherwise = n {dirNave = Direita}

pararNave :: Nave -> Nave
pararNave nave = nave {dirNave = Parado}

visualizarNave :: Nave -> Picture
visualizarNave Nave {spriteNave, xCoord} =
  transladar xCoord yNave . spriteAtivo $ spriteNave

visualizarVidas :: Int -> Sprite -> Picture
visualizarVidas n Sprite {imagem, largura} =
  translated xVidas yVidas
    . concatPictures (-largura * distanciaPadraoEntreObjetos) 0
    . replicate n
    $ imagem
