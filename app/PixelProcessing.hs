{-# LANGUAGE NamedFieldPuns #-}

module PixelProcessing where

import Assets (Pic, Pixel)
import CodeWorld
  ( Picture
  , (&)
  , blank
  , colored
  , dilated
  , red
  , solidRectangle
  , translated
  )
import Estado

data Sprite = Sprite
  { imagem :: Picture
  , altura :: Double
  , largura :: Double
  }

data SpriteAnimado = SpriteAnimado
  { spriteAtivo :: Sprite
  , spriteInativo :: Sprite
  , estado :: Estado
  }

pixelToPicture :: Pixel -> Picture
pixelToPicture ' ' = blank
pixelToPicture '#' = solidRectangle 1 1
pixelToPicture _ = colored red $ solidRectangle 1 1

concatPictures :: Double -> Double -> [Picture] -> Picture
concatPictures x y = foldl (\acc b -> translated x y acc & b) blank

concatPicturesH :: [Picture] -> Picture
concatPicturesH = concatPictures (-1) 0

concatPicturesV :: [Picture] -> Picture
concatPicturesV = concatPictures 0 1

picToSprite :: Double -> Pic -> Sprite
picToSprite escala pic = Sprite imagem (escala * altura) (escala * largura)
  where
    imagem =
      dilated escala
        . centralizar
        . concatPicturesV
        . map (concatPicturesH . map pixelToPicture)
        $ pic
    largura = fromIntegral . length . head $ pic
    altura = fromIntegral . length $ pic
    centralizar = translated (largura / 2) (altura / 2)

transladar :: Double -> Double -> Sprite -> Picture
transladar x y Sprite {imagem} = translated x y imagem

alternaSprite :: SpriteAnimado -> SpriteAnimado
alternaSprite s@SpriteAnimado {spriteAtivo, spriteInativo} =
  s {spriteAtivo = spriteInativo, spriteInativo = spriteAtivo}
