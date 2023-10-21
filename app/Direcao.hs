module Direcao where

data Direcao
  = Esquerda
  | Parado
  | Direita
  deriving (Eq)

dirToDouble :: Direcao -> Double
dirToDouble Esquerda = -1.0
dirToDouble Parado = 0.0
dirToDouble Direita = 1.0

inverter :: Direcao -> Direcao
inverter Esquerda = Direita
inverter Direita = Esquerda
inverter Parado = Parado
