{-# LANGUAGE NamedFieldPuns #-}

module Estado where

data Estado
  = Ativo
  | EmInativacao
      { timer :: Double
      }

estaAnimavel :: Estado -> Bool
estaAnimavel Ativo = True
estaAnimavel EmInativacao {timer} = timer > 0

estaAtivo :: Estado -> Bool
estaAtivo Ativo = True
estaAtivo EmInativacao {} = False

atualizarEstado :: Double -> Estado -> (Estado, Bool)
atualizarEstado _ Ativo = (Ativo, False)
atualizarEstado t EmInativacao {timer}
  | timer > 0 = (EmInativacao (timer - t), False)
  | otherwise = (Ativo, True)
