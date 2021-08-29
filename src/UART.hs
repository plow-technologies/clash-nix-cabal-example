{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module UART (uart) where
import qualified Clash.Signal as Signal

import Clash.Prelude (BitVector
                     ,Unsigned
                     ,Bool(True,False)
                     ,Bit
                     ,Generic
                     ,Show
                     ,Eq
                     ,NFDataX
                     ,(<*>)
                     ,(<$>)
                     ,(!)
                     ,($)
                     ,(>)
                     ,(&&)
                     ,(==)
                     ,flip
                     ,not
                     ,(<)
                     ,(-))
import qualified Clash.Prelude as Clash
import Control.Lens ((.=)
                    ,(+=)
                    ,(%=))

import qualified Control.Monad as Monad
import Control.Monad.Trans.State
import Data.Generics.Product (field)

-- UART RX Logic
data RxReg
  = RxReg
  { rx_reg        :: BitVector 8
  , rx_data       :: BitVector 8
  , rx_sample_cnt :: Unsigned 4
  , rx_cnt        :: Unsigned 4
  , rx_frame_err  :: Bool
  , rx_over_run   :: Bool
  , rx_empty      :: Bool
  , rx_d1         :: Bit
  , rx_d2         :: Bit
  , rx_busy       :: Bool
  } deriving (Generic,  Eq,Show,NFDataX )



uartRX :: RxReg -> Bit -> Bool -> Bool -> RxReg
uartRX r@(RxReg {rx_d1,
                 rx_d2,
                 rx_cnt,
                 rx_empty,
                 rx_reg,
                 rx_busy,
                 rx_sample_cnt}) rx_in uld_rx_data rx_enable = flip execState r $ do
  -- Synchronize the async signal
  field @"rx_d1" .= rx_in
  field @"rx_d2" .= rx_d1
  -- Uload the rx data
  Monad.when uld_rx_data $ do
    field @"rx_data"  .= rx_reg
    field @"rx_empty" .= True
  -- Receive data only when rx is enabled
  if rx_enable then do
    -- Check if just received start of frame
    Monad.when (not rx_busy && rx_d2 == 0) $ do
      field @"rx_busy"       .= True
      field @"rx_sample_cnt" .= 1
      field @"rx_cnt"        .= 0
    -- Star of frame detected, Proceed with rest of data
    Monad.when rx_busy $ do
      field @"rx_sample_cnt" += 1
      -- Logic to sample at middle of data
      Monad.when (rx_sample_cnt == 7) $ do
        if rx_d1 == 1 && rx_cnt == 0 then
          field @"rx_busy" .= False
        else do
          field @"rx_cnt" += 1
          -- start storing the rx data
          Monad.when (rx_cnt > 0 && rx_cnt < 9) $ do
           field @"rx_reg" %= Clash.replaceBit (rx_cnt - 1) (rx_d2)
          Monad.when (rx_cnt == 9) $ do
            field @"rx_busy" .= False
            -- Check if End of frame received correctly
            if rx_d2 == 0 then
              field @"rx_frame_err" .= True
            else do
              field @"rx_empty"     .= False
              field @"rx_frame_err" .= False
              -- Check if last rx data was not unloaded
              field @"rx_over_run"  .= not rx_empty
  else do
    field @"rx_busy" .= False

-- UART TX Logic
data TxReg
  = TxReg
  { tx_reg      :: BitVector 8
  , tx_empty    :: Bool
  , tx_over_run :: Bool
  , tx_out      :: Bit
  , tx_cnt      :: Unsigned 4
  } deriving (Generic,NFDataX)



uartTX :: TxReg -> Bool -> BitVector 8 -> Bool -> TxReg
uartTX t@(TxReg {tx_empty,
                 tx_reg,                   
                 tx_cnt}) ld_tx_data tx_data tx_enable = flip execState t $ do
  Monad.when ld_tx_data $ do
    if not tx_empty then
      field @"tx_over_run" .= False
    else do
      field @"tx_reg"   .= tx_data
      field @"tx_empty" .= False
  Monad.when (tx_enable && not tx_empty) $ do
    field @"tx_cnt" += 1
    Monad.when (tx_cnt == 0) $
      field @"tx_out" .= 0
    Monad.when (tx_cnt > 0 && tx_cnt < 9) $
      field @"tx_out" .= tx_reg ! (tx_cnt - 1)
    Monad.when (tx_cnt == 9) $ do
      field @"tx_out"   .= 1
      field @"tx_cnt"   .= 0
      field @"tx_empty" .= True
  Monad.unless tx_enable $
    field @"tx_cnt" .= 0

-- Combine RX and TX logic


uart
  :: Clash.HiddenClockResetEnable dom => 
     Clash.Signal dom Bool
     -> Clash.Signal dom (BitVector 8)
     -> Clash.Signal dom Bool
     -> Clash.Signal dom Bit
     -> Clash.Signal dom Bool
     -> Clash.Signal dom Bool
     -> (Clash.Signal dom Bit, Clash.Signal dom Bool,
         Clash.Signal dom (BitVector 8), Clash.Signal dom Bool)
uart ld_tx_data tx_data tx_enable rx_in uld_rx_data rx_enable =
    ( tx_out   <$> txReg
    , tx_empty <$> txReg
    , rx_data  <$> rxReg
    , rx_empty <$> rxReg
    )
  where
    rxReg     = Signal.register rxRegInit (uartRX <$> rxReg <*> rx_in <*> uld_rx_data
                                                  <*> rx_enable)
    rxRegInit = RxReg { rx_reg        = 0
                      , rx_data       = 0
                      , rx_sample_cnt = 0
                      , rx_cnt        = 0
                      , rx_frame_err  = False
                      , rx_over_run   = False
                      , rx_empty      = True
                      , rx_d1         = 1
                      , rx_d2         = 1
                      , rx_busy       = False
                      }

    txReg     = Signal.register txRegInit (uartTX <$> txReg <*> ld_tx_data <*> tx_data
                                                  <*> tx_enable)
    txRegInit = TxReg { tx_reg      = 0
                      , tx_empty    = True
                      , tx_over_run = False
                      , tx_out      = 1
                      , tx_cnt      = 0
                      }
