{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE RankNTypes #-}
module Top where
import UART
import Clash.Prelude
import Control.Lens as Lens (Lens, Lens', over)
import Clash.Intel.ClockGen
import Clash.Signal

createDomain vSystem{vName="StdSystem", vPeriod=hzToPeriod 12e6}
createDomain vSystem{vName="Blink", vPeriod=hzToPeriod 12e6}

{-# ANN topEntity
  (Synthesize
    { t_name     = "blinky"
    , t_inputs   =
        [ PortName "clk"
        , PortName "pmod1_1"
        , PortName "pmod1_2"
        , PortName "pmod1_3"
        , PortName "pmod1_4"
        ]
    , t_output   = PortProduct "out"
        [ PortName "led0"
        , PortName "led1"
        , PortName "led2"
        , PortName "led3"
        , PortName "led4"           
        , PortName "led5"        
        , PortName "led6"
        , PortName "led7"
        ]
    }) #-}



ma :: Num a => a -> (a, a) -> a
ma acc (x,y) = acc + x * y

counterFast :: HiddenClockResetEnable Blink => Signal Blink (Unsigned 23)
counterFast = register   (0 :: Unsigned 23) ((+ 1) <$> counterFast)

counter :: Ord a => Integral a => Eq a => Num a => a -> Unsigned 8
counter c = if c < 6000000
               then 0
               else 0b11111111

leds :: BitPack a => a -> (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
leds c =
  ( c!7
  , c!6 
  , c!5
  , c!4
  , c!3
  , c!2
  , c!1
  , c!0
  )


topEntity ::  Clock Blink -> Reset Blink -> Enable Blink -> Signal Blink (Bit, Bit, Bit,Bit , Bit,Bit , Bit,Bit)
topEntity  clk _ _  =
    exposeClockResetEnable (leds.counter  <$> counterFast) 
                           clk 
                           (unsafeToReset $ pure False) 
                           enableGen  


main :: IO ()
main = print "hello world"
