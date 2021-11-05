{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}
module Futuretide where
import Sound.Tidal.Core (stack, silence, (#))
-- import Sound.Tidal.Context
import Control.Concurrent
import Control.Monad
import Control.Monad as M
import qualified Sound.Tidal.Pattern as P
-- import Sound.Tidal.Pattern (Value(..))
import Sound.Tidal.Stream
import qualified Sound.Tidal.Tempo as T
import Sound.Tidal.Config
import qualified Sound.OSC.FD as O
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Control.Exception as E

futuretideTarget = Target {oName = "Futuretide",
                           oAddress = "127.0.0.1",
                           oPort = 7770,
                           oBusPort = Nothing,
                           oLatency = 0.1,
                           oWindow = Nothing,
                           oSchedule = Pre MessageStamp, -- crucial to keep OSC from delaying messages via bundle timestamps
                           oHandshake = False
                           }

futuretideDoTick :: Bool -> Stream -> T.State -> IO ()
futuretideDoTick fake stream st =
  E.handle (\ (e :: E.SomeException) -> do
    putStrLn $ "Failed to futuretideDoTick: " ++ show e
           ) $
  do
    tempo <- readMVar (sTempoMV stream)
    pMap <- readMVar (sPMapMV stream)
    sMap <- readMVar (sStateMV stream)
    busses <- readMVar (sBusses stream)
    sGlobalF <- readMVar (sGlobalFMV stream)
    -- putStrLn $ show st
    let config = sConfig stream
        cxs = sCxs stream
        cycleNow = T.timeToCycles tempo $ T.start st -- from tidal, seemingly just used for fake ticks
        -- realCyclesNow = T.timeToCycles tempo $ fst (T.nowTimespan st)
        realCyclesNow = P.start (T.nowArc st)
        patstack = sGlobalF $ playStack pMap
        -- If a 'fake' tick, it'll be aligned with cycle zero
        pat | fake = P.withResultTime (+ cycleNow) patstack
            | otherwise = patstack
        frameEnd = snd $ T.nowTimespan st
        -- add cps to state
        sMap' = Map.insert "_cps" (P.VF (T.cps tempo)) sMap
        --filterOns = filter eventHasOnset
        extraLatency | fake = 0
                     | otherwise = cFrameTimespan config + T.nudged tempo
        -- First the state is used to query the pattern
        futureArc = T.nowArc st + P.Arc 2 2

        es = sortOn (P.start . P.part) $ P.query pat (P.State {P.arc = futureArc,
                                                               P.controls = sMap'
                                                              }
                                                     )
        -- Then it's passed through the events
        (sMap'', es') = P.resolveState sMap' es

        -- TODO onset is calculated in toOSC as well..
        on e tempo'' = (sched tempo'' $ P.start $ P.wholeOrPart e)
        (tes, tempo') = processCps tempo $ es'
    -- putStrLn "NOW AND FUTURE ARCS"
    -- print (fmap fromRational (T.nowArc st) :: P.ArcF Double, fmap fromRational futureArc :: P.ArcF Double)
    -- putStrLn "NOW AND FUTURE TIME"
    -- print (fromRational realCyclesNow :: Double, fromRational (P.start futureArc) :: Double)
    -- print (fromRational realCyclesNow :: Double)
    -- putStrLn "EVENTS"
    -- forM_ es $ \e -> print (fmap fromRational (P.wholeOrPart e) :: P.ArcF Double)
    -- putStrLn "TEMPO EVENTS"
    -- forM_ tes $ \e -> print (fmap fromRational (P.wholeOrPart (snd e)) :: P.ArcF Double)
    forM_ cxs $ \cx@(Cx target _ oscs _ _) -> do
         let latency = oLatency target + extraLatency
             ms = concatMap (\(t, e) ->
                              if (True || fake || (on e t) < frameEnd)
                              then concatMap (toOSC latency busses e t) oscs
                              else []
                          ) tes
         -- print latency
         O.sendMessage (cxUDP cx) (O.message "/time" [O.Double (fromRational realCyclesNow), O.Double (T.cps tempo)])
            `E.catch` (\(e :: E.SomeException) -> return ())
         forM_ ms $ \ m -> send (sListen stream) cx m `E.catch` \ (e :: E.SomeException) -> do
           putStrLn $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e

-- Just clocked from Tempo.hs with the extra threads stripped out
futuretideClocked :: Config -> MVar T.Tempo -> (T.State -> IO ()) -> IO [ThreadId]
futuretideClocked config tempoMV callback
  = do s <- O.time
       let st = T.State {T.ticks = 0,
                         T.start = s,
                         T.nowTimespan = (s, s + frameTimespan),
                         T.nowArc = P.Arc 0 0,
                         T.starting = True
                        }
       clockTid <- forkIO $ loop st
       return [clockTid]
  where frameTimespan :: Double
        frameTimespan = cFrameTimespan config
        loop st =
          do -- putStrLn $ show $ nowArc ts
             tempo <- readMVar tempoMV
             t <- O.time
             let logicalT ticks' = T.start st + fromIntegral ticks' *  frameTimespan
                 logicalNow = logicalT $ T.ticks st + 1
                 -- Wait maximum of two frames
                 delta = min (frameTimespan * 2) (logicalNow - t)
                 e = T.timeToCycles tempo logicalNow
                 s = if T.starting st && T.synched tempo
                     then T.timeToCycles tempo (logicalT $ T.ticks st)
                     else P.stop $ T.nowArc st
             when (t < logicalNow) $ threadDelay (floor $ delta * 1000000)
             t' <- O.time
             let actualTick = floor $ (t' - T.start st) / frameTimespan
                 -- reset ticks if ahead/behind by skipTicks or more
                 ahead = abs (actualTick - T.ticks st) > cSkipTicks config
                 newTick | ahead = actualTick
                         | otherwise = T.ticks st + 1
                 st' = st {T.ticks = newTick,
                           T.nowArc = P.Arc s e,
                           T.nowTimespan = (logicalNow,  logicalNow + frameTimespan),
                           T.starting = not (T.synched tempo)
                          }
             when ahead $ putStrLn $ "skip: " ++ show (actualTick - T.ticks st)
             callback st'
             {-putStrLn ("actual tick: " ++ show actualTick
                       ++ " old tick: " ++ show (ticks st)
                       ++ " new tick: " ++ show newTick
                      )-}
             loop st'


startFuturetide stream target os = do
    remote_addr <- resolve (oAddress target) (show $ oPort target)
    remote_bus_addr <- if isJust $ oBusPort target
                       then Just <$> resolve (oAddress target) (show $ fromJust $ oBusPort target)
                       else return Nothing
    u <- O.openUDP (oAddress target) (oPort target)
    let cxs = [Cx {cxUDP = u, cxAddr = remote_addr, cxBusAddr = remote_bus_addr, cxTarget = target, cxOSCs = os}]
    futuretideClocked (sConfig stream) (sTempoMV stream) $
        futuretideDoTick False (stream {sCxs = cxs, sListen = Nothing})


{-
Revised scheme...

We want to read from "now" to some time in the future.
We want to do this incrementally.

We also want to keep track of this per pattern, so patterns can be replaced without losing their information.

Can tell when a pattern has been replaced by its history length incrementing in the Play Map
Send a "purge pattern future" message when this happens
(and if this is missed, that's ok, the events will go away)

-}
