:set -XOverloadedStrings
:set prompt ""

:load Futuretide
import Sound.Tidal.Context

import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

stream <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

-- Replace oAddress with Isaac's computer's IP/.local name
startFuturetide stream (futuretideTarget {oAddress = "127.0.0.1"}) [superdirtShape]

:{
let only = (hush >>)
    p = streamReplace stream
    hush = streamHush stream
    panic = do hush
               once $ sound "superpanic"
    list = streamList stream
    mute = streamMute stream
    unmute = streamUnmute stream
    unmuteAll = streamUnmuteAll stream
    unsoloAll = streamUnsoloAll stream
    solo = streamSolo stream
    unsolo = streamUnsolo stream
    once = streamOnce stream
    first = streamFirst stream
    asap = once
    nudgeAll = streamNudgeAll stream
    all = streamAll stream
    resetCycles = streamResetCycles stream
    setcps = asap . cps
    getcps = streamGetcps stream
    getnow = streamGetnow stream
    xfade i = transition stream True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition stream True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition stream True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition stream True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition stream True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition stream True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition stream True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition stream True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition stream True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition stream True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition stream True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition stream True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition stream True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition stream True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition stream True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition stream True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition stream False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

:{
let getState = streamGet stream
    setI = streamSetI stream
    setF = streamSetF stream
    setS = streamSetS stream
    setR = streamSetR stream
    setB = streamSetB stream
:}

:set prompt "tidal> "
:set prompt-cont ""

-- Test pattern
:{
    d2
      $ n "0*4 0 0 0"
      # s "super808"
      # distort 0.5
      # amp 0.5
      # rate (slow 4 $ sine * 0.3 + 0.1)
      # voice "[1 2 3 4]/4"
:}
