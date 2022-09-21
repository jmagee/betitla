module Betitla.Sport
( Sport (..)
, pickSportTerm
) where

import           Betitla.Util

data Sport = AlpineSki
           | BackcountrySki
           | Canoeing
           | Crossfit
           | EBikeRide
           | Elliptical
           | Golf
           | Handcycle
           | Hike
           | IceSkate
           | InlineSkate
           | Kayaking
           | Kitesurf
           | NordicSki
           | Ride
           | RockClimbing
           | RollerSki
           | Rowing
           | Run
           | Sail
           | Skateboard
           | Snowboard
           | Snowshoe
           | Soccer
           | StairStepper
           | StandUpPaddling
           | Surfing
           | Swim
           | Velomobile
           | VirtualRide
           | VirtualRun
           | Walk
           | WeightTraining
           | Wheelchair
           | Windsurf
           | Workout
           | Yoga
           deriving (Show)

sportTerms :: Sport -> [String]
sportTerms AlpineSki = ["ski", "mountain ski", "alpine ski"]
sportTerms BackcountrySki = ["ski", "backcountry ski", "cross country ski"]
sportTerms Canoeing = ["canoeing", "boating", "canoe ride"]
sportTerms Crossfit = ["crossfit"]
sportTerms EBikeRide = ["ride", "e-ride"]
sportTerms Elliptical = ["ellipses", "elliptical movement", "elliptical"]
sportTerms Golf = ["golf", "putting", "fake-sport"]
sportTerms Handcycle = ["hand-ride", "handcycle", "handy"]
sportTerms Hike = ["hike", "trek"]
sportTerms IceSkate = ["skate", "ice skate", "skating", "ice skating", "rink-a-dink"]
sportTerms InlineSkate = ["skate", "inline skating", "skating", "roller blading"]
sportTerms Kayaking = ["kayaking", "boating", "kayak ride"]
sportTerms Kitesurf = ["kitesurf", "kite surfing", "surfing with a kite"]
sportTerms NordicSki = ["ski", "nordic ski"]
sportTerms Ride = ["ride", "bicycle ride"]
sportTerms RockClimbing = ["rock climbing", "climbing of the rocks", "mountaineering"]
sportTerms RollerSki = ["rollerski", "ski"]
sportTerms Rowing = ["rowing", "boating"]
sportTerms Run = ["run"]
sportTerms Sail = ["sail", "boat"]
sportTerms Skateboard = ["skate", "skateboard"]
sportTerms Snowboard = ["board", "snowboard"]
sportTerms Snowshoe = ["shoe", "snow shoe"]
sportTerms Soccer = ["football"]
sportTerms StairStepper = ["stair stepper", "stepper", "stairs"]
sportTerms StandUpPaddling = ["paddling", "stand-up paddling"]
sportTerms Surfing = ["surfing"]
sportTerms Swim = ["swim", "swiming"]
sportTerms Velomobile = ["velomobile", "mobiling", "bike car"]
sportTerms VirtualRide = ["virtual ride", "cyber ride", "ride in cyberspace"]
sportTerms VirtualRun = ["virtual run", "cyber run", "run in cyberspace"]
sportTerms Walk = ["walk", "constitutional", "stroll"]
sportTerms WeightTraining = ["weight training", "lifting"]
sportTerms Wheelchair = ["wheelchair", "chairing", "wheeling"]
sportTerms Windsurf = ["wind surf", "surf"]
sportTerms Workout = ["workout", "exercise", "the Jane Fonda"]
sportTerms Yoga = ["yoga", "peace", "joy"]

pickSportTerm :: Sport -> IO String
pickSportTerm = pickAny . sportTerms
