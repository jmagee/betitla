-- | Typeclass that provides conversion to string (like show), but with the intention to be
-- read by a user (unlike show, which ought to output something that can be parsed with read.)

module Betitla.Display
( Display
, display
) where

import           Data.Text (Text)
import           Witch     (from)

-- | A type class for displaying data
-- Like show, but intended to be user derived to have better
-- control of the final string.
class Show a => Display a where
  -- | Display as Text.
  -- The resulting text is intended to be read by a user.
  display :: a -> Text
  display = from . show
