{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.NDeep
-- Description :  A further extension to the (|||) layout combinator that offers
--                more precise message handling.
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides @NDeep@, a further extension to @X.L.LayoutCombinators@ own
-- extension of the @Choose@ layout combinator that, like the aforementioned,
-- provides its own version of @(|||)@. It enables more precise message handling
-- by allowing you to send a message to a layout n deep in the chain of @(|||)@,
-- regardless of whether that layout is currently active.
--------------------------------------------------------------------------------

module XMonad.Layout.NDeep (
  -- * Usage
  -- $Usage
  NDeep(..), (|||),
  MessageNDeep(..), to
) where

import XMonad hiding ((<&&>),(|||),(<||>))
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutCombinators (NewSelect(..))

import Control.Monad (guard)
import Data.Maybe (isNothing)

-- $Usage
--
-- Presented in the form of a commented example config.
--
-- N.B. Messages and layouts shown in this example are stand-ins for messages
-- and layouts of your choosing; these ones don't actually exist, so the config
-- won't compile.
--
-- > -- To use this module, first ensure that any import of XMonad,
-- > -- XMonad.Layout or XMonad.Layout.LayoutCombinators hides (|||), is
-- > -- qualified, or uses an explicit import list not including (|||).
-- > import XMonad hiding ((|||))
-- > import XMonad.Layout.LayoutCombinators hiding ((|||))
-- > import XMonad.Layout.NDeep (to,(|||))
-- > import qualified Data.Map.Strict as M
-- >
-- > nDeepKeyBinds :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- > nDeepKeyBinds = \cnf -> M.fromList
-- >   -- Suppose ProduceSideEffects is a message with side effects that are
-- >   -- still visible when the layout producing them is not active.
-- >   [ ((modMask cnf, xK_a), sendMessage . to 0 $ ProduceSideEffect)
-- >   -- Suppose AccidentWaitingToHappen is a message that can be interpreted
-- >   -- by more than one of your layouts, and you don't want to accidentally
-- >   -- send it to the wrong one.
-- >   , ((modMask cnf, xK_b), sendMessage . to 1 $ AccidentWaitingToHappen)
-- >   -- Using a message provided by the LayoutCombinators module with NDeep.
-- >   , ((modMask cnf, xK_c), sendMessage $ JumpToLayout "accidentProne")
-- >   ]
-- >
-- > main :: IO ()
-- > main = xmonad $ def
-- >   { keys = nDeepKeyBinds <+> keys def
-- >   -- Note that no brackets are being used. ||| is right associative,
-- >   -- meaning that (a ||| b ||| c ||| d) is interpreted as
-- >   -- (a ||| (b ||| (c ||| d))). NDeep relies on this linear structure to
-- >   -- function properly without nesting messages. If you were to form a
-- >   -- balanced tree with your layouts by bracketing them as
-- >   -- ((a ||| b) ||| (c ||| d)) instead, then in order to target e.g. the
-- >   -- layout b you'd use (sendMessage . to 0 . to 1 $ BMessage) to first
-- >   -- select the 0-subtree (a ||| b), then the 1-subtree of that; b.
-- >   , layoutHook = sideEffectLayout ||| intendedRecipient ||| possibleAccident
-- >   }

-- | Newtype wrapper for the @NewSelect@ layout combinator.
newtype NDeep l r a = NDeep (NewSelect l r a)
  deriving (Read, Show)

-- | NDeep version of the infix layout combinator @(|||)@, replacing the
--   versions supplied by @XMonad@ and @XMonad.Layout.LayoutCombinators@, which
--   must thus be hidden in imports like e.g.
--
-- > import XMonad hiding ((|||))
--
(|||) :: l a -> r a -> NDeep l r a
la ||| ra = NDeep (NewSelect True la ra)
infixr 5 |||

-- | New @Message@ handled by the @NDeep@ wrapped version of @(|||)@; allows a
--   message to be sent to a specific layout n deep in the chain, whether or
--   not it's current. Zero indexed.
data MessageNDeep = MessageNDeep Int SomeMessage deriving Typeable
instance Message MessageNDeep

-- | A less clumsy interface to the above. Use like e.g.
--
-- > sendMessage . to 0 $ AMessage
--
to :: Message m => Int -> m -> MessageNDeep
to nth = MessageNDeep nth . SomeMessage

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (NDeep l r) a where

  description (NDeep l) = description l

  runLayout (W.Workspace i (NDeep l) mSt) sr =
    runLayout (W.Workspace i l mSt) sr <&&&> NDeep
    where (<&&&>) = flip $ (<$>) . (<$>) . (<$>)

  handleMessage (NDeep ns) m =
    case fromMessage m :: Maybe MessageNDeep of
      Nothing                    -> handleMessage ns  m <&&> NDeep
      Just (MessageNDeep nth sm) -> handleOn nth  ns sm <&&> NDeep
    where
      (<&&>) = flip $ (<$>) . (<$>)
      infixr 2 <&&>

      fmx <||> fmy = fmx >>= \mx -> if isNothing mx then fmy else return mx
      infixr 3 <||>

      handleOn n (NewSelect b l r) sm
        | (n  < 0)  = return Nothing
        | (n == 0)  = handleMessage l sm <||> return (Just l)
                                         <&&> \l' -> NewSelect b l' r
        | otherwise =  handleMessage r (SomeMessage $ MessageNDeep (n-1) sm)
                  <||> handleMessage r sm
                  <||> return (r <$ guard (n == 1))
                  <&&> NewSelect b l
