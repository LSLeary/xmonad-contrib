-- --< Frontmatter >-- {{{

{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms, MultiWayIf #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Quadrant
-- Description :  The Quadrant Layout
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides the Quadrant layout which takes as parameters four sublayouts with
-- which to tile each quadrant of the screen, and offers a highly configurable
-- set of possible centring and expansion behaviours when not all quadrants are
-- occupied. It is intended to be used with highly automated layouts like Grid
-- in order to bridge a gap between SplitGrid and BSP on the spectrum of
-- layouts trading off automation for window-arranging power.
--------------------------------------------------------------------------------

-- }}}

-- --< Imports; Exports >-- {{{

module XMonad.Layout.Quadrant
  ( -- * Usage
    -- $Usage
    Quadrant(..), AllQuadrants
    -- ** Settings
  , ProfileMap, QSettings(..), InsertAbove(..)
    -- *** Profile Synonyms
    -- $ProfileSynonyms
  , pattern Active, pattern Alternative, pattern Lies
    -- ** Producing Quadrant Values
  , quadrant, allQuadrants, quadGrid
    -- * The QuadrantMessage Interface
    -- $QuadrantMessageInterface
  , QuadrantMessage(..), TargetQ(..)
    -- ** User-Level Messages
    -- *** Window Shifting
  , pushWindow, pushWindowD, swapWindows, swapWindowsD
    -- *** Changing Settings
  , modifyActiveQS, modifyPreferE, modifyExpand, modifyEPreferV
  , modifyInsertAbove, modifyCentre, modifyCPreferV, resetQSettings
  , reorderWith, toggleLtoRTopDown, toggleAltQS, toggleLies
  , transformOrigin, transformCOffset, shiftOrigin, shiftCentred
    -- ** Convenience Functions
  , toggle, set, rotate, cycleIA, toggleProf
    -- * Spatial Types
    -- $SpatialTypes
  , Corner(..), Vertical(..), Horizontal(..), Orientation(..)
    -- ** Corner Synonyms
    -- $CornerSynonyms
  , pattern TL, pattern TR, pattern BL, pattern BR
  ) where

import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing, listToMaybe)
import Data.Function ((&))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (join, void, guard)
import Control.Applicative ((<|>))

import XMonad hiding ((<&&>),(<||>))
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid (Grid(..))

-- }}}

-- --< Usage >-- {{{

-- $Usage
--
-- The Quadrant layout has many settings, and uses a profile system both as an
-- aid to managing them and as a means to implement stateful manipulations on
-- the user side, as demonstrated by the 'toggleLies' message. The settings
-- currently in effect are stored in the 'ProfileMap' under the key 'Active' (or
-- @"active"@); if that location is empty then the name of the workspace is
-- checked before falling back on the global defaults given by the @Default@
-- instance.
--
-- Note that Quadrant can be confusing when you're not used to its behaviour,
-- particularly when centring and expansion of the quadrants—"lies"—obscure
-- which windows belong to which. It's recommended to temporarily set up a
-- keybinding to the @toggleLies@ message mentioned above and use it unsparingly
-- until an intuition is formed.
--
-- Example config:
--
-- > import XMonad
-- > import XMonad.Layout.Quadrant hiding (C,L,R)
-- >
-- > import qualified Data.Map as M
-- >
-- > main :: IO ()
-- > main = xmonad def
-- >   { keys = quadKeys <+> keys def
-- >   , layoutHook = quadGrid { profiles = myQProfiles } ||| Full
-- >   }
-- >
-- > -- A ProfileMap holding defaults for workspaces with tags "1", "2", "3",
-- > -- "F3", etc. Note it does not set an Active value; if it did the workspace
-- > -- defaults would not be loaded until such a time as the `resetQSettings`
-- > -- message were sent.
-- > myQProfiles :: ProfileMap
-- > myQProfiles = M.fromList $ video ++ coding ++
-- >   [ ("1", def { xExpand = True, ePreferV = False, yOrigin = -220 })
-- >   , ("2", def { yExpand = False, xOrigin = 200, yOrigin = -50 })
-- >   , ("8", def { xExpand = True })
-- >   ] where addFs  = ([id, ('F':)] <*>)
-- >           coding = (,) <$> addFs ["5", "6"] <*> pure def { yOrigin =  210 }
-- >           video  = (,) <$> addFs ["3"]      <*> pure def
-- >             { xOrigin = 630, yOrigin = -340, order = [TL,BL,TR,BR] }
-- >
-- > quadKeys :: XConfig Layout -> Data.Map.Map (ButtonMask, KeySym) (X ())
-- > quadKeys conf = M.fromList
-- >   [ ((modm               , xK_l),      sM   toggleLies)
-- >
-- >   -- Reset settings to defaults.
-- >   , ((modm.|.ctrl.|.shift, xK_r),      sM   resetQSettings)
-- >
-- >   -- Shifting the origin.
-- >   , ((modm.|.ctrl.|.shift, xK_h),      sM $ shiftOrigin X (-30))
-- >   , ((modm.|.ctrl.|.shift, xK_j),      sM $ shiftOrigin Y (-30))
-- >   , ((modm.|.ctrl.|.shift, xK_k),      sM $ shiftOrigin Y   30)
-- >   , ((modm.|.ctrl.|.shift, xK_l),      sM $ shiftOrigin X   30)
-- >
-- >   -- Shift horizontally centred windows left or right.
-- >   , ((       ctrl.|.shift, xK_h),      sM $ shiftCentred X (-30))
-- >   , ((       ctrl.|.shift, xK_l),      sM $ shiftCentred X   30)
-- >   -- Reset offset to 0.
-- >   , ((       ctrl.|.shift, xK_r),      sM $ transformCOffset (set (0,0)))
-- >
-- >   -- Toggle expansion.
-- >   , ((       ctrl.|.shift, xK_j),      sM $ modifyExpand toggle X)
-- >   , ((       ctrl.|.shift, xK_k),      sM $ modifyExpand toggle Y)
-- >   -- Toggle centring.
-- >   , ((       ctrl.|.shift, xK_comma),  sM $ modifyCentre toggle X)
-- >   , ((       ctrl.|.shift, xK_period), sM $ modifyCentre toggle Y)
-- >
-- >   -- Shift the focused window to an adjacent quadrant.
-- >   , ((modm               , xK_a),      sM $ ToQuadrantWith rotate)
-- >   , ((modm               , xK_s),      sM   pushWindowD)
-- >   , ((modm               , xK_d),      sM $ pushWindow X)
-- >   , ((modm               , xK_f),      sM $ pushWindow Y)
-- >
-- >   -- Rotate all windows between quadrants.
-- >   , ((modm       .|.shift, xK_a),      sM $ RedistributeWindows rotate)
-- >   -- Swap the contents of the focused quadrant with an adjacent one.
-- >   , ((modm       .|.shift, xK_s),      sM   swapWindowsD)
-- >   , ((modm       .|.shift, xK_d),      sM $ swapWindows X)
-- >   , ((modm       .|.shift, xK_f),      sM $ swapWindows Y)
-- >
-- >   -- Cycle the insertAbove setting through all three options.
-- >   , ((modm,                xK_a),      sM $ modifyInsertAbove cycleIA)
-- >
-- >   -- Toggle the quadrant ordering between the two most obvious choices.
-- >   , ((modm,                xK_s),      sM   toggleLtoRTopDown)
-- >
-- >   -- Set ePreferV to False so horizontal expansion will be preferred.
-- >   , ((modm,                xK_v),      sM $ modifyEPreferV (set False))
-- >
-- >   ] where sM    = sendMessage
-- >           ctrl  = controlMask
-- >           shift = shiftMask
-- >           modm  = modMask conf

-- }}}

-- --< Types >-- {{{

-- --< Spatial >-- {{{

-- $SpatialTypes
-- The @C@, @L@ and @R@ constructors conflict with other constructors common in
-- xmonad.hs source files, so import @hiding@ them if you don't need them.
-- Bidirectional pattern synonyms have been provided so that e.g. @TL@ or @BR@
-- can be used in place of @C T L@ and @C B R@, meaning that none of the @C@,
-- @T@, @B@, @L@ or @R@ constructors are necessary to match on Corners or
-- produce @Corner@ values.

-- | For use as a field in the C data constructor.
data Vertical = T -- ^ Top
              | B -- ^ Bottom
  deriving (Show, Read, Eq)

-- | For use as a field in the C data constructor.
--   Conflicts with Direction2D from XMonad.Util.Types and re-exporters such as
--   XMonad.Actions.Navigation2D.
data Horizontal = L -- ^ Left
                | R -- ^ Right
  deriving (Show, Read, Eq)

-- | For specifying a quadrant of the screen.
--   Conflicts with Side from XMonad.Hooks.ManageHelpers.
data Corner = C Vertical Horizontal
  deriving (Show, Read, Eq)

-- $CornerSynonyms
-- Non-colliding, defactored pattern synonyms for each @Corner@.
pattern TL, TR, BL, BR :: Corner
pattern TL = C T L
pattern TR = C T R
pattern BL = C B L
pattern BR = C B R

-- | For specifying a choice between vertical and horizontal.
data Orientation = X -- ^ Horizontal
                 | Y -- ^ Vertical
  deriving (Show, Read, Eq)

-- }}}

-- --< Quadrant >-- {{{

-- | The Quadrant data type for which the LayoutClass instance is written.
data Quadrant tl tr bl br a = Quadrant
  { internals :: Internal a -- ^ Holds internal state necessary to implement or
                            --   optimise layout operations with minimal quirks.
  , profiles  :: ProfileMap -- ^ Holds various configurations of user settings.
  , tl        :: tl a       -- ^ The layout for the top-left quadrant.
  , tr        :: tr a       -- ^ The layout for the top-right quadrant.
  , bl        :: bl a       -- ^ The layout for the bottom-left quadrant.
  , br        :: br a       -- ^ The layout for the bottom-right quadrant.
  } deriving (Show, Read)

-- | A type synonym simplifying signatures in the case that all quadrants are
--   controlled by layouts of the same type.
type AllQuadrants l = Quadrant l l l l

-- }}}

-- --< QSettings >-- {{{

type ProfileMap = M.Map String QSettings

-- $ProfileSynonyms
-- Pattern synonyms emphasising that the "active", "alternative" and "lies" are
-- special profiles which module-provided messages read from or write to.
pattern Active, Alternative, Lies :: String

-- | The settings in use are stored with the key @Active@, i.e. @"active"@.
pattern Active      = "active"

-- | The settings toggled out with 'toggleAltQS' are stored with the key
--   @Alternative@, i.e. @"alternative"@.
pattern Alternative = "alternative"

-- | The settings toggled out with 'toggleLies' are stored with the key @Lies@,
--   i.e. @"lies"@.
pattern Lies        = "lies"

-- | User facing settings for the Quadrant layout.
data QSettings = QSettings {
  -- | Default: @[TL, TR, BL, BR]@. A linear ordering on the quadrants.
  --   Must contain every 'Corner' exactly once.
  order       :: [Corner],
  -- | Default: 'IfEmpty'. Determines whether or not a window opened at the top
  --   of one quadrant opens into the bottom of the quadrant above (earlier in
  --   the ordering).
  insertAbove :: InsertAbove,
  -- | Default: 'Focused'. The mode used for the passing of unknown Messages to
  --   child layouts.
  passMessTo  :: TargetQ,
  -- | Default: @True@. Determines whether expansion is preferred over centring
  --   when the two conflict.
  preferE     :: Bool,
  -- | Default: @False@. Determines whether or not quadrants will expand out
  --   horizontally when the adjacent quadrant is empty.
  xExpand     :: Bool,
  -- | Default: @True@. Determines whether or not quadrants will expand out
  --   vertically when the adjacent quadrant is empty.
  yExpand     :: Bool,
  -- | Default: @True@. When possible to expand either vertically or
  --   horizontally, prefer the former.
  ePreferV    :: Bool,
  -- | Default: @True@. Centre a quadrant when its horizontal neighbour is
  --   empty.
  xCentre     :: Bool,
  -- | Default: @True@. Centre a quadrant when its vertical neighbour is empty.
  yCentre     :: Bool,
  -- | Default: @False@. When possible to centre either vertically or
  --   horizontally, prefer the former.
  cPreferV    :: Bool,
  -- | Default: @0@. The origin is where the innermost point of each quadrant
  --   meets. This setting determines horizontally where the origin lies
  --   relative to the centre of the screen.
  xOrigin     :: Int,
  -- | Default: @0@. Determines vertically where the origin lies relative to the
  --   centre of the screen.
  yOrigin     :: Int,
  -- | Default: @0@. A horizontal offset given to horizontally centred
  --   quadrants.
  cxOffset    :: Int,
  -- | Default: @0@. A vertical offset given to vertically centred quadrants.
  cyOffset    :: Int
} deriving (Show, Read, Eq)

-- | A data type for specifying where windows should be inserted upon opening at
--   the top of a quadrant.
data InsertAbove
  = Always  -- ^ A window opened at the top of a quadrant always opens into the
            --   quadrant above (earlier in the ordering).
  | IfEmpty -- ^ A window opened at the top of a quadrant will open into the
            --   quadrant above iff that quadrant is empty.
  | Never   -- ^ A window opened in one quadrant will never open into another.
    deriving (Show, Read, Eq)

instance Default QSettings where
  def = QSettings
    { order       = corners
    , insertAbove = IfEmpty
    , passMessTo  = Focused
    , preferE     = True
    , xExpand     = False
    , yExpand     = True
    , ePreferV    = True
    , xCentre     = True
    , yCentre     = True
    , cPreferV    = False
    , xOrigin     = 0
    , yOrigin     = 0
    , cxOffset    = 0
    , cyOffset    = 0
    }

-- }}}

-- --< QuadrantMessage >-- {{{

-- $QuadrantMessageInterface
-- The 'QuadrantMessage' data constructors are chosen to produce a powerful,
-- low-redundancy interface on top of which cuddlier messages can be
-- implemented. Many of them are expected to have direct utility only for power
-- users with very specific goals, hence their omission from the above config.
-- As a replacement, some notes with nods to in-source usage:
--
--  * 'ModifyQSettings' uses information from both active and inactive settings
--    to alter both. It's used directly to implement 'toggleAltQS',
--    'toggleLies' and 'modifyActiveQS', which is then used to implement all
--    other @modify\*@ and the @transform\*@ messages, which themselves are used
--    to implement shift messages.
--
--  * 'WithFocusedQ' allows you to transform another message into one that's
--    contextual upon which quadrant currently holds the focused window, if any.
--    It's used to implement 'swapWindows' and 'swapWindowsD' in terms of
--    'RedistributeWindows' and the 'Focused' case of 'MessageQ' in terms of the
--    'Only' case.
--
--  * 'ToQuadrantWith' shifts the focused window to another quadrant; it's used
--    to implement 'pushWindow' and 'pushWindowD'.
--
--  * 'RedistributeWindows' shuffles the contents of the quadrants between one
--    another. As previously mentioned, it's used to implement 'swapWindows' and
--    'swapWindowsD' with the help of 'WithFocusedQ'.
--
--  * 'MessageQ' offers precise handling of Messages to child layouts. It's used
--    to implement the default handling of unknown Messages, and in its own
--    implementation of the 'Focused' case in terms of the 'Only' case.

-- | Messages for interacting with the Quadrant layout at runtime.
data QuadrantMessage
  -- | A master message for arbitrary modifications to profiles.
  = ModifyQProfiles (ProfileMap -> ProfileMap)
  -- | Maybe send a QuadrantMessage that depends on which quadrant has focus,
  --   if any.
  | WithFocusedQ (Maybe Corner -> Maybe QuadrantMessage)
  -- | Change which quadrant the focused window is tiled in arbitrarily.
  | ToQuadrantWith (Corner -> Corner)
  -- | Redistribute windows amongst quadrants.
  | RedistributeWindows (Corner -> Corner)
  -- | Pass a Message in a 'SomeMessage' wrapper to the layouts in the
  --   targeted quadrants. Provides precise Message handling for child layouts.
  | MessageQ TargetQ SomeMessage
    deriving Typeable

instance Message QuadrantMessage

-- | Target data type for child-layout message handling.
--   Conflicts with All from Data.Monoid and Data.Semigroup.
data TargetQ
  = Focused     -- ^ If there is a quadrant with focus target it, else discard.
  | All         -- ^ Target all quadrants.
  | None        -- ^ Discard the message.
  | Only Corner -- ^ Target a static quadrant.
    deriving (Show, Read, Eq)

-- }}}

-- --< Private >-- {{{

-- | Internal state necessary to implement or optimise layouting operations.
data Internal a = Internal
  { inTL    :: [a]     -- ^ The windows in the top-left quadrant.
  , inTR    :: [a]     -- ^ The windows in the top-right quadrant.
  , inBL    :: [a]     -- ^ The windows in the bottom-left quadrant.
  , inBR    :: [a]     -- ^ The windows in the bottom-right quadrant.
  , managed :: S.Set a -- ^ A set of all managed windows; allows quick lookup
                       --   that reduces the time complexity of the layout from
                       --   O(n^2) to O(n log n).
  } deriving (Show, Read)

instance Default (Internal a) where
  def = Internal def def def def def

-- | Type-safe non-empty lists used in the insertion algorithm.
data NEList a = Last a        -- ^ The last element of an NEList a
              | a :| NEList a -- ^ The cons operation on a and NEList a.
  deriving (Show, Read, Eq)

infixr 8 :|

instance Foldable NEList where
  foldMap f (Last h) = f h
  foldMap f (h:|t)   = f h <+> foldMap f t

-- }}}

-- }}}

-- --< Public Functions >-- {{{

-- --< Quadrant values >-- {{{

-- | A user version of the Quadrant constructor with empty internals passed.
--   Use as, e.g.
--
-- > , layoutHook = quadrant myQProfiles Grid Grid Grid Grid
--
quadrant :: ProfileMap -> tl a -> tr a -> bl a -> br a -> Quadrant tl tr bl br a
quadrant = Quadrant def

-- | Construct an empty @'AllQuadrants' l a@ given profiles and a layout. E.g.
--
-- > , layoutHook = allQuadrants myQProfiles Grid
--
allQuadrants :: ProfileMap -> l a -> AllQuadrants l a
allQuadrants profs l = quadrant profs l l l l

-- | Construct an empty, @'AllQuadrants' 'Grid' a@ with target ratio 3/2 and no
--   profiles. E.g.
--
-- > , layoutHook = quadGrid { profiles = myQProfiles }
--
quadGrid :: AllQuadrants Grid a
quadGrid = allQuadrants M.empty $ GridRatio (3/2)

-- }}}

-- --< Messages: Window Manipulation >-- {{{

-- | Move the focused window to the quadrant adjacent with respect to the
--   supplied 'Orientation'.
pushWindow :: Orientation -> QuadrantMessage
pushWindow = ToQuadrantWith . flipC

-- | Move the focused window to the quadrant diagonally opposite.
pushWindowD :: QuadrantMessage
pushWindowD = ToQuadrantWith flipD

-- | Swap the contents of the focused quadrant with those in the quadrant
--   adjacent with respect to the supplied 'Orientation'.
swapWindows :: Orientation -> QuadrantMessage
swapWindows o = WithFocusedQ . fmap $ \c1 -> RedistributeWindows $ \c2 ->
                if c2 `elem` [c1, flipC o c1] then flipC o c2 else c2

-- | Swap the contents of the focused quadrant with those in the quadrant
--   diagonally opposite.
swapWindowsD :: QuadrantMessage
swapWindowsD = WithFocusedQ . fmap $ \c1 -> RedistributeWindows $ \c2 ->
               if c2 `elem` [c1, flipD c1] then flipD c2 else c2

-- }}}

-- --< Messages: Profile Manipulation >-- {{{

-- | Reset settings to defaults.
resetQSettings :: QuadrantMessage
resetQSettings = ModifyQProfiles (M.delete Active)

-- | Toggle out the 'Active' settings for the 'Alternative' settings. Simply
--   enables the use of two independent configurations.
toggleAltQS :: QuadrantMessage
toggleAltQS = ModifyQProfiles $ toggleProf (\_ alts -> alts) Alternative

-- | If you have any centring or expansion settings on, then overwrite the
--   @Lies@ profile with @Active@ and disable them. Otherwise, read in those
--   settings from @Lies@ and write them to @Active@.
toggleLies :: QuadrantMessage
toggleLies = ModifyQProfiles $ toggleProf togL Lies
  where setLies xE yE xC yC s = s { xExpand = xE, yExpand = yE
                                  , xCentre = xC, yCentre = yC }
        togL s l | xExpand s || yExpand s || xCentre s || yCentre s
                 = (\f a -> f a a a a) setLies False s
                 | otherwise
                 = (setLies <$> xExpand <*> yExpand <*> xCentre <*> yCentre) l s

-- }}}

-- --< Messages: Manipulate Active Settings >-- {{{

-- | Modify the settings currently in effect.
modifyActiveQS :: (QSettings -> QSettings) -> QuadrantMessage
modifyActiveQS f = ModifyQProfiles (`setting` f)

-- | Change the 'preferE' setting.
modifyPreferE :: (Bool -> Bool) -> QuadrantMessage
modifyPreferE f = modifyActiveQS $ \s -> s { preferE = f (preferE s) }

-- | Change the 'xExpand' or 'yExpand' setting depending on the supplied
--   'Orientation'.
modifyExpand :: (Bool -> Bool) -> Orientation -> QuadrantMessage
modifyExpand f X = modifyActiveQS $ \s -> s { xExpand = f (xExpand s) }
modifyExpand f Y = modifyActiveQS $ \s -> s { yExpand = f (yExpand s) }

-- | Change the 'ePreferV' setting.
modifyEPreferV :: (Bool -> Bool) -> QuadrantMessage
modifyEPreferV f = modifyActiveQS $ \s -> s { ePreferV = f (ePreferV s) }

-- | Change the 'xCentre' or 'yCentre' setting depending on the supplied
--   'Orientation'.
modifyCentre :: (Bool -> Bool) -> Orientation -> QuadrantMessage
modifyCentre f X = modifyActiveQS $ \s -> s { xCentre = f (xCentre s) }
modifyCentre f Y = modifyActiveQS $ \s -> s { yCentre = f (yCentre s) }

-- | Change the 'cPreferV' setting.
modifyCPreferV :: (Bool -> Bool) -> QuadrantMessage
modifyCPreferV f = modifyActiveQS $ \s -> s { cPreferV = f (cPreferV s) }

-- | Change the 'insertAbove' setting.
modifyInsertAbove :: (InsertAbove -> InsertAbove) -> QuadrantMessage
modifyInsertAbove f = modifyActiveQS
                    $ \s -> s { insertAbove = f (insertAbove s) }

-- | Change the position of the origin arbitrarily.
transformOrigin :: ((Int, Int) -> (Int, Int)) -> QuadrantMessage
transformOrigin transform = modifyActiveQS $ \s ->
  let (newX, newY) = transform (xOrigin s, yOrigin s)
  in  s { xOrigin = newX, yOrigin = newY }

-- | Shift the origin by the given amount along the given axis.
shiftOrigin :: Orientation -> Int -> QuadrantMessage
shiftOrigin X n = transformOrigin $ \(x, y) -> (x + n, y)
shiftOrigin Y n = transformOrigin $ \(x, y) -> (x, y + n)

-- | Change the centring offset arbitrarily.
transformCOffset :: ((Int, Int) -> (Int, Int)) -> QuadrantMessage
transformCOffset transform = modifyActiveQS $ \s ->
  let (newX, newY) = transform (cxOffset s, cyOffset s)
  in  s { cxOffset = newX, cyOffset = newY }

-- | Shift centred quadrants by the given amount along the given axis.
shiftCentred :: Orientation -> Int -> QuadrantMessage
shiftCentred X n = transformCOffset $ \(x, y) -> (x + n, y)
shiftCentred Y n = transformCOffset $ \(x, y) -> (x, y + n)

-- | Change the linear ordering on the quadrants. If the given function does not
--   produce a valid LO from the active one then no change will be made.
reorderWith :: ([Corner] -> [Corner]) -> QuadrantMessage
reorderWith f = modifyActiveQS $ \s -> s {order = f (order s) }

-- | A Message to toggle the linear ordering between two typical values:
--   left to right then top-down, and top-down then left to right.
toggleLtoRTopDown :: QuadrantMessage
toggleLtoRTopDown = reorderWith (g <$>)
  where g TR = BL
        g BL = TR
        g c   = c

-- }}}

-- --< Convenience functions >-- {{{

-- | An alias of 'not'; use in a modify messages to flip a Bool setting.
toggle :: Bool -> Bool
toggle = not

-- | An alias of 'const'; produces a constant function returning the first
--   argument, and hence can be used in modify and transform messages to set a
--   value.
set :: a -> b -> a
set = const

-- | Rotates a 'Corner' 90 degrees clockwise. For use with 'ToQuadrantWith' and
--   'RedistributeWindows'.
rotate :: Corner -> Corner
rotate (C T L) = C T R
rotate (C T R) = C B R
rotate (C B R) = C B L
rotate (C B L) = C T L

-- | Cycles though the values in the InsertAbove type.
cycleIA :: InsertAbove -> InsertAbove
cycleIA IfEmpty = Always
cycleIA Always  = Never
cycleIA Never   = IfEmpty

-- | Write the Active settings to the given profile, then write combine of the
--   active settings and those that were in the given profile to Active.
--   If the given profile is empty, it's initially treated as a clone of Active.
toggleProf
  :: (QSettings -> QSettings -> QSettings) -> String -> ProfileMap -> ProfileMap
toggleProf combine prof ps =
  let (mAlts, ps') = insertLookup prof acts ps
  in  M.insert Active (acts `combine` fromMaybe acts mAlts) ps'
  where insertLookup = M.insertLookupWithKey (\_ a _ -> a)
        acts = active ps

-- }}}

-- }}}

-- --< Type helper functions >-- {{{

-- | Apply a change to the active settings in the ProfileMap.
setting :: ProfileMap -> (QSettings -> QSettings) -> ProfileMap
setting p f = M.alter (Just . f . fromMaybe def) Active p

-- | Get the active settings from a ProfileMap, assuming incorrectly that the
--   global defaults should be used if there is no Active profile.
active :: ProfileMap -> QSettings
active = M.findWithDefault def Active

-- | Get the active settings from a Quadrant. See comment for `active`.
qSettings :: Quadrant tl tr bl br a -> QSettings
qSettings = active . profiles

-- | Flip a Corner in its Horizontal or Vertical aspect.
flipC :: Orientation -> Corner -> Corner
flipC X (C v L) = C v R
flipC X (C v R) = C v L
flipC Y (C T h) = C B h
flipC Y (C B h) = C T h

-- | Flip a Corner diagonally.
flipD :: Corner -> Corner
flipD = flipC X . flipC Y

-- | A list holding every corner exactly once, and the default LO.
corners :: [Corner]
corners = [TL, TR, BL, BR]

-- | Parameterised version of the inVH accessor functions.
inC :: Quadrant tl tr bl br a -> Corner -> [a]
inC q c = _inC c (internals q)
  where _inC (C T L) = inTL
        _inC (C T R) = inTR
        _inC (C B L) = inBL
        _inC (C B R) = inBR

-- | Replace the windows in the specified quadrant with the supplied list.
updateIn :: Corner -> [a] -> Quadrant tl tr bl br a -> Quadrant tl tr bl br a
updateIn (C T L) l q = q { internals = (internals q) { inTL = l } }
updateIn (C T R) l q = q { internals = (internals q) { inTR = l } }
updateIn (C B L) l q = q { internals = (internals q) { inBL = l } }
updateIn (C B R) l q = q { internals = (internals q) { inBR = l } }

-- | Update the windows  in the specified quadrant with the given function.
updateWith
  :: ([a] -> [a]) -> Corner -> Quadrant tl tr bl br a -> Quadrant tl tr bl br a
updateWith f c q = updateIn c (f $ inC q c) q

-- | Produce an NEList from a head and a tail.
toNEList :: a -> [a] -> NEList a
toNEList h (t:ts) = h :| toNEList t ts
toNEList h    []  = Last h

-- | Safe head for non-empty lists.
sHead :: NEList a -> a
sHead (Last h) = h
sHead (h:|_)   = h

-- }}}

-- --< LayoutClass instance >-- {{{

instance ( LayoutClass tl Window, LayoutClass tr Window
         , LayoutClass bl Window, LayoutClass br Window
         ) => LayoutClass (Quadrant tl tr bl br) Window where

  description q
    | (length . nub) [tld, trd, bld, brd] == 1 = "AllQuadrants (" ++ tld ++ ")"
    | otherwise = "Quadrants (" ++ tld ++ ", " ++ trd ++ ", "
                                ++ bld ++ ", " ++ brd ++ ")"
    where (tld, trd, bld, brd) = ( description (tl q), description (tr q)
                                 , description (bl q), description (br q) )

  -- --< runLayout >-- {{{

  runLayout (W.Workspace i q ms) sr@(Rectangle xco yco wid hei) = do

    (tlWRs, mtl) <- doSubLayout tl TL
    (trWRs, mtr) <- doSubLayout tr TR
    (blWRs, mbl) <- doSubLayout bl BL
    (brWRs, mbr) <- doSubLayout br BR

    return ( tlWRs ++ trWRs ++ blWRs ++ brWRs
           , Just $ q' { tl = fromMaybe (tl q) mtl, tr = fromMaybe (tr q) mtr
                       , bl = fromMaybe (bl q) mbl, br = fromMaybe (br q) mbr
                       , internals = (internals q') { managed = tMSet }
                       } )
    where

      -- Perform layouting for a child layout. Type variables are scoped.
      doSubLayout
        :: LayoutClass l Window
        => (Quadrant tl tr bl br Window -> l Window) -> Corner
        -> X ([(Window, Rectangle)], Maybe (l Window))
      doSubLayout getL c = runLayout (W.Workspace i (getL q) $ toMS c) (rect c)
        where

          -- Produces the Maybe (Stack a)s expected by each sublayout, passing
          -- down correct focus information where it can.
          toMS = tryFocus (W.focus <$> ms) . inC q'

          -- Use the plain Rectangle for a quadrant to produce an expanded or
          -- centred Rectangle as appropriate given the state.
          rect c' = limit sr . expandQ q' sr c' . centreQ q' sr c'
                  $ (Rectangle <$> xOf <*> yOf <*> wOf <*> hOf) c'
            where -- The non-centred, non-expanded Rectangles for each quadrant:

              xOf (C _ L) = xco
              xOf (C _ R) = xco + fi wid `div` 2 + fi (xOrigin s')

              yOf (C T _) = yco
              yOf (C B _) = yco + fi hei `div` 2 - fi (yOrigin s')

              wOf (C _ L) = fi (xOrigin s') + wid `div` 2
              wOf (C _ R) = wid - wOf BL

              hOf (C T _) = - fi (yOrigin s') + hei `div` 2
              hOf (C B _) = hei - hOf TR

      -- The windows in the current stack, integrated into a list.
      toManage = W.integrate' ms

      -- The new managed.
      tMSet = S.fromList toManage

      -- Partially updated Quadrant state.
      q'   -- Insert new windows into the inVH fields.
         = insertNew toManage
           -- Remove unmanaged windows from the inVH fields.
         . delUnmanaged ((managed . internals) q S.\\ tMSet)
           -- Detect a permutation of the stack and apply it.
         . permute tMSet toManage
           -- Bound the origin to well within the confines of the screen.
         $ q {  profiles = ps' }

      -- Profiles with any rogue active settings reined in.
      ps' = let (xO2, yO2) = (bound wid $  xOrigin s, bound hei $  yOrigin s)
                (xC2, yC2) = (bound wid $ cxOffset s, bound hei $ cyOffset s)
            in  profiles q `setting` \_ -> s {  xOrigin = xO2,  yOrigin = yO2
                                             , cxOffset = xC2, cyOffset = yC2
                                             , order = validateLO (order s) }
        where ps = profiles q
              s = fromMaybe def (M.lookup Active ps <|> M.lookup i ps)
              bound dim = fi dim `div` 2 - 50 & \v -> max (-v) . min v
              validateLO cs = if isValidLO cs then cs else corners

      -- Updated QSettings.
      s' = qSettings q'

  -- }}}

  -- --< handleMessage >-- {{{

  handleMessage q m = case fromMessage m :: Maybe QuadrantMessage of
    Nothing -> case fromMessage m :: Maybe LayoutMessages of
      Just ReleaseResources  -> broadcast q m
      Just Hide              -> broadcast q m
      Nothing                -> handleMessage q . SomeMessage
                              $ MessageQ (passMessTo s) m
    Just qm -> case qm of
      ModifyQProfiles f      -> do
        i <- W.currentTag <$> gets windowset
        let ps' = f (profiles q)
            q'  = q { profiles = ps' }
            lo' = order $ fromMaybe def (M.lookup Active ps' <|> M.lookup i ps')
        if | order s == lo' -> return (Just q')
           | isValidLO lo'  -> restackAndReturn (Just q')
           | otherwise      -> return $ Just q' { profiles = profiles q'
                                `setting` \a -> a { order = order s } }
      WithFocusedQ f         -> getFocus q <&&> snd >>= \mc -> case f mc of
        Just (WithFocusedQ _)  -> return Nothing
        Just qm'               -> handleMessage q (SomeMessage qm')
        Nothing                -> return Nothing
      MessageQ (Only TL) sm -> handleMessage (tl q) sm <&&> \l -> q { tl = l }
      MessageQ (Only TR) sm -> handleMessage (tr q) sm <&&> \l -> q { tr = l }
      MessageQ (Only BL) sm -> handleMessage (bl q) sm <&&> \l -> q { bl = l }
      MessageQ (Only BR) sm -> handleMessage (br q) sm <&&> \l -> q { br = l }
      MessageQ All        sm -> broadcast q sm
      MessageQ None       _  -> return Nothing
      MessageQ Focused    sm -> handleMessage q . SomeMessage . WithFocusedQ
                              $ \mc -> MessageQ . Only <$> mc <*> Just sm
      ToQuadrantWith f       -> toQuadrantWith f q >>= restackAndReturn
      RedistributeWindows f  -> restackAndReturn
                              $ (order s ~&~ \c -> updateWith (inC q c++) (f c))
                                (corners ~&~ updateWith (set []) $ q)
                             <$ guard (corners /= (f <$> corners))
      -- Satisfying the almighty exhaustivity checker, wrong though it may be.
      _                      -> return Nothing
    where restackAndReturn mq = whenJust mq restack >> return mq
          s = qSettings q

  -- }}}

-- }}}

-- --< Insertion >-- {{{

-- | Given the new list of windows to manage, insert the new ones into q.
--   Works by building a Map from known windows to (non-empty) lists of new
--   windows to be inserted above them, then passes the job off to insFM.
insertNew :: Ord a => [a] -> Quadrant tl tr bl br a -> Quadrant tl tr bl br a
insertNew tml q = foldr (.) id qUpdates q
  where
    s = qSettings q

    -- List of functions to update q with.
    qUpdates = zipWith updateIn (order s) (qContents intRep')
      where
      -- Gets back the contents of each quadrant from an intRep
      qContents = map catMaybes . splitOn Nothing
      -- Our initial internal representation; wraps all windows in Justs and
      -- concatenates the quadrants, using Nothing as a separator.
      intRep = reverse . intercalate [Nothing]
             $ map (map Just . inC q) (order s)
      -- Post insertion intRep.
      intRep' = insFM (insertAbove s) [] intRep nwMap

    -- Building the aforementioned Map. Any new windows not above a known window
    -- are keyed to Nothing.
    nwMap  = uncurry (mInsert Nothing) (foldl acc ([], M.empty) tml)
      where
        acc (l, wmap) w | w `S.member` (managed . internals $ q)
                        = ([], mInsert (Just w) l wmap)
                        | otherwise = (Just w : l, wmap)
        -- Ignoring empties, convert a list to an NEL and insert it with key k.
        mInsert _    []  = id
        mInsert k (w:ws) = M.insert k (toNEList w ws)

-- | Insert from Map. Given the insertAbove setting, a zipper of the
--   concatenated internal representation split over two variables and the Map
--   in question, traverse the zipper bottom-up inserting new windows.
insFM :: Ord a => InsertAbove -> [Maybe a] -> [Maybe a]
      -> M.Map (Maybe a) (NEList (Maybe a)) -> [Maybe a]

-- If we're on a separator, proceed to the next entry.
insFM ia ls (Nothing:rs) nMap = insFM ia (Nothing:ls) rs nMap

-- If we're on a window before a separator and the window is a key in our Map,
-- pop the head of the NEList in the Map, inserting it above the separator if ia
-- is Always or if it's IfEmpty and there isn't a window above the separator
-- already. In this case we proceed to the newly inserted window. Otherwise, we
-- insert all windows from the NEList below the separator and proceed past it,
-- removing the Map entry.
insFM ia ls (Just w:Nothing:rs) nMap = case Just w `M.lookup` nMap of
  Nothing -> insFM ia (Nothing:Just w:ls) rs nMap
  Just l   | ia == Always || ia == IfEmpty
                          && (isNothing . join . listToMaybe) rs
          -> insFM ia (Nothing:Just w:ls) (sHead l:rs) (popMap l (Just w) nMap)
           | otherwise
          -> insFM ia (Nothing:toRList l++Just w:ls) rs (M.delete (Just w) nMap)

-- If we're on a window not before a separator and it's a key in our Map, insert
-- the new windows after it and remove the Map entry. Either way, proceed.
insFM ia ls (Just w:rs) nMap = case Just w `M.lookup` nMap of
  Nothing -> insFM ia (Just w:ls) rs nMap
  Just l  -> insFM ia (toRList l ++ Just w:ls) rs (M.delete (Just w) nMap)

-- If we run out of windows and separators, we then deal with insertions not
-- associated with a window. We wish to perform such insertions above any
-- existing window, so we search for the topmost one. If we find one then we
-- associate it with the windows to be inserted, focus on it and recurse.
-- Otherwise we perform one insertion into the lowest quadrant, focusing on and
-- associating further insertions with that window instead.
insFM ia ls [] nMap = case Nothing `M.lookup` nMap of
  Nothing -> ls
  Just l  -> case break isJust ls of
    (ns, jw:js) -> insFM ia js (jw:ns) (popMap (jw:|l) Nothing nMap)
    (ns,    []) -> insFM ia [] (sHead l:ns) (popMap l Nothing nMap)

-- | Used in insFM to pop an element off the start of a list in the Map and make
--   it the new key.
popMap :: Ord k => NEList k -> k -> M.Map k (NEList k) -> M.Map k (NEList k)
popMap (w1:|ws) = M.insert w1 ws ... M.delete
popMap      _   = M.delete

-- | toList conversion, reversing in the process.
toRList :: NEList a -> [a]
toRList = foldl (flip (:)) []

-- }}}

-- --< Permuting; deletion >-- {{{

-- | Detects if the window stack has only been permuted. If so, computes and
--   applies the permutation to the windows in Quadrant's internal state.
permute :: Ord a => S.Set a -> [a] -> Quadrant tl tr bl br a
        -> Quadrant tl tr bl br a
permute tmSet tmL q
  | (managed . internals) q == tmSet
  = corners ~&~ updateWith (map replace) $ q
  | otherwise = q
  where replace w = fromMaybe w (M.lookup w permuMap)
        permuMap  = M.fromList . filter (uncurry (/=))
                  $ zip managedL tmL
        managedL = foldMap (inC q) (order $ qSettings q)

-- | Removes any unmanaged windows from internal state.
delUnmanaged
  :: Ord a => S.Set a -> Quadrant tl tr bl br a -> Quadrant tl tr bl br a
delUnmanaged dels = corners ~&~ updateWith (filter (`S.notMember` dels))

-- }}}

-- --< Expansion; centering >-- {{{

-- | Parametrised version of xCentre.
centre :: Orientation -> QSettings -> Bool
centre X = xCentre
centre Y = yCentre

-- | Parametrised version of xExpand.
expand :: Orientation -> QSettings -> Bool
expand X = xExpand
expand Y = yExpand

-- | Given Quadrant state holding settings and the screen Rectangle, perform
--   centring on the Rectangle of the specified quadrant.
centreQ :: Quadrant tl tr bl br a -> Rectangle -> Corner -> Rectangle
        -> Rectangle
centreQ q (Rectangle _ _ xRes yRes) c (Rectangle x y wid hei)
  = Rectangle (x + translation (cen X) (isR c)   (cxOffset s) (xRes - wid))
              (y + translation (cen Y) (isB c) (- cyOffset s) (yRes - hei))
              wid hei
  where
    translation centring rev offset freeSpace
      | centring  = fi offset + (if rev then -1 else 1) * (fi freeSpace `div` 2)
      | otherwise = 0
    cen = doQuadAct centre (not . preferE) expand cPrefer q c
    cPrefer X = not . cPreferV
    cPrefer Y = cPreferV
    isR (C _ h) = (h == R)
    isB (C v _) = (v == B)
    s = qSettings q

-- | Given Quadrant state holding settings and the screen Rectangle, perform
--   expansion on the Rectangle of the specified quadrant.
expandQ :: Quadrant tl tr bl br a -> Rectangle -> Corner -> Rectangle
        -> Rectangle
expandQ q (Rectangle l t xRes yRes) c (Rectangle ql qt wid hei)
  = Rectangle (if ex X then l    else ql)  (if ex Y then t    else qt)
              (if ex X then xRes else wid) (if ex Y then yRes else hei)
  where ex = doQuadAct expand preferE centre ePrefer q c
        ePrefer X = not . ePreferV
        ePrefer Y = ePreferV

-- | The same logic that controls both expansion and centring, factored out.
--   Determines whether or not to perform act1 on the specified quadrant, in the
--   given orientation.
doQuadAct
  :: (Orientation -> QSettings -> Bool)
  -> (               QSettings -> Bool)
  -> (Orientation -> QSettings -> Bool)
  -> (Orientation -> QSettings -> Bool)
  -> Quadrant tl tr bl br a -> Corner -> Orientation -> Bool
doQuadAct act1 pref1 act2 pref2 q c ori
  -- act1 is enabled along ori in q.
  =  act1 ori s
  -- If act2 is enabled along ori in q, act1 better have preference.
  && (act2 ori s ==> pref1 s)
  -- The quadrant we're expanding or centring into better be empty.
  && cNull q (flipC ori c)
  -- If act1 is enabled in the other orientation, then either the diagonally
  -- opposite quadrant better be empty, or this orientation better have
  -- preference.
  && (act1 (flipO ori) s ==> cNull q (flipD c) || pref2 ori s)
  -- If act2 is enabled in the other orientation, then either the diagonally
  -- opposite quadrant better be empty, or act1 better have preference.
  && (act2 (flipO ori) s ==> cNull q (flipD c) || pref1 s)
  where flipO X = Y
        flipO Y = X
        cNull = null ... inC
        s = qSettings q

-- | Given the screen Rectangle, reposition a quadrant's Rectangle to fit
--   within. Ensures centred windows with large offsets remain confined within
--   the screen Rectangle if possible.
limit :: Rectangle -> Rectangle -> Rectangle
limit (Rectangle x0 y0 xRes yRes) (Rectangle x y w h) = Rectangle newx newy w h
  where
    newx = max x0 . (x -) . max 0 $ (x + fi w) - (x0 + fi xRes)
    newy = max y0 . (y -) . max 0 $ (y + fi h) - (y0 + fi yRes)

-- }}}

-- --< Messaging >-- {{{

-- | Stack getter function
getStack :: X (Maybe (W.Stack Window))
getStack = gets $ W.stack . W.workspace . W.current . windowset

-- | Set the window stack in the current workspace to Quadrant's version.
restack :: Quadrant tl tr bl br Window -> X ()
restack q = do
  floating <- gets $ W.floating . windowset
  st <- getStack
  let floats = filter (`M.member` floating) $ W.integrate' st
  setStack (qStack floats $ W.focus <$> st)
  where
    qStack flo mfoc = tryFocus mfoc
                    $ flo ++ foldMap (inC q) (order $ qSettings q)
    setStack ms = modify $ \xst@(XState { windowset = ws })
      -> xst { windowset = ws
               { W.current = (W.current ws)
                 { W.workspace = (W.workspace . W.current $ ws) { W.stack = ms }
             } } }

-- | Maybe given a focus and a list, try to make a correctly focused W.Stack.
tryFocus :: Eq a => Maybe a -> [a] -> Maybe (W.Stack a)
tryFocus _       []  = Nothing
tryFocus mFoc (w:ws) = flip (<|>) (Just $ W.Stack w [] ws) $ do
  foc <- mFoc
  case break (== foc) (w:ws) of
    (up, f:down) -> Just $ W.Stack f (reverse up) down
    _            -> Nothing

-- | If there is one, get the focused window and the quadrant housing it.
getFocus :: Quadrant tl tr bl br Window -> X (Maybe (Window, Corner))
getFocus q = do
  mw <- getStack <&&> W.focus
  let mc = mw >>= \w -> (listToMaybe . filter (elem w . inC q) $ corners)
  return $ (,) <$> mw <*> mc

-- | Maybe move the focused window from its current quadrant to the one f maps
--   it to.
toQuadrantWith
  :: (Corner -> Corner) -> Quadrant tl tr bl br Window
  -> X (Maybe (Quadrant tl tr bl br Window))
toQuadrantWith f q = do
  mwc <- getFocus q
  return $ do
    (w, c) <- mwc
    guard (f c /= c)
    return . updateWith (w:) (f c) . updateWith (filter (w /=)) c $ q

-- | Broadcast a Message to all sublayouts.
broadcast
  :: (LayoutClass tl a, LayoutClass tr a, LayoutClass bl a, LayoutClass br a)
  => Quadrant tl tr bl br a -> SomeMessage
  -> X (Maybe (Quadrant tl tr bl br a))
broadcast q m = do
  mtl' <- handleMessage (tl q) m
  mtr' <- handleMessage (tr q) m
  mbl' <- handleMessage (bl q) m
  mbr' <- handleMessage (br q) m
  return $ q { tl = fromMaybe (tl q) mtl', tr = fromMaybe (tr q) mtr'
             , bl = fromMaybe (bl q) mbl', br = fromMaybe (br q) mbr' }
        <$ (mtl' <||> mtr' <||> mbl' <||> mbr')
  where ma <||> mb = void ma <|> void mb

-- }}}

-- --< Misc >-- {{{

-- | Verify that a [Corner] unambiguously represents a linear ordering.
isValidLO :: [Corner] -> Bool
isValidLO cs = (length . take 5) cs == 4 && (length . nub) cs == 4

-- | Blackbird combinator.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x -> f . g x
infixl 8 ...

-- | Reverse double-fmap for all those troublesome X (Maybe a)s.
(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip $ (<$>) . (<$>)
infixr 4 <&&>

-- | Map over a list and fold down with composition (reversed).
(~&~) :: [a] -> (a -> b -> b) -> (b -> b)
(~&~) = flip $ foldr (.) id ... (<$>)

-- | Alias.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Break xs into the segments not containing x. E.g.
--   lines = splitOn '\n'
--   words = splitOn ' '
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = let (prex, xonwards) = break (==x) xs in
  prex : case xonwards of _:onwards -> splitOn x onwards
                          _         -> []

-- | Implication. Lower precedence than && and ||
(==>) :: Bool -> Bool -> Bool
b1 ==> b2 = not b1 || b2
infixl 1 ==>

-- }}}
