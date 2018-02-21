{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Simple Miso app with some buttons, some text, but most importantly: a
-- component that encapsulates the flatpickr widget. Uses the component pattern
-- described in this example: https://github.com/FPtje/miso-component-example
module Main where

import           Control.Lens ( (^.), (.=), (%=), makeLenses, zoom, use )
import           Data.Monoid ( (<>) )
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import           Data.Time.LocalTime ( LocalTime(..) )
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Format as Time
import qualified Flatpickr
import           Flatpickr ( Interface(..), Opts(..) )
import           Miso ( App(..), Transition )
import qualified Miso
import           Miso.Html
import qualified Miso.String as Miso

data Model
   = Model
     { _mFlatpickr        :: !Flatpickr.Model
       -- ^ The JS widget component.
     , _mFlatpickrVisible :: !Bool
       -- ^ Toggled by the "Toggle calendar visibility" button
     , _mDate             :: !Time.Day
       -- ^ The currently selected date
     }
     deriving ( Eq )

makeLenses ''Model

data Action
  = FlatpickrAction !(Flatpickr.Action Action)
    -- ^ Passes Actions to the Flatpickr widget component.
  | ToggleCalendarVisibility
  | PreviousDay
  | NextDay
  | DateChange !Time.Day
  -- ^ Thrown when the date is changed by the widget.
  | NoOp

main :: IO ()
main = do
    -- Initialise starting date to today
    curTime <- Time.getCurrentTime
    timeZone <- Time.getCurrentTimeZone

    let day :: Time.Day
        (LocalTime day _timeOfDay) = Time.utcToLocalTime timeZone curTime

    Miso.startApp App
      { initialAction = NoOp
      , model         = initialModel day
      , update        = Miso.fromTransition . updateModel
      , view          = viewModel
      , events        = Miso.defaultEvents
      , subs          = []
      , mountPoint    = Nothing
      }

initialModel :: Time.Day -> Model
initialModel day =
    Model
    { _mFlatpickr        = Flatpickr.initialModel
    , _mFlatpickrVisible = True
    , _mDate             = day
    }

updateModel :: Action -> Transition Action Model ()
updateModel action = case action of
    NoOp -> pure ()

    FlatpickrAction act -> do
      date <- use mDate
      zoom mFlatpickr $ Flatpickr.updateModel (flatpickrIface date) act

    ToggleCalendarVisibility ->
      mFlatpickrVisible %= not

    PreviousDay -> do
      mDate %= Time.addDays (-1)
      date <- use mDate

      -- Update the widget with the new date
      zoom mFlatpickr $
        Flatpickr.updateModel (flatpickrIface date) (Flatpickr.SetDate date)


    NextDay -> do
      mDate %= Time.addDays 1
      date <- use mDate

      -- Update the widget with the new date
      zoom mFlatpickr $
        Flatpickr.updateModel (flatpickrIface date) (Flatpickr.SetDate date)

    DateChange day ->
      mDate .= day

viewModel :: Model -> View Action
viewModel m =
    div_ []
      ( viewCalendar m ++
        [ div_[]
          [ button_ [ onClick PreviousDay ] [ text "Previous day" ]
          , button_ [ onClick NextDay     ] [ text "Next day" ]
          ]
        , div_ []
          [ h1_ []
           [ text $ "Selected date: " <> Miso.toMisoString selectedDate
           ]
          ]
        , div_ []
          [ button_
            [ onClick ToggleCalendarVisibility ]
            [ text "Toggle calendar visibility" ]
          ]
        ]
      )
  where
    selectedDate = Time.formatTime Time.defaultTimeLocale "%F" $ m ^. mDate

-- | Show the calendar, but only when it's been set to be visible.
viewCalendar :: Model -> [View Action]
viewCalendar m
    | not (m ^. mFlatpickrVisible) = []
    | otherwise =
      [ Flatpickr.viewModel $ flatpickrIface $ m ^. mDate
      ]

-- | The Flatpickr component needs to know some things about the parent that
-- includes it. With the parent being this module, that information has to be
-- provided here.
flatpickrIface :: Time.Day -> Interface Action
flatpickrIface date =
    Interface
    { uniqueId   = "topLevelCalendar"
    , passAction = FlatpickrAction
    , onChanged  = DateChange
    , noop       = NoOp
    , options    =
        Opts
        { weekNumbers = True
        , inline      = True
        , defaultDate =
            Miso.toMisoString $
            Time.formatTime Time.defaultTimeLocale "%F" date
        }
    }
