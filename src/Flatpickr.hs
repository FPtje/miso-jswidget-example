{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric            #-}

-- | Miso component that represents the flatpickr jswidget. See
-- https://flatpickr.js.org/
--
-- For more info on components, see
-- https://github.com/FPtje/miso-component-example
module Flatpickr
  ( Model
  , initialModel
  , Interface(..)
  , Action(..)
  , Opts(..)
  , updateModel
  , viewModel
  ) where

-- Imports are explicit for the sake of documentation.
import           Control.Lens ( (.=), (^.), makeLenses, use )
import           Control.Monad ( forM_ )
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import           GHC.Generics ( Generic )
import           GHCJS.Foreign.Callback ( Callback )
import qualified GHCJS.Foreign.Callback as JSCallback
import           GHCJS.Marshal ( ToJSVal(..), fromJSValUnchecked )
import           GHCJS.Types ( IsJSVal, JSVal )
import qualified Miso
import           Miso.Html
import qualified Miso.String as Miso

-- | The model of this component stores a reference to the javascript object
-- created when initialising the date picker. This reference is used to clean
-- up after the DOM element is removed, and to be able to call methods on it
-- during its lifetime (in our case setting the date).
data Model
   = Model
     { _mFlatpickr :: !(Maybe FlatpickrWidget)
     , _mOptions   :: !Opts
     }
     deriving (Eq)

-- | Initial model. The flatpicker reference doesn't exist yet. The
-- lifeCycleKey helps Miso distinguish one Flatpickr from the other, or even
-- another widget.
initialModel :: Opts -> Model
initialModel opts =
    Model
    { _mFlatpickr = Nothing
    , _mOptions   = opts
      -- ^ Options for the widget. See @Opts@.
    }

-- | The interface defines what this component needs from any parent that
-- embeds it. The @action@ parameter refers to the action type of the parent.
data Interface action
   = Interface
     { uniqueId   :: !Miso.MisoString
       -- ^ Unique identifier for this widget, helps Miso distinguish widgets.
     , passAction :: Action -> action
       -- ^ A way to pass @Action@s back to this component.
     , onChanged  :: Time.Day -> action
       -- ^ An action that this component promises to throw when the widget's
       -- date changes.
     , noop       :: !action
       -- ^ A convenience "No Operation" action.
     }

-- | The internal actions
data Action
   = OnCreated
     -- ^ Gets thrown by Miso when the element in which the widget should be
     -- embedded is created (see @viewModel@ below).
   | OnDestroyed
     -- ^ Thrown by Miso when the DOM element is removed from the DOM tree.
   | FlatpickrCreated !FlatpickrWidget
     -- ^ An action we throw when the widget is created. Used to update the
     -- model with a reference to the widget.
   | SetDate !Time.Day
     -- ^ Action that the parent can pass to this component when they change
     -- the date on their side.

-- | Options for the flatpickr widget. Corresponds directly with some of the
-- options described here: https://flatpickr.js.org/options/
-- Other options are left out because they're not needed in this example.
data Opts
   = Opts
     { weekNumbers :: !Bool
     , inline      :: !Bool
     , defaultDate :: !Miso.MisoString
     } deriving (Eq, Generic)

-- We can just derive ToJSVal since the Opts only contains types that can be
-- marshalled to JS. Converting @Opts@ to JSVal will create a javascript
-- object that exactly corresponds to flatpickr's options. With other widgets
-- this may be more complicated. In that case you might want to define the
-- conversion function manually.
instance ToJSVal Opts

-- | The @FlatpickrWidget@ represents the object returned by the flatpickr
-- function.
newtype FlatpickrWidget = FlatpickrWidget JSVal
instance IsJSVal FlatpickrWidget

-- This is the least elegant bit :(
-- The Model requires an Eq instance, but comparing javascript objects isn't
-- trivial. It makes sense to always return @True@, though. The flatpickr
-- widget exists outside of Miso's scope of reason, so for as far as Miso is
-- concerned, we're always talking about the same widget, even when the state
-- of that widget changes.
--
-- Alternative solutions are more than welcome.
instance Eq FlatpickrWidget where
  _ == _ = True

-- Some lenses
makeLenses ''Model

-- | Handles the @Action@s defined above.
updateModel
    :: Interface action
    -> Action
    -> Miso.Transition action Model ()
updateModel iface action = case action of
    OnCreated -> do
      opts <- use mOptions
      Miso.scheduleSub $ \sink -> do
        -- Turn the options into a Javascript value
        jsOpts <- toJSVal opts
        domElement <- getElementById (uniqueId iface)
        -- Call the FFI function to create the widget
        flatpickr <- createWidget domElement jsOpts
        -- Add our events, in this case just one event
        addOnChangeEvent iface sink flatpickr

        -- Throw the FlatpickrCreated, so we can store the widget in our model
        sink $ passAction iface $ FlatpickrCreated flatpickr

    OnDestroyed -> do
      maybeFlatpickr <- use mFlatpickr
      mFlatpickr .= Nothing

      -- Clean up any state that doesn't remove itself
      forM_ maybeFlatpickr $ \flatpickr -> Miso.scheduleIO $ do
        -- All Callbacks created in Haskell should be cleaned up. See the
        -- documentation of the releaseCallback in GHCJS.Foreign.Callback.
        releaseCallbacks flatpickr

        -- Also destroy the widget with its destroy function.
        destroyFlatpickr flatpickr
        pure $ noop iface

    FlatpickrCreated flatpickr ->
      mFlatpickr .= Just flatpickr

    -- The parent can pass this action to this component, indicating the date
    -- has been changed from elsewhere in the application. This handler tells
    -- the widget about the update.
    SetDate date -> do
      maybeFlatpickr <- use mFlatpickr

      forM_ maybeFlatpickr $ \flatpickr -> Miso.scheduleIO $ do
        setDate flatpickr $
          Miso.toMisoString $
          Time.formatTime Time.defaultTimeLocale "%F" date

        pure $ noop iface

-- | View function. Note that it doesn't actually create or destroy the
-- widget. It just adds the @Action@s that get called when the actual DOM
-- elements are created/destroyed.
viewModel :: Interface action -> Model -> Miso.View action
viewModel iface m
    -- flatpickr happens to support both showing a calendar and adding a date
    -- picker to an input field. This example supports both.
    | inline (m ^. mOptions) = viewInline
    | otherwise = viewInput
  where
    viewInline =
        div_ [ ]
        [ -- flatpickr places its widget next to the div below, as opposed to
          -- inside of it, hence the nested div element.

          -- The key here is important! Every node with either `onCreated` or
          -- `onDestroyed` /must/ have a unique key, otherwise the life cycle
          -- events will not always be called.
          nodeHtmlKeyed "div" (Miso.toKey $ uniqueId iface)
            [ -- id is important, since getElementById is used in the update function
              id_ $ uniqueId iface
            , Miso.onCreated $ passAction iface OnCreated
            , Miso.onDestroyed $ passAction iface OnDestroyed
            ]
            []
        ]

    viewInput =
        nodeHtmlKeyed "input" (Miso.toKey $ uniqueId iface)
        [ -- id is important, since getElementById is used in the update function
          id_ $ uniqueId iface
        , type_ "text"
        , Miso.onCreated $ passAction iface OnCreated
        , Miso.onDestroyed $ passAction iface OnDestroyed
        ] []

-- | flatpickr has custom events. See:
-- https://flatpickr.js.org/events/#onchange
--
-- This function creates a callback function and adds it to the widget's list
-- of onChange events. Other widgets will have other ways of registering event
-- listeners, but creating a callback and registering it somewhere should be
-- the same pattern everywhere.
addOnChangeEvent
    :: Interface action
    -> (action -> IO ())
    -> FlatpickrWidget
    -> IO ()
addOnChangeEvent iface sink flatpickr = do
    callback <- JSCallback.asyncCallback3 cbOnChange
    jsAddOnChangeEvent flatpickr callback
  where
    -- This haskell function gets called when the event happens. Flatpickr
    -- gives three arguments: a list of selected dates, a string
    -- representation of the last selected date and the flatpickr widget
    -- itself. The easiest way to get the date was to parse the string.
    cbOnChange :: JSVal -> JSVal -> JSVal -> IO ()
    cbOnChange _selectedDates jsDateStr _flatpickr = do
        dateStr <- fromJSValUnchecked jsDateStr
        day <-
          Time.parseTimeM
            False
            Time.defaultTimeLocale
            "%F" $
            Miso.fromMisoString dateStr

        -- The sink can throw actions back to the Miso world. Throw the
        -- onChanged event defined in the @Interface@ given to us, and our
        -- parent will receive an update without knowing that it came from
        -- Javascript.
        sink $ onChanged iface day

-- Binds flatpickr's widget creating function.
foreign import javascript unsafe "$r = flatpickr($1, $2);"
  createWidget :: JSVal -> JSVal -> IO FlatpickrWidget

-- flatpickr widgets have a setDate function that take several formats of
-- dates, one of them being a "Y-m-d" ("%F" in Haskell terms) formatted
-- string.
foreign import javascript unsafe "$1.setDate($2);"
  setDate :: FlatpickrWidget -> Miso.MisoString -> IO FlatpickrWidget

-- Adds a callback function to the widget's onChange event list.
foreign import javascript unsafe "$1.config.onChange.push($2);"
  jsAddOnChangeEvent :: FlatpickrWidget -> Callback a -> IO ()

-- All callbacks created in Haskell have to be cleaned up. This FFI function
-- is a lazy way to do that. "h$release" is the javascript variant of the
-- releaseCallback function defined in @GHCJS.Foreign.Callback@ in ghcjs-base.
--
-- If you bind more events, or if the widget can contain event callbacks that
-- /aren't/ defined in Haskell, I'd recommend removing them in Haskell using
-- releaseCallback.
foreign import javascript unsafe
  "for (var i in $1.config.onChange) h$release($1.config.onChange[i]);"
  releaseCallbacks :: FlatpickrWidget -> IO ()

-- Destroys the flatpickr widget.
foreign import javascript unsafe "$1.destroy();"
  destroyFlatpickr :: FlatpickrWidget -> IO ()

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: Miso.MisoString -> IO JSVal
