{-# LANGUAGE OverloadedStrings #-}

module ShellInterpreter.Interpretation
       ( Interpretation(..)
       , Item(..)
       , Config(..)
       , OptionPrefix(..)
       , OptionSeparator(..)
       , Order(..)
       , make )
       where


-- Imports.

import Prelude hiding (sequence)

import Data.List  (intersperse)
import Data.Maybe (fromJust, isNothing)

import qualified Data.Text as T (Text, concat)


-- Public API.

data Interpretation = Interpretation
    { command :: T.Text
    , items   :: [Item]
    , config  :: Config
    }

data Item
    = Option
        { oKey   :: T.Text
        , oValue :: Maybe T.Text }
    | Argument
        { aKey   :: T.Text }

data Config = Config
    { order     :: Order
    , prefix    :: OptionPrefix
    , separator :: OptionSeparator
    }

data Order = Given | OptionsFirst | ArgumentsFirst

data OptionPrefix = None | SingleDash | DoubleDash

data OptionSeparator = Space | Equality

defaultConfig = Config OptionsFirst DoubleDash Equality

-- Converts the Interpretation option into a text that includes the command with
-- its options and arguments properly formatted and in the correct sequence.

make :: Interpretation -> T.Text
make interpretation = T.concat [command interpretation, " ", formattedItems]
    where sequencedItems = sequence (items interpretation) (order . config $ interpretation)
          formattedItems = T.concat $ intersperse " " (map format' sequencedItems)
          format'        = format (config interpretation)


-- Functions/types for internal use.

-- Sequence the given Interpretation Items according to the given order.
sequence :: [Item] -> Order -> [Item]
sequence items' order'
    | orderIsGiven order'          = items'
    | orderIsOptionsFirst order'   = concat [options, arguments]
    | orderIsArgumentsFirst order' = concat [arguments, options]

    where options   = filter itemIsOption items'
          arguments = filter itemIsArgument items'

-- Format an Item into text, according to the given configuratin.
format :: Config -> Item -> T.Text
format config' item
    | itemIsArgument item = aKey item
    | itemIsOption item   = if (isNothing value') then T.concat [prefix', key']
                                                  else T.concat [prefix', key', separator', "\"", fromJust value', "\""]

    where key'   = oKey item
          value' = oValue item

          prefix'    = getOptionPrefix config'
          separator' = getOptionSeparator config'

-- Get the prefix to prepend to options according to the given configuration.
getOptionPrefix :: Config -> T.Text
getOptionPrefix (Config order' prefix' separator')
    | optionPrefixIsNone       prefix' = ""
    | optionPrefixIsSingleDash prefix' = "-"
    | optionPrefixIsDoubleDash prefix' = "--"

-- Get the separator to insert between options' keys and values, according to
-- the given configuration.
getOptionSeparator :: Config -> T.Text
getOptionSeparator (Config order' prefix' separator')
    | optionSeparatorIsSpace separator'    = " "
    | optionSeparatorIsEquality separator' = "="

-- Type checking functions for Item.

itemIsOption :: Item -> Bool
itemIsOption (Option _ _) = True
itemIsOption _            = False

itemIsArgument :: Item -> Bool
itemIsArgument (Argument _) = True
itemIsArgument _            = False

-- Type checking functions for OptionPrefix.

optionPrefixIsNone :: OptionPrefix -> Bool
optionPrefixIsNone None = True
optionPrefixIsNone _    = False

optionPrefixIsSingleDash :: OptionPrefix -> Bool
optionPrefixIsSingleDash SingleDash = True
optionPrefixIsSingleDash _          = False

optionPrefixIsDoubleDash :: OptionPrefix -> Bool
optionPrefixIsDoubleDash DoubleDash = True
optionPrefixIsDoubleDash _          = False

-- Type checking functions for OptionSeparator.

optionSeparatorIsSpace :: OptionSeparator -> Bool
optionSeparatorIsSpace Space = True
optionSeparatorIsSpace _     = False

optionSeparatorIsEquality :: OptionSeparator -> Bool
optionSeparatorIsEquality Equality = True
optionSeparatorIsEquality _        = False

-- Type checking functions for Order.

orderIsGiven :: Order -> Bool
orderIsGiven Given = True
orderIsGiven _     = False

orderIsOptionsFirst :: Order -> Bool
orderIsOptionsFirst OptionsFirst = True
orderIsOptionsFirst _            = False

orderIsArgumentsFirst :: Order -> Bool
orderIsArgumentsFirst ArgumentsFirst = True
orderIsArgumentsFirst _              = False
