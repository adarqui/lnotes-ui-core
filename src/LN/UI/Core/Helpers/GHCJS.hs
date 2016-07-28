{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Core.Helpers.GHCJS (
  JSString,
  textToJSString'
) where



import           Data.Text          (Text)
import qualified Data.Text          as Text

#ifdef __GHCJS__
import           Data.JSString      (JSString)
import qualified Data.JSString.Text as JSS (textToJSString)
#else
type JSString = String
#endif



#ifdef __GHCJS__
textToJSString' :: Text -> JSString
textToJSString' = JSS.textToJSString
#else
textToJSString' :: Text -> String
textToJSString' = Text.unpack
#endif
