{-# LANGUAGE OverloadedStrings#-}
module Pages where
import qualified Text.Blaze as B
import Text.Blaze ((!))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html4.Strict as B
import qualified Text.Blaze.Html4.Strict.Attributes as BA
import Control.Monad
import Data.Monoid
import Constants

cssHeader = do
  B.link ! BA.type_ "text/css" ! BA.rel "stylesheet" ! BA.href "style.css"

formatPost :: (Int,B.Html,B.Html) -> B.Html
formatPost (i,t,p) = do
  B.div ! BA.class_ "title" $ B.h1 t
  B.br
  B.div ! BA.class_ "post" $ B.p p
  B.div ! BA.class_ "footer" $ 
      B.a ! BA.href (B.toValue (domain ++ "/remove/?id=" ++ show i)) $ "delete"

inputPage :: B.Html
inputPage = B.html $ do
  B.head .
    B.title $ "New Post"
  B.body 
    (B.form ! BA.enctype "multipart/form-data" ! BA.method "POST" ! BA.action "/send" $ do
      B.label "Title: " >> B.input ! BA.type_ "text" ! BA.name "title" ! BA.size "40"
      B.br
      B.label "Post: "  
        >> B.textarea mempty ! BA.type_ "text" ! BA.name "post" ! BA.cols "40" ! BA.rows "20"
      B.input ! BA.type_ "submit" ! BA.name "upload")

mainPage :: [(Int, B.Html, B.Html)] -> B.Html
mainPage ps = B.html $ do
    B.head $ do
      B.title "The Blog!"
      cssHeader
    B.div ! BA.id "container" $ B.body $ do
      B.div ! BA.id "navigation" $ do
        B.h2 "Menu" 
        B.a ! BA.href (B.toValue (domain ++ "/new")) $ "new"
      B.div ! BA.id "posts" $ mapM_ formatPost (reverse ps)
