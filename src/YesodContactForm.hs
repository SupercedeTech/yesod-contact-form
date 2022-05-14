{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module YesodContactForm where

import ClassyPrelude hiding (Handler)
import qualified Data.Text as T
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/contact ContactR GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data Contact = Contact
  { contactName :: Text
  , contactEmail :: Text
  , contactPhone :: Maybe Text
  , contactOrg :: Maybe Text
  , contactMessage :: Textarea
  }
  deriving Show

contactForm :: Html -> MForm Handler (FormResult Contact, Widget)
contactForm extra = do
  name <- mreq (check validateName textField) "" Nothing
  email <- mreq emailField "" Nothing
  phone <- mopt (check validatePhone textField) "" Nothing
  org <- mopt textField "" Nothing
  message <- mreq textareaField "" Nothing
  let widget =
        [whamlet|
          $newline never
          #{extra}

          <fieldset :isJust (fvErrors (snd name)):.err>
            <label for=#{fvId (snd name)}>Name
            ^{fvInput (snd name)}
            $maybe err <- fvErrors (snd name)
              <p.err>#{err}

          <fieldset :isJust (fvErrors (snd email)):.err>
            <label for=#{fvId (snd email)}>Email
            ^{fvInput (snd email)}
            $maybe err <- fvErrors (snd email)
              <p.err>#{err}

          <fieldset :isJust (fvErrors (snd phone)):.err>
            <label for=#{fvId (snd phone)}>Phone
            ^{fvInput (snd phone)}
            $maybe err <- fvErrors (snd phone)
              <p.err>#{err}

          <fieldset :isJust (fvErrors (snd org)):.err>
            <label for=#{fvId (snd org)}>Organisation
            ^{fvInput (snd org)}
            $maybe err <- fvErrors (snd org)
              <p.err>#{err}

          <fieldset :isJust (fvErrors (snd message)):.err>
            <label for=#{fvId (snd message)}>Message
            ^{fvInput (snd message)}
            $maybe err <- fvErrors (snd message)
              <p.err>#{err}

          <input type=submit value="Send">
        |]
      contact = Contact
        <$> fst name
        <*> fst email
        <*> fst phone
        <*> fst org
        <*> fst message
  pure (contact, widget)

  where

  validateName :: Text -> Either Text Text
  validateName name
    | name == ""    = Left "Name must not be empty"
    | name == "Foo" = Left "Got a Foo"
    | name == "Bar" = Left "Got a Bar"
    | otherwise     = Right name

  validatePhone :: Text -> Either Text Text
  validatePhone phone
    | T.any (\c -> c `notElem` asString "1234567890+ ") phone =
        Left "Phone number contains invalid characters"
    | otherwise = Right phone

getContactR :: Handler Html
getContactR = do
  ((res, form), enctype) <- runFormPost contactForm
  defaultLayout
    [whamlet|
      $newline never
      <form enctype=#{enctype} method=post>
        ^{form}
      #{tshow res}
    |]

postContactR :: Handler Html
postContactR = getContactR

makeFoundation :: IO App
makeFoundation = pure App

main :: IO ()
main = makeFoundation >>= warp 80

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  foundation <- makeFoundation
  app <- toWaiApp foundation
  return (3000, foundation, app)
