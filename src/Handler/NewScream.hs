module Handler.NewScream
    ( getNewScreamR
    , postNewScreamR
    ) where

import Import
import Yesod.Form.Bootstrap3
    ( renderBootstrap3
    , BootstrapFormLayout(..)
    )
import Yesod.Markdown (markdownField)
import Yesod.Form.Functions (FormRender)

screamForm :: AForm Handler Scream
screamForm = Scream
    <$> areq markdownField "Body" Nothing

getNewScreamR :: Handler Html
getNewScreamR = do
    (form, enctype) <- generateForm screamForm
    defaultLayout $ do
        setTitle "New Scream"
        $(widgetFile "screams/new")
    where
        generateForm = generateFormPost . bootstrapForm

postNewScreamR :: Handler Html
postNewScreamR = do
    ((res, form), enctype) <- runForm screamForm
    case res of
      FormSuccess scream -> do
          _screamId <- runDB $ insert scream
          redirect HomeR
      _ -> defaultLayout $(widgetFile "screams/new")
    where
        runForm = runFormPost . bootstrapForm

bootstrapForm :: Monad m => FormRender m a
bootstrapForm = renderBootstrap3 BootstrapBasicForm
