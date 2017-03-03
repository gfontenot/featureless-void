module Task
    ( taskMain
    ) where

import Options.Applicative

import Import
import Application
import Task.TweetImport

type Task = Handler ()
type Command m = Mod CommandFields m

taskMain :: IO ()
taskMain = handler =<< parseTask

parseTask :: IO Task
parseTask = execParser $ info (helper <*> taskParser) fullDesc

taskParser :: Parser Task
taskParser = subparser importTweetCommand

importTweetCommand :: Command Task
importTweetCommand = command "import"
    $ info
    ( importTweet <$> pathArgument )
    ( progDesc "Import tweet into the database" )

pathArgument :: Parser FilePath
pathArgument = argument str
        ( metavar "PATH"
        <> help "Path to input file."
        )
