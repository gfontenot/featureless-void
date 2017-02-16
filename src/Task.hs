module Task
    ( taskMain
    ) where

import Options.Applicative

import Import
import Application
import Task.Import

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
    ( importTweets <$> pathArgument )
    ( progDesc "Import tweets into the database" )

pathArgument :: Parser Text
pathArgument = pack <$> argument str
        ( metavar "PATH"
        <> help "Path to input file."
        )
