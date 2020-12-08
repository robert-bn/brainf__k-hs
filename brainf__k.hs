{-#language FlexibleContexts#-}
{-#language BangPatterns#-}

import Control.Monad.State (MonadState, execStateT, MonadIO, get, modify, liftIO)
import Control.Monad.Loops (untilM_)
import Data.Char (chr, isSpace)
import Data.Word (Word8)
import System.Environment (getArgs)
import Data.Either (partitionEithers)
import Zipper
import Text.ParserCombinators.ReadP hiding (get)

data Op = LEFT          -- <
        | RIGHT         -- > 
        | OUTPUT        -- ,
        | INPUT         -- .
        | ADD Word8     -- Adds to the current memory address (+ is equivalent to Add 1, and - to Add 255)
        | ZERO          -- Sets current memory address to 0 (this is equivalent to "[-]" in brainfuck)
        | LOOP [Op]       -- Loop expressions enclosed by []
    deriving (Show, Eq)

type Memory = Zipper Word8

exec :: (MonadState Memory m, MonadIO m) => [Op] -> m ()
exec [] = return ()
exec (c:cs) = case c of
    LEFT         -> modify unsafeLeft     >> exec cs
    RIGHT        -> modify unsafeRight    >> exec cs 
    ADD n        -> modify (inplace (+n)) >> exec cs
    ZERO         -> modify (replace 0)    >> exec cs
    OUTPUT       -> do
                        m <- get 
                        liftIO . putChar . chr . fromEnum . current $ m
                        exec cs
    INPUT        -> do
                        inputChar <- liftIO getChar
                        modify . replace . toEnum . fromEnum $ inputChar
                        exec cs
    LOOP subExpr -> do
                        m <- get
                        if (current m /= 0)  -- If value at the current data point is 0 jump forward to next ]
                            then untilM_ (exec subExpr) (get >>= return . (==0) . current)
                            else return ()
                        exec cs



parse :: String -> [Op]
parse = fst . last . readP_to_S (many operationP) . filter (flip elem "<>,.+-[]")
    where operationP = choice [ char '<' >> return LEFT
                              , char ',' >> return INPUT
                              , char '>' >> return RIGHT
                              , char '.' >> return OUTPUT
                              , char '+' >> return (ADD 1)
                              , char '-' >> return (ADD 255)
                              , do {char '['; subExpr <- many operationP; char ']'; return (LOOP subExpr)}
                              ]

optimize :: [Op] -> [Op]
optimize [] = []
{-
    remove [-] loops that set the current memory address to 0
    (technically causes a mild inconsistency with the brainfuck
    language, as some loops with
    only + or - in might never terminate, for example the brainfuck
    program "+[-+]" would never terminate, but with this optimization
    it would terminate). Fortunately for any terminating program, this
    should not be an issue.
-}
optimize (ADD n:ADD m:cs) = optimize (ADD (n+m):cs) -- concatenate adjacent additions to the current memory address 
optimize (LOOP xs:cs) = case xs of
                             []      -> optimize cs
                             [ADD _] -> ZERO : optimize cs   -- eliminate loops that exist only to set current memory address to 0, typically written [-]
                             ys      -> LOOP (optimize ys) : optimize cs
optimize (c:cs) = c : optimize cs


evalProgram :: [Op] -> IO ()
evalProgram cs = do
    execStateT (exec cs) initState
    return ()
    where
        initState :: Memory
        initState = Zipper (repeat 0) 0 (repeat 0)

    
main :: IO ()
main = do
    f:_ <- getArgs
    bf  <- readFile f
    let !program = optimize (parse bf)
    evalProgram program
    
