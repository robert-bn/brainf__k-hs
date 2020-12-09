{-#language FlexibleContexts#-}
import Control.Monad.State (MonadState, execStateT, evalStateT, MonadIO, get, modify, liftIO)
import Control.Monad.Loops (whileM_)
import Data.Char (chr, isSpace)
import Data.Word (Word8)
import System.Environment (getArgs)
import Data.Either (partitionEithers)
import Zipper
import Text.ParserCombinators.ReadP hiding (get)

data Op = LEFT            -- <
        | RIGHT           -- > 
        | OUTPUT          -- ,
        | INPUT           -- .
        | ADD Int         -- Adds to the current memory address (+ is equivalent to Add 1, and - to Add 255)
        | ZERO            -- Sets current memory address to 0 (this is equivalent to "[-]" in brainfuck)
        | LOOP [Op]       -- Loop expressions enclosed by []
    deriving (Show, Eq)

type Memory = Zipper Int

parse :: String -> [Op]
parse = fst . last . readP_to_S (many operationP) . filter (flip elem "<>,.+-[]")
    where operationP = choice [ do {char '<'; return LEFT}
                              , do {char ','; return INPUT}
                              , do {char '>'; return RIGHT}
                              , do {char '.'; return OUTPUT}
                              , do {char '+'; return (ADD 1)}
                              , do {char '-'; return (ADD (-1))}
                              , do {char '['; subExpr <- many operationP; char ']'; return (LOOP subExpr)}
                              ]

optimize :: [Op] -> [Op]
optimize []                = []
optimize (ADD n:ADD m:cs)  = optimize (ADD (n+m):cs) -- concatenate adjacent additions to the current memory address 
optimize (LOOP [ADD _]:cs) =  ZERO : optimize cs   -- eliminate loops that exist only to set current memory address to 0, typically written [-]
optimize (LOOP xs:cs)      = LOOP (optimize xs) : optimize cs
optimize (c:cs)            = c : optimize cs

exec :: (MonadState Memory m, MonadIO m) => Op -> m ()
exec LEFT           = modify unsafeLeft
exec RIGHT          = modify unsafeRight
exec (ADD n)        = modify (inplace (+n))
exec ZERO           = modify (replace 0)
exec OUTPUT         = get >>= liftIO . putChar . chr . current
exec INPUT          = liftIO getChar >>= modify . replace . fromEnum
exec (LOOP subExpr) = whileM_ (get >>= return . (/=0) . current) (mapM_ exec subExpr)
    
main :: IO ()
main = do
    f:_ <- getArgs
    bf  <- readFile f
    let program = optimize (parse bf)
    evalStateT (mapM_ exec program) emptyMemory
    return ()
    where
        emptyMemory :: Memory
        emptyMemory = (Zipper (repeat 0) 0 (repeat 0))
