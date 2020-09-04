{- 
   Name: Saad Arshad
   G#:   00857432
   Partner: Sergio Delgado
   G#:   00512529
-}

module Project4 where

import Control.Monad        -- many useful functions
import Control.Concurrent   -- threadDelay, forkIO, MVar..., Chan...
import Data.IORef           -- newIORef, readIORef, writeIORef
import System.Environment   -- getArgs

import System.Random        -- randomRIO
import Debug.Trace

{-
-- download BoundedChan from hackage if you want to use this one.
-- You'll get: BC.BoundedChan, BC.newBoundedChan, BC.readChan, BC.writeChan, etc.

-- import qualified Control.Concurrent.BoundedChan as BC
-}
-----------------------------------------------------------
rounds = 1

type ChairNode = (Int,Bool)   -- our Chair Node that holds the chair number and if its open
type Seats = [MVar ChairNode]      -- List for chair

type Player = (Int,Bool)      -- Player that holds the chair its in and if its still in game
type PlayerList = [Player]    -- List of Players

data KEY = Key deriving (Show,Eq)

-----------------------------------------------------------
main :: IO ()
main = do 
       num <- getArgs               --gets input for players
       let k = (setK num)           -- coverts players into int
       setNumCapabilities (k+1)     -- Sets how many threads can run
       putStr "Game Starting with "
       putStr (show k)
       putStrLn " players"
       locker <- newMVar Key
       gg <- newMVar 1            --MYVar variable for how many threads are dead (Current Players)
       mm <- newMVar True        --MyVar variable for music controll  
       ss <- makeChairs 1 k       --This will hold our chairs r <- (ss!!9) r <- takeMVar print r             
       --rs <- randomRIO(1,10) :: IO Int
       --r <- takeMVar (ss!!rs)
       --print r
       makePlayers 1 k ss mm gg locker --Makes all of the players threads and starts running them with forkIO 
       emcee k 1 mm ss locker          --No need to make an extra emcee thread just use main thread as emcee function
       threadDelay 100000              --Wait till everything is done and then print the end promt
       return ()


emcee 1 _ mm _ lock = return() --Base case for emcee meaning the number of players is 1 and games over
emcee totalP currentP mm (x:xs) lock = do
  success <- withLock lock $ do
                             putStrLn $ "Round "++(show currentP)
  ms <- takeMVar mm      --take Music Mvar
  threadDelay 80000
  putMVar mm False      --Turn music off
  threadDelay 80000     --Wait till the threads find a chair
  ch <- takeMVar x      --Take a chair out of the game
  let cn = geti ch
  putMVar x (cn,False)  --removing a chair
  chairsReset xs        --Reset the rest of the chairs to be open
  emcee (totalP-1) (currentP+1) mm xs lock     --Recall but with 1 less player in the game
  return ()




-- Creates the child threads for the players and runs the threads
makePlayers i k ss mm gg lock = if i <= k                                  --Logic check to make Players from 1 - K (user input of players)
                              then do
                                 pflag <- newMVar (i,True)             --Creates a player flag with number and status
                                 forkIO  $ startPlayers pflag ss mm k lock --Starts running a player thread with its indicading pflag
                                 makePlayers (i+1) k ss mm gg lock        --Recursive Call to keep making threads for the amout of players
                                 return()
                              else return ()                             --Full number of players created

startPlayers pflag _ _ 1 lock = do
                              play <- takeMVar pflag   --Read the Player number and status
                              let pnumber = show (geti play)
                              let pst = getO play
                              if (pst == True)
                                then
                                   do
                                 success <- withLock lock $ do
                                                          putStrLn $ "P"++pnumber++" won!!!!"
                                 return()
                                 else return()
startPlayers pflag ss mm k lock = do 
                                 threadSleep mm           --Put the player to sleep untill the music is off
                                 play <- takeMVar pflag   --Read the Player number and status
                                 let psts = getO play
                                 -- success <- withLock lock $ do
                                  --                           print play
                                 if(psts == True)  --The player is still in the game
                                 then 
                                   do
                                    seat <- findSeat (geti play) ss lock  -- look for a seat
                                    m <- takeMVar mm
                                    putMVar mm True     --Puts the player back to sleep after looking for a seat
                                    threadDelay 70000
                                    (startPlayers seat (tail ss) mm (k-1) lock)
                                 else do
                                       return () 


findSeat pn [] lock = do
                    player <- newMVar (pn,False)
                    success <- withLock lock $ do
                                             putStrLn $ "P"++show pn++" lost"   
                    return player
findSeat pn ss lock = do
                            seat <- takeMVar (head ss)   --Takes a seat from the chairs list
                            let sstatus = getO seat      
                            let scount = geti seat        --gets info from seat
                            if (sstatus == True)          --IF the seat is open
                            then 
                                 do
                                 putMVar (head ss) (scount,False)     --take the seat by setting it to false
                                 threadDelay 10000
                                 player <- newMVar (pn,True)
                                 success <- withLock lock $ do
                                                           putStrLn $ "P"++show pn++" sat in C"++show scount
                                 return player
                            else 
                            	do
                            		putMVar (head ss) (scount,False)    --puts the seat back into the list of MVars
                            		findSeat pn (tail ss) lock

-- Converts into int or default to 10
setK :: [String] -> Int
setK [] = 10              
setK (x:xs) = read x :: Int

-- Resets all of the chairs
chairsReset [] = return ()
chairsReset (x:xs) = do
                     t <- takeMVar x
                     let count = geti t
                     putMVar x (count,True)   --puts the chair MVar to true
                     chairsReset xs


geti (i,bool) = i        --Gets the Index of chair
getO (i,bool) = bool     --Gets the Flag for chair

makeChairs :: Int -> Int -> IO [MVar ChairNode]
makeChairs i k = if i < k          -- A loop to make chairs 1 less than the number of players
        then do 
             chair <- (newMVar (i,True))        --new MVar for chairs
             chairs <- makeChairs (i+1) k       
             return (chair:chairs)              -- return the list that holds all of the chairs
        else return []


--Puts the Thread to sleep untill emcee wakes it up
threadSleep mm = do
               s <- readMVar mm       --Takes the Music MVar
               if(s == False)         --Check if the MVar for music has been turned off from emcee
               then do 
                  return () 
               else do                --If music is still playing then just keep calling it back till its stops
                  threadSleep mm

--Locking function taken from proffesors example withlock
withLock :: MVar KEY -> IO a -> IO a
withLock locker m = do
  k <- takeMVar locker
  ans <- m
  putMVar locker k
  return ans

  