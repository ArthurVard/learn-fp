module ParallelHaskellDigest.Part9 where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

type Account = TVar Int



main = do
    v1 <- atomically $ newTVar "Joe"
    v2 <- atomically $ newTVar "Bob"
    done <- atomically $ newTVar 0
    -- thread A (you can just pretend forkDelayIO == forkIO)
    forkDelayIO . atomically $ do
                              -- transaction log if A runs first
        x <- readTVar v1      -- v1: Joe -> Joe
        y <- readTVar v2      -- v1: Joe -> Joe, v2: Sue -> Sue
        writeTVar v1 "Sue"    -- v1: Joe -> Sue
        writeTVar v2 x        -- v1: Joe -> Sue, v2: Bob -> Joe
        writeTVar v1 y        -- v1: Joe -> Bob, v2: Bob -> Joe
        modifyTVar done (+1)  -- (stm 2.3 but easy to define)
    -- thread B
    forkDelayIO . atomically $ do
                              -- (if A runs first)
        writeTVar v1 "Jean"   -- v1: Bob -> Jean
        writeTVar v2 "Paul"   -- v1: Bob -> Jean, v2: Joe -> Paul
        modifyTVar done (+1)
    waitThreads 2 done
    people <- atomically $ do -- (if A runs first)
        p1 <- readTVar v1     -- v1: Jean -> Jean
        p2 <- readTVar v2     -- v1: Jean -> Jean, v2: Paul -> Paul
        return (p1, p2)
    print people -- if A runs first, (Jean, Paul)
                 -- if B runs first, (Paul, Jean).

-- boring details just for this example
forkDelayIO job = forkIO $
    randomRIO (1, 1000000) >>= threadDelay >> job
waitThreads n v = atomically $
    do { d <- readTVar v;  when (d < n) retry }

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
     bal <- readTVar acc
     writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (- amount)


-- still just a transaction
transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
    deposit to amount
    withdraw from amount

-- now we have an action!
doTransfer :: Account -> Account -> Int -> IO ()
doTransfer from to amount =
    atomically $ transfer from to amount
