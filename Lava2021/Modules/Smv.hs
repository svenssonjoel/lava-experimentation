module Smv
  ( smv
  )
 where

import Signal
import Netlist
import Generic
import Sequent
import Property
import Error
import LavaConf
import Verification

import Data.List
  ( intersperse
  , nub
  )

import System.IO
  ( openFile
  , IOMode(..)
  , hPutStr
  , hClose
  )

import Compilers.Ghc.IOBuffering
  ( noBuffering
  )

import Data.IORef

import System.Cmd (system)
import System.Exit (ExitCode(..))

----------------------------------------------------------------
-- smv

smv :: Checkable a => a -> IO ProofResult
smv a =
  do checkVerifyDir
     noBuffering
     (props,_) <- properties a
     proveFile defsFile (writeDefinitions defsFile props)
 where
  defsFile = verifyDir ++ "/circuit.smv"

----------------------------------------------------------------
-- definitions

writeDefinitions :: FilePath -> [Signal Bool] -> IO ()
writeDefinitions file props =
  do firstHandle  <- openFile firstFile WriteMode
     secondHandle <- openFile secondFile WriteMode
     var <- newIORef 0

     hPutStr firstHandle $ unlines $
       [ "-- Generated by Lava2000"
       , ""
       , "MODULE main"
       ]

     let new =
           do n <- readIORef var
              let n' = n+1; v = "w" ++ show n'
              n' `seq` writeIORef var n'
              return v

         define v s =
           case s of
             Bool True     -> op0 "1"
             Bool False    -> op0 "0"
             Inv x         -> op1 "!" x

             And []        -> define v (Bool True)
             And [x]       -> op0 x
             And xs        -> opl "&" xs

             Or  []        -> define v (Bool False)
             Or  [x]       -> op0 x
             Or  xs        -> opl "|" xs

             Xor  []       -> define v (Bool False)
             Xor  xs       -> op0 (xor xs)

             VarBool s     -> do op0 s
                                 hPutStr firstHandle ("VAR " ++ s ++ " : boolean;\n")
             DelayBool x y -> delay x y

             _             -> wrong Error.NoArithmetic
           where
            w i = v ++ "_" ++ show i

            op0 s =
              hPutStr secondHandle $
                "DEFINE " ++ v ++ " := " ++ s ++ ";\n"

            op1 op s =
              op0 (op ++ "(" ++ s ++ ")")

            opl op xs =
              op0 (concat (intersperse (" " ++ op ++ " ") xs))

            xor [x]    = x
            xor [x,y]  = "!(" ++ x ++ " <-> " ++ y ++ ")"
            xor (x:xs) = "(" ++ x ++ " & !("
                      ++ concat (intersperse " | " xs)
                      ++ ") | (!" ++ x ++ " & (" ++ xor xs ++ ")))"

            delay x y =
              do hPutStr firstHandle ("VAR " ++ v ++ " : boolean;\n")
                 hPutStr secondHandle $
                      "ASSIGN init(" ++ v ++ ") := " ++ x ++ ";\n"
                   ++ "ASSIGN next(" ++ v ++ ") := " ++ y ++ ";\n"

     outvs <- netlistIO new define (struct props)
     hPutStr secondHandle $ unlines $
       [ "SPEC AG " ++ outv
       | outv <- flatten outvs
       ]

     hClose firstHandle
     hClose secondHandle

     system ("cat " ++ firstFile ++ " " ++ secondFile ++ " > " ++ file)
     system ("rm " ++ firstFile ++ " " ++ secondFile)
     return ()
 where
  firstFile  = file ++ "-1"
  secondFile = file ++ "-2"

----------------------------------------------------------------
-- primitive proving


proveFile :: FilePath -> IO () -> IO ProofResult
proveFile file before =
  do putStr "Smv: "
     before
     putStr "... "
     x <- system ( lavaDirectory
                ++ "/Scripts/smv.wrapper "
                ++ file
                ++ " -showTime"
                 )
     let res = case x of
                 ExitSuccess   -> Valid
                 ExitFailure 1 -> Indeterminate
                 ExitFailure _ -> Falsifiable
     putStrLn (show res ++ ".")
     return res

----------------------------------------------------------------
-- the end.

