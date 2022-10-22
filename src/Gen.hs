{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Gen(
            gen
          , getGens
          , gensFromOpts
          ) where

import           System.IO (stderr, hPutStrLn)
import           Control.Monad (when)
import           Data.Maybe (fromMaybe)
import           Prelude         hiding (readFile, writeFile)
import           Text.XML
import qualified CmdLine     as CL
import qualified ModelLoader as ML
import qualified GenMessage  as Message
import qualified GenDomain   as Domain
import qualified GenDao      as Dao
import qualified GenService  as Service
import qualified GenRoute    as Route

gen :: CL.Options -> String -> IO ()
gen opts file = do
    when (CL.verbose opts) $ print opts
    Document prologue root epilogue <- readFile def file
    let model = ML.model root
    print model
    let gens = getGens opts
    if null gens
        then hPutStrLn stderr "no generators enabled."
        else mapM_ (\ g -> g opts file) gens

getGens :: CL.Options -> [CL.Options -> String -> IO ()]
getGens opts = justGens $ removeNothing maybeGens
    where
        justGens      = map (fromMaybe (error "Something bad happend..."))
        removeNothing = filter (\ g -> case g of
            Just _ -> True
            Nothing -> False)
        maybeGens     = map (\ g -> g opts) gensFromOpts

gensFromOpts :: [CL.Options -> Maybe (CL.Options -> String -> IO ())]
gensFromOpts =
    [ \ opts -> if CL.genMessage opts then Just Message.gen else Nothing
    , \ opts -> if CL.genDomain opts then Just Domain.gen else Nothing
    , \ opts -> if CL.genDao opts then Just Dao.gen else Nothing
    , \ opts -> if CL.genService opts then Just Service.gen else Nothing
    , \ opts -> if CL.genRoute opts then Just Route.gen else Nothing
    ]
