module Gen(gen) where

import           Control.Monad (when)
import qualified CmdLine as CL
import qualified GenMessage as Message
import qualified GenDomain  as Domain
import qualified GenService as Service
import qualified GenRoute   as Route

gen :: CL.Options -> String -> IO ()
gen opts file = do
    when (CL.verbose opts || CL.debug opts) $ print opts
    let gens =
            [ Message.gen
            , Domain.gen
            , Service.gen
            , Route.gen
            ]
    mapM_ (\ g -> g opts file) gens

