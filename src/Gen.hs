module Gen(gen) where

import           Control.Monad (when)
import qualified CmdLine as CL
import qualified GenMessage as GM
import qualified GenDomain  as GD
import qualified GenService as GS
import qualified GenRoute   as GR

gen :: CL.Options -> String -> IO ()
gen opts file = do
    when (CL.verbose opts || CL.debug opts) $ print opts
    let gens =
            [ GM.gen
            , GD.gen
            , GS.gen
            , GR.gen
            ]
    mapM_ (\ g -> g opts file) gens

