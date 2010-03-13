
import Graphics.UI.Clutter hiding (show)
import Data.Maybe
import Control.Monad

main = do
  clutterInit

  stg <- stageGetDefault

  r1 <- rectangleNewWithColor (fromJust $ colorFromString "blue")
  r2 <- rectangleNewWithColor (fromJust $ colorFromString "green")
  r3 <- rectangleNewWithColor (fromJust $ colorFromString "red")

  g <- groupNew

  mapM_ (containerAddActor g) [r1, r2, r3]

  containerAddActor stg g


  r5 <- groupGetNthChild g 5
  when (isNothing r5) (putStrLn "Nothing")


  actorShowAll g
  actorShowAll stg

  clutterMain

