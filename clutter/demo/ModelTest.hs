
import Graphics.UI.Clutter hiding (Nothing, show)
import qualified System.Glib.GTypeConstants as GType
import qualified Graphics.UI.Clutter.GTypes as CGT


colList :: ColCons Int (ColCons Float EOC)
colList = 4 ## 5 ## EOC
--colList = (4::Int) ## (5::Int) ## EOC

colList2 :: ColCons Int (ColCons Double EOC)
colList2 = 4 ## 5 ## EOC

colList3 :: ColCons (Maybe String) (ColCons Int (ColCons Int EOC))
colList3 = Just "arst" ## 4 ## 9 ## EOC

colList4 :: ColCons (Maybe String) (ColCons Float (ColCons Int EOC))
colList4 = (Just "lol") ## 9002 ## 834 ## EOC


-- should be broken
--colList5 = (Just "lol") ## (4::Int) ## EOC ## (8::Float)

putDescr str x = putStrLn (str ++ show x)


main = do
  clutterInit
  lm <- listModelNew [(GType.int, Just "Bananas"),
                      (GType.double, Just "Monkey coefficient"),
                      (GType.float, Nothing),
                      (GType.string, Nothing)]

  modelGetNRows lm >>= putDescr "N rows: "
  modelGetNColumns lm >>= putDescr "N cols: "

  putStrLn "Types and Names:"
  print =<< mapM (modelGetColumnType lm) [0,1,2,3]
  print =<< mapM (modelGetColumnName lm) [0,1,2,3]

  modelAppend lm ((4 :: Int) ## (2.3 :: Double) ## (4.6 :: Float) ## Just "arst" ## EOC) [0,1,2,3]
  modelGetNRows lm >>= putDescr "N rows: "

  modelAppend lm ((2 :: Int) ## (3.4 :: Double) ## (2.1 :: Float) ## Just "wowomg" ## EOC) [0,1,2,3]
  modelPrepend lm ((9 :: Int) ## (33.33 :: Double) ## (1.12 :: Float) ## Just "aoeu" ## EOC) [0,1,2,3]

  modelInsertValue lm 1 2 (53.2::Float)

  putStrLn "arst"

