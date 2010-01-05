
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
                      (GType.float, Nothing)]

  modelGetNRows lm >>= putDescr "N rows: "
  modelGetNColumns lm >>= putDescr "N cols: "

  putStrLn "Types and Names:"
  print =<< mapM (modelGetColumnType lm) [0,1,2]
  print =<< mapM (modelGetColumnName lm) [0,1,2]


