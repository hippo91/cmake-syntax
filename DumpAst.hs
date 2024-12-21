import System.Environment
import Data.List (intercalate)
import Language.CMake.Parser (fileParser)
import Text.Trifecta.Parser (parseFromFile)
import Data.Foldable (forM_)

usage:: String
usage = "Usage: DumpAst <filename>"

parseArgs:: [String] -> (String, String)
parseArgs (_head:_tail) = (filename, error_message) where
  error_message = if not $ null _tail then "Unknown argument(s): " ++ intercalate "," _tail ++ ". " ++ usage else ""
  filename = if null _tail then _head else ""
parseArgs _ = ("", "Needs one argument! " ++ usage)

parseFile:: String -> IO ()
parseFile filename = do
  putStrLn ("Parsing file: " ++ filename ++ "...")
  do
    result <- parseFromFile fileParser filename
    forM_ result print
  putStrLn "Done."

main :: IO ()
main = do
  args <- getArgs
  let (filename, error_message) = parseArgs args
  if error_message /= "" then
    putStrLn error_message
  else do
    parseFile filename