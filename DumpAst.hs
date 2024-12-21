import System.Environment
import Data.List (intercalate)

usage:: String
usage = "Usage: DumpAst <filename>"

parseArgs:: [String] -> (String, String)
parseArgs (_head:_tail) = (filename, error_message) where
  error_message = if not $ null _tail then "Unknown argument(s): " ++ intercalate "," _tail ++ ". " ++ usage else ""
  filename = if null _tail then _head else ""
parseArgs _ = ("Needs one argument! ", usage)

main :: IO ()
main = do
  args <- getArgs
  let (filename, error_message) = parseArgs args
  if error_message /= "" then
    putStrLn error_message
  else do
    putStrLn ("Parsing file: " ++ filename ++ "...")
    putStrLn "Done."