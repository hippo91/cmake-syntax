import Data.Foldable (forM_)
import Data.List (intercalate)
import Language.CMake.AST (Argument (Argument), CommandInvocation (CommandInvocation), File (File), FileElement (CommandElement, NonCommandElement), Literal (Literal), LiteralElem (LiteralString, VariableReference))
import Language.CMake.Parser (fileParser)
import System.Environment (getArgs)
import Text.Trifecta.Parser (parseFromFile)

usage :: String
usage = "Usage: DumpAst <filename>"

parseArgs :: [String] -> (String, String)
parseArgs (_head : _tail) = (filename, error_message)
  where
    error_message = if not $ null _tail then "Unknown argument(s): " ++ intercalate "," _tail ++ ". " ++ usage else ""
    filename = if null _tail then _head else ""
parseArgs _ = ("", "Needs one argument! " ++ usage)

parseFile :: String -> IO ()
parseFile filename = do
  putStrLn ("Parsing file: " ++ filename ++ "...")
  do
    result <- parseFromFile fileParser filename
    forM_ result (\x -> prettyPrint x 0)
  putStrLn "Done."

indentTab :: Int -> String
indentTab x = concat $ replicate x "\t"

class PrettyPrint a where
  prettyPrint :: a -> Int -> IO ()

instance PrettyPrint File where
  prettyPrint (File []) _ = putStrLn "File{}"
  prettyPrint (File (x : xs)) _ = do
    putStrLn "File{"
    prettyPrint x 1
    mapM_ (`prettyPrint` 1) xs
    putStrLn "}"

instance PrettyPrint FileElement where
  prettyPrint (CommandElement x) indent_value = do
    let current_indent = indentTab indent_value
    putStrLn $ current_indent ++ "CommandElement{"
    prettyPrint x $ indent_value + 1
    putStrLn $ current_indent ++ "}"
  prettyPrint NonCommandElement indent_value = putStrLn $ current_indent ++ "NonCommandElement{}"
    where
      current_indent = indentTab indent_value

instance PrettyPrint CommandInvocation where
  prettyPrint (CommandInvocation x y) indent_value = do
    let child_indent_value = indent_value + 1
    let current_indent = indentTab indent_value
    let child_indent = indentTab child_indent_value
    putStrLn $ current_indent ++ "CommandInvocation{"
    putStrLn $ child_indent ++ show x
    mapM_ (`prettyPrint` child_indent_value) y
    putStrLn $ current_indent ++ "}"

instance PrettyPrint Argument where
  prettyPrint (Argument x) indent_value = do
    let current_indent = indentTab indent_value
    putStrLn $ current_indent ++ "Argument{"
    prettyPrint x $ indent_value + 1
    putStrLn $ current_indent ++ "}"

instance PrettyPrint Literal where
  prettyPrint (Literal []) indent_value = putStrLn $ current_indent ++ "Literal{}"
    where
      current_indent = indentTab indent_value
  prettyPrint (Literal (x : xs)) indent_value = do
    let current_indent = indentTab indent_value
    putStrLn $ current_indent ++ "Literal{"
    prettyPrint x $ indent_value + 1
    mapM_ (`prettyPrint` (indent_value + 1)) xs
    putStrLn $ current_indent ++ "}"

instance PrettyPrint LiteralElem where
  prettyPrint (LiteralString x) indent_value = putStrLn $ indentTab indent_value ++ "LiteralString{" ++ show x ++ "}"
  prettyPrint (VariableReference x) indent_value = putStrLn $ indentTab indent_value ++ "VariableReference{" ++ show x ++ "}"

main :: IO ()
main = do
  args <- getArgs
  let (filename, error_message) = parseArgs args
  if error_message /= ""
    then putStrLn error_message
    else do
      parseFile filename
