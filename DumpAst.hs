import System.Environment ( getArgs )
import Data.List (intercalate)
import Language.CMake.Parser (fileParser)
import Text.Trifecta.Parser (parseFromFile)
import Data.Foldable (forM_)
import Language.CMake.AST (File (fileElements, File), FileElement (CommandElement, NonCommandElement), CommandInvocation (CommandInvocation), Argument (Argument), Literal (Literal), LiteralElem (LiteralString, VariableReference))

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
    forM_ result (\x -> prettyPrint x 0)
  putStrLn "Done."

indentTab:: Int -> String
indentTab x = concat $ replicate x "\t"

class PrettyPrint a where
  prettyPrint :: a -> Int -> IO()

instance PrettyPrint File where
  prettyPrint (File []) _ = putStrLn "File{}"
  prettyPrint (File (x:xs)) _ = do 
    putStrLn "File{"
    prettyPrint x 1
    mapM_ (\x -> prettyPrint x 1) xs
    putStrLn "}"

instance PrettyPrint FileElement where
  prettyPrint (CommandElement x) indent_value = do
    putStrLn $ indentTab indent_value ++ "CommandElement{"
    prettyPrint x $ indent_value + 1 
    putStrLn $ indentTab indent_value ++ "}"
  prettyPrint NonCommandElement indent_value = putStrLn $ indentTab indent_value ++ "NonCommandElement{}"

instance PrettyPrint CommandInvocation where
  prettyPrint (CommandInvocation x y) indent_value = do
    putStrLn $ indentTab indent_value ++ "CommandInvocation{"
    putStrLn $ indentTab (indent_value + 1) ++ "commandId: " ++ show x
    putStrLn $ indentTab (indent_value + 1) ++ "commandArgs: ["
    mapM_ (\x -> prettyPrint x $ indent_value + 2) y
    putStrLn $ indentTab (indent_value + 1) ++ "]"
    putStrLn $ indentTab indent_value ++ "}"

instance PrettyPrint Argument where
  prettyPrint (Argument x) indent_value = do
    putStrLn $ indentTab indent_value ++ "Argument{"
    prettyPrint x $ indent_value + 1
    putStrLn $ indentTab indent_value ++ "}"

instance PrettyPrint Literal where
  prettyPrint (Literal []) indent_value = putStrLn $ indentTab indent_value ++ "Literal{}"
  prettyPrint (Literal (x:xs)) indent_value = do
    putStrLn $ indentTab indent_value ++ "Literal{"
    prettyPrint x $ indent_value + 1
    mapM_ (\x -> prettyPrint x $ indent_value + 1) xs
    putStrLn $ indentTab indent_value ++ "}"

instance PrettyPrint LiteralElem where
  prettyPrint (LiteralString x) indent_value = putStrLn $ indentTab indent_value ++ "LiteralString{literalString: " ++ show x ++ "}"
  prettyPrint (VariableReference x) indent_value = putStrLn $ indentTab indent_value ++ "VariableReference{variableName: " ++ show x ++ "}"

main :: IO ()
main = do
  args <- getArgs
  let (filename, error_message) = parseArgs args
  if error_message /= "" then
    putStrLn error_message
  else do
    parseFile filename
