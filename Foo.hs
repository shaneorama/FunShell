import System.Process

data MyEnum = Foo|Bar|Baz

instance Show MyEnum where
 show Foo = "__foo"
 show Bar = "__bar"
 show Baz = "__baz"

class Umm x where
 um :: x -> String

instance Umm MyEnum where
 um Foo = "Ummm ... FOO"
 um _ = "Ummm ... ???"

main = do
 putStr "|> "
 command <- getLine
 callCommand command
 main

