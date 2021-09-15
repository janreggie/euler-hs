# Details because you'll be writing a post about this

- `/src/Solutions/Px.hs` contains `px :: Maybe String -> Integer` which is a solver for Problem `x`.
- `Maybe String` is there for those problems that have a file input and contents of the file are passed there
- `/src/Solver` contains `solver :: Integer -> Maybe String -> Integer` that takes in `x` and returns the appropriate `px`
- `/app/main.hs` should take in arguments from the command line to determine which solver function to use
- Use Options.Applicative to take in flags!!

This is supposed to be fun.
Make it so.
