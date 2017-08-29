This file is a literate Haskell file using bird notation

Normal text is ignored by the compiler, allowing us to write more freely than if we just used comments.

However comments are not redundant, as these paragraphs should be before or after function definitions not in the middle. If you want to mention something in the middle of a function definition, use a comment.

In bird notation, only lines beginning with '>' are considered to contain code.

Sometime you may want to write a code example or keep an old version of a function, you can use '<' instead. Since it is not a '>', the compiler ignores it, but the reader can tell that it is meant to be code.

For example here is the first fact function we wrote using if-else expressions. 

< fact x = if x == 0 
<          then 1 
<          else x * fact (x - 1)

Here is the second version using guards:

< fact x | x == 0    = 1
<        | otherwise = x * fact (x - 1)

The final version we wrote used a combination of pattern matching and guards.

We patterned matched against 0 for the base case. The guard was used to ensure that the value that didn't match 0 was valid for the recursive case of factorial. 

> fact :: Int -> Int
> fact 0 = 1                             -- Base case
> fact x | x > 0     = x * fact (x - 1)  -- Recursive case
>        | otherwise = error "Factorial works on nonnegative integers"
 
One final comment about bird notation, is that the code must have a single line between it and any text. Otherwise GHC will report an error.

It is possible to get tools to generate literal Haskell into a PDF, however it usually more work than it is worth (at least to get something worth look at) for things like assignments so we do not expect you do this. 





