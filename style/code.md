# Code style guide

## High Level Philosophy

The impetus of this project is to get readers up to speed and using haskell in real world contexts. This means shedding some of the established patterns in haskell pedagogy (using `String` instead of `Text`, sticking to `Prelude`, neglecting data structures like `Vector` and `Map` in favour of the familiar `List`). We have a few justifications for this approach:
1. We are filling a gap in the market, we are providing diversity of learning materials.
2. We believe that this approach will provide a swifter education.
3. We would like to give the learner the benefit of the doubt.

Therefore the guiding principle for writing lessons should be: "Teach the reader to use what you would use, not what you think they are capable of understanding"

## Technical Specifics

* The default prompt is `ghci>`. Do not add the imported modules in scope.
* Use _haskell_ code blocks for Haskell code
* Use _console?lang=haskell&prompt=ghci>,ghci|_ code blocks for ghci examples
* Explicitly mention the names of modules and language extensions
used at the beginning of each tutorial
* Ensure that name conflicts, even if unseen in the tutorial, do not occur.
Import data types unqualified unless in cases of name conflict.
For example, to import `Data.Vector`, write: 
```haskell 
import Data.Vector (Vector)
import qualified Data.Vector as V
```
* For terminal commands, _shell_ code blocks must be used, and the prompt should be `$`
* Keep examples simple when possible



