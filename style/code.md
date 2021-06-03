# Code style guide

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

