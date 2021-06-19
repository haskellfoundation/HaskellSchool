---
version: 1.3.1
title: Containers
---

_Tuples_, _Lists_, _Assoc Lists_, _Sets_, _Maps/Hashmaps_, et _Vectors_. (Les termes techniques ne sont volontairement pas traduits et seront affichés en _italique_). On utilise le terme "conteneur" (_container_) pour parler de manière générale de l'ensemble de ces structures de données.

{% include toc.html %}

## Quand utiliser tel ou tel conteneur ?

Voici une liste de pense-bêtes permettant de choisir le bon conteneur en fonction du contexte.
L'objectif est mnémotechnique et fera surement plus sens après avoir lu les chapitres correspondants.

- Besoin d'associer simplement des données de différents types ? Utilisons un _[Tuple](#tuples)_
- Besoin d'ajouter des éléments au début de la liste (_prepend_) de manière performante ? Utilisons une _[List](#lists)_
- Besoin de garantir l'unicité et l'ordre des éléments ? Utilisons un _[Set](#sets)_
- Besoin de rechercher une valeur à partir d'une clé ? Utilisons une _[Map](#maps)_
- Besoin d'indexer des données de manière performante ? Utilisons un _[Vector](#vectors)_

## Utiliser les modules (_List_, _Set_, _Map_)

Une fonctionnalité appréciable des modules _List_, _Set_ et _Map_ est leur _api_ identique (ou presque). Ainsi, l'utilisation de ces conteneurs est plus intuitive et plus simple à mémoriser, permettant un accès rapide à un large éventail d'outils pour un investissement relativement faible.

La contrepartie est l'exposition à des conflits dans nos imports (_namespace collisions_). C'est une pratique commune et recommandée d'utiliser des imports nommés (_qualified imports_). Si vous suivez ce chapitre avec votre interpréteur `ghci`, nous vous invitons à importer les modules comme décrit ci-dessous, ainsi tous les modules seront correctement disponibles.

```haskell
ghci> import Data.Set as Set
ghci> import Data.Map as Map
ghci> import Data.List.NonEmpty as NE
ghci> :t Map.empty
Map.empty :: Map k a
ghci> :t Set.empty
Set.empty :: Set.Set a
ghci> :t NE.head
NE.head :: NE.NonEmpty a -> a
```

Afin de se familiariser avec les messages d'erreur, en prévision du jour où ils arriveront (et ce jour arrivera :wink:), voici ce qui se produit sans import nommé (_unqualified imports_).

```haskell
ghci> import Data.List
ghci> import Data.Set
ghci> import Data.Map
ghci> lookup

<interactive>:4:1: error:
    Ambiguous occurrence ‘lookup’
    It could refer to
       either ‘Data.Map.lookup’,
              imported from ‘Data.Map’
              (and originally defined in ‘Data.Map.Internal’)
           or ‘Prelude.lookup’,
              imported from ‘Prelude’ (and originally defined in ‘GHC.List’)
ghci> empty

<interactive>:5:1: error:
    Ambiguous occurrence ‘empty’
    It could refer to
       either ‘Data.Map.empty’,
              imported from ‘Data.Map’
              (and originally defined in ‘Data.Map.Internal’)
           or ‘Data.Set.empty’,
              imported from ‘Data.Set’
              (and originally defined in ‘Data.Set.Internal’)
```

## _Tuples_

Les _tuples_ sont la première structure de données que vous allez découvrir en Haskell. C'est un conteneur simple, natif avec une syntaxe concise. Les champs sont référencés selon leurs positions. Théoriquement, Les _tuples_ peuvent contenir un nombre infini de champs, c'est ce qu'on appelle arité (_arity_). En réalité, les spécifications _Haskell Report_ n'imposent aux compilateurs (et interpréteurs) une taille minimale que de 15 champs (_15-tuple_). GHC supporte un nombre de champs allant jusqu'à 62 (_62-tuple_). Le nombre minimum de champs pour un _tuple_ est de 2 (_2-tuple_). C'est sur ce type que nous nous concentrerons étant donné qu'Haskell fournit de nombreuses fonctionnalités par défaut pour celui-ci. A titre d'exemple, voici un _tuple_ avec 8 champs (_8-tuple_).

```haskell
ghci> :t ('0', '1', '2', '3', '4', '5', '6', "8-tuple")
('0', '1', '2', '3', '4', '5', '6', "8-tuple")
  :: (Char, Char, Char, Char, Char, Char, Char, [Char])
```
__Note__: `:t` dans l'interpréteur ghci affiche le type de la valeur donnée. Il affichera le message sous la forme `valeur :: Type`.

### Quand l'utiliser ?

Un _tuple_ est utile pour associer des données, éventuellement hétérogènes : "J'ai besoin d'associer ces éléments". Il n'a pas de valeur *sémantique* (sens), mais se concentre sur la *syntaxe* (structure), c'est pourquoi c'est souvent déconseillé d'en abuser. Si la structure de données à créer est utilisée à plusieurs endroits de l'application, il est préférable d'utiliser un _Record_. Au contraire, si l'utilisation est isolée et locale (interne à une fonction par exemple) alors un _tuple_ peut être approprié !

### Comment le créer ?

Un _tuple_ peut être créé en utilisant les parenthèses avec chaque élément séparé par une virgule `(a, b)`.

```haskell
ghci> myTuple = (True, "hello")
ghci> :t myTuple
myTuple :: (Bool, [Char])
```

On peut également laisser un champ vide, il se transforme alors en fonction. Cette technique est appelée _tuple sectioning_. Elle requiert l'extension `TupleSections`.

```haskell
ghci> :set -XTupleSections
ghci> :t (True,)
(True,) :: t -> (Bool, t)
ghci> :t (,"hello")
(,"hello") :: t -> (t, String)
ghci> :t (,)
(,) :: a -> b -> (a, b)
ghci> (,) True "hello"
(True,"hello")
```
__Note__: Pour activer une extension du langage (_language extension_) depuis l'interpréteur ghci on utilise le mot-clé `:set` qui permet de définir une option, le prefixe `-X` permet de déclarer que c'est une extension du langage qui est directement suivie du nom de l'extension en _pascal case_.

### Comment le manier ?

La principale manière d'utiliser un _tuple_ est en le décomposant. L'approche la plus commune est d'utiliser le _pattern matching_ sur la structure du _tuple_ et d'accéder à son contenu. Cette technique est particulièrement pratique parce qu'elle fonctionne pour tous types de tuples.

```haskell
ghci> (\(a, b) -> not a) myTuple
False
ghci> (\(a, b) -> b <> " world") myTuple
"hello world"

ghci> (\(a, b, c) -> a <> " " <> b <> " " <> c) ("my", "name", "is")
"my name is"
```

Avec un _tuple_ d'arité de 2 (_2-tuple_), les fonctions `fst` et `snd` sont disponibles dans la bibliothèque standard. Ils sont les seuls _tuples_ pour lesquels ces fonctions sont disponibles. Pour les autres, il faudra écrire ses propres fonctions pour accéder aux éléments voulus.

```haskell
ghci> :t fst
fst :: (a, b) -> a
ghci> fst myTuple
True
ghci> :t snd
snd :: (a, b) -> b
ghci> snd myTuple
"hello"
```

### Quelles limitations ?

La principale limitation des _tuples_ en Haskell est que chaque arité correspond à un type disctinct. Il n'y a donc pas de fonction commune pour ajouter un élément à un _tuple_. Ces fonctions doivent donc être écrites _ad hoc_.

Voici un exemple pour augmenter la taille du _tuple_

```haskell
ghci> twoTupleToThreeTuple c (a, b) = (a, b, c)
ghci> twoTupleToThreeTuple () myTuple
(1, "world", ())
```

Tenter d'appeler notre fonction sur un _tuple_ d'arité différente conduira à une erreur de typage; la fonction attend un _2-tuple_ mais on lui a fourni un _3-tuple_.

```haskell
ghci> twoTupleToThreeTuple True (1, 2, 3)

<interactive>:19:27: error:
    • Couldn't match expected type: (a, b)
                  with actual type: (a0, b0, c0)
                  ...
```

## _Lists_

En termes d'utilisation, les _lists_ permettent de résoudre le problème d'extension qu'on a observé avec les _tuples_ (capacité à augmenter la taille du conteneur sans créer une nouvelle instance). Par contre, une _list_ ne peut contenir qu'un unique type de donnée (elle est dite homogène). Les _lists_ sont construites avec une syntaxe spécifique : les crochets, avec des virgules séparant chaque élément.

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
```

### Types inductif/récursif (_Inductive Types_)

Les _lists_ sont la première introduction aux types dit "inductif", une sous-catégorie des types "récursifs". Voici un exemple identique à l'implémentation en Haskell sans le sucre syntaxique.

```haskell
data List a = Nil | Cons a (List a)
```

On peut voir que ce type est récursif : `Nil` est la valeur de base, le constructeur `Cons` y "adjoint" `a` et appelle récursivement `List a`. On peut également constater pourquoi les _lists_ ne peuvent contenir qu'un seul type de données. En effet, la récursion ne porte que sur un type de données `a`. Dans notre définition, on peut remplacer `Nil` par une _list_ vide `[]` et `Cons` par `:` pour retrouver la syntaxe par défaut.

Voici plusieurs exemples équivalents pour illustrer cette mécanique : la création par défaut d'une _list_, la création avec le constructeur `:`, et une création manuelle avec le constructeur `Cons`.

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
ghci> 1 : 2 : 3 : 4 : []
[1,2,3,4]
ghci> Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
```

### Quand l'utiliser ?

Les _Linked Lists_ sont des structures de données très présentes en programmation fonctionnelle, par conséquent vous y serez très souvent confronté en Haskell. C'est en général le premier conteneur auquel on pense. De part, la faible performance de l'ajout d'élément en fin de _list_ (_append_) et la relative faible performance d'accès par index (𝛰(n) avec n l'index), elles sont généralement utilisées lorsque l'on sait que l'on va devoir itérer sur les éléments de celles-ci et que l'ordre des éléments est important.

Un bon exemple d'utilisation des _Linked Lists_ : implémenter une _Stack_ parce que l'ajout et le retrait d'élément se font avec une complexité 𝛰(1) (_Last In First Out_).

Un mauvais exemple d'utilisation des _Linked lists_ : implémenter une _Queue_ parce que l'ajout ou le retrait d'élément se fait avec une complexité 𝛰(n) (_First In First Out_).

Un exemple concret, que l'on rencontre souvent dans le développement d'applications, est la gestion des résultats de requêtes de base de données. Une requête peut retourner soit aucun résultat `[]`, soit un certain nombre de résultats potentiellement ordonnés`[entity..]`. Les bibliothèques d'accès aux base de données ont rarement besoin de garantir un accès par index performant, ainsi elles laissent cette responsabilité à la fonction appelante.

### Concaténation de _lists_

Pour concaténer deux listes (ou plus), on utilise l'opérateur `++` :

```haskell
ghci> [1, 2] ++ [3, 4, 1]
[1, 2, 3, 4, 1]
```

### Les fonctions utilitaires : _Head_ / _Tail_

Lorsque l'on utilise des _lists_, il est commun de travailler avec le premier élément (_head_) et le reste des éléments (_tail_).

Haskell fournit deux fonctions simples : `head` et `tail` :

```haskell
ghci> head ["Orange", "Banana", "Apple"]
"Orange"
ghci> tail ["Orange", "Banana", "Apple"]
["Banana","Apple"]
```

Malheureusement ces fonctions ont un désavantage dans la bibliothèque de base; elles peuvent lever des exceptions même lorsqu'on leur fournit un élément dont le type est correct. La cause de ces exceptions est que ces deux fonctions ne gèrent pas l'ensemble des cas possibles (_partial functions_) : exemple une liste vide.

```haskell
ghci> head []
*** Exception: Prelude.head: empty list
ghci> tail []
*** Exception: Prelude.tail: empty list
```

Pour couvrir ces cas de manière sûre, il est commun en Haskell d'utiliser le type `Maybe`. Il permet de notifier au programme que les cas qui ne sont pas couverts doivent retourner `Nothing`. L'appelant de la fonction retournant un type _maybe_ est alors forcé de gérer les cas où `Nothing` sera retourné mais se prémunit des exceptions lors de l'éxecution du programme (_runtime exceptions_).

```haskell
ghci> :i Maybe
data Maybe a = Nothing | Just a     -- Defined in ‘GHC.Maybe’
...
```
__Note__: `:i` dans l'interpréteur ghci donne des informations à propos du type, la première ligne est son implémentation.

On peut à présent définir des fonctions _head_ et _tail_ sûres, prenant en compte tous les cas possibles sans exception, (_total functions_) en utilisant le _pattern matching_ !

```haskell
ghci> :{
| safeHead :: [a] -> Maybe a
| safeHead [] = Nothing
| safeHead (x:xs) = Just x
|
| safeTail :: [a] -> Maybe [a]
| safeTail [] = Nothing
| safeTail (x:xs) = Just xs
| :}
ghci> safeHead ["Orange", "Banana", "Apple"]
Just "Orange"
ghci> safeHead []
Nothing
ghci> safeTail ["Orange", "Banana", "Apple"]
Just ["Banana","Apple"]
ghci> safeTail []
Nothing
```
__Note__: `:{` et `:}` permettent d'écrire des définitions sur plusieurs lignes dans l'interpréteur ghci.

Youpi! Plus d'exception.

Une autre manière de s'assurer que `head` et `tail` sont sûres est d'utiliser les _NonEmpty Lists_.

```haskell
ghci> import Data.List.NonEmpty
ghci> :i NonEmpty
data NonEmpty a = a :| [a]
```

En observant la définition de `NonEmpty`, on s'aperçoit qu'elle oblige la présence d'un premier élément. Le symbole `:|` est un constructeur comme `:`, dans les faits la définition de `NonEmpty` est identique à la définition des _lists_ vues précédemment, à la nuance prêt qu'elle omet le cas `[]` (_Nil_).

Cette manière aborde le problème avec une approche opposée à la solution précédente avec `Maybe`. Plutôt que de forcer l'appelant à se prémunir des cas manquants lorsqu'il reçoit le résultat, elle force l'appelant à construire sa liste avec des données valides (lorsqu'il appelle la fonction).

```haskell
ghci> import Data.List.NonEmpty as NE
ghci> :t NE.head
NE.head :: NonEmpty a -> a
ghci> head (1 :| [])
1
ghci> NE.head []
<interactive>:11:9: error:
    • Couldn't match expected type: NonEmpty a
                  with actual type: [a]
    ...
```

Il faut noter que cette erreur n'est pas une erreur d'exécution (_runtime error_) mais une erreur de typage (_type error_), le compileur ghc nous prévient que l'on tente d'utiliser une liste potentiellement vide au lieu du type de liste requis `NonEmpty`.

### Considération sur les performances

Haskell implémente les listes comme des _Linked Lists_. Le constructeur `:` (appelé _cons_ abbrégé de _constructor_) sert de lien (_link_) entre les éléments de la liste. Ce choix a pour conséquences que certaines opérations soient rapides et d'autres lentes :

Ajouter un élément au début d'une liste est facile et rapide. Il suffit de "lier" un nouvel élément à la liste existante avec le constructeur `:`.

```haskell
prepend value list = value : list
```

Par contre, étant donné que la liste peut être vide (`[]` ou `Nil`) ou qu'un élément peut être lié au reste de la liste (comme on l'a vu précédemment), celle-ci ne comporte pas d'information sur sa taille ou de référence vers la fin de la liste.

Par conséquent, pour récupérer la longueur d'une liste on doit itérer sur l'ensemble de celle-ci pour compter le nombre d'éléments. Pour trouver un élément à un index spécifique, on doit traverser la liste jusqu'à l'atteindre.

De même, pour ajouter un élément à la fin d'une liste existante on doit ajouter un constructeur à la fin de celle-ci et "lier" l'élément :

```haskell
append originalList newList =
    case originalList of
        [] -> newList
        x : xs -> x : append xs newList
```

La fonction `append` definit ci-dessus est identique à l'opérateur `++`, il faut donc être prudent lorsque l'on veut ajouter un élément en fin de liste. Une concaténation avec `++` dans une boucle donne une compléxité de 𝛰(n²) !

En raison de l'implémentation avec des _Linked List_, la majeure partie des opérations sur les listes ont une complexité linéaire en temps (_linear time complexity_, `𝛰(n)`). Dans la plupart des cas, ces opérations sont plus lentes qu'avec d'autres conteneurs, une bonne raison d'apprendre à connaître chacun avec leurs avantages et leurs inconvénients !

## _Assoc(iation) Lists_

Jusqu'à maintenant on a vu l'accès aux valeurs d'une _list_ à partir de ses indexes ou aux valeurs d'un _tuple_ par _pattern matching_. Cependant, un des cas d'utilisation les plus répandus dans l'utilisation des conteneurs est le stockage de paires clés-valeurs (_Key-Value_). Les _Assoc(iation) Lists_ (_assoc_ dans la litérature) permettent ce stockage en combinant _list_ et _tuple_ (_2-tuple_). Etant donné que ces structures de données ne sont que la combinaison des deux autres, la seule chose nécessaire est une fonctionalité de recherche (_lookup_) qui est fourni par le `Data.List` de la bibliothèque par défaut.

```haskell
ghci> import Data.List as List
ghci> assoc = [("foo", True), ("bar", False)]
ghci> :t assoc
assoc :: [(String, Bool)]
ghci> :t List.lookup
List.lookup :: Eq a => a -> [(a, b)] -> Maybe b
ghci> List.lookup "foo" assoc
Just True
ghci> List.lookup "bar" assoc
Just False
ghci> List.lookup "baz" assoc
Nothing
```

On retrouve ici la gestion du cas d'erreur en cas d'absence de résultat grâce au type `Maybe`. En effet, il est toujours possible de rechercher une clé absente de la liste. Dans ce cas, aucun résultat ne peut être retourné et on obtient la valeur `Nothing`.

Il est intéressant de noter que la contrainte `Eq a` sur la clé permet à la fonction de recherche _lookup_ de réaliser une comparaison d'égalité entre le critère de recherche et les clés de la liste.

Tandis que les _assoc lists_ sont une première introduction aux conteneurs de type clé-valeur comme elles sont construites à partir des types vus précédemment, elles ne sont en réalité que peu utiles au quotidien (il y a des solutions plus adaptées). Comme évoqué précédemment, les listes ne sont pas très performantes pour les fonctions de recherches et la complexité asymptotique est, dans le pire cas de figure, linéaire `𝛰(n)`. Les _assoc lists_ sont souvent utilisées comme structures de données intermédiaires avec pour objectif d'être transformées en `Map`. La conversion en elle même a une complexité de `𝛰(n*log n)` mais la recherche par clé sur une `Map` se fera alors avec une complexité de `𝛰(log n)`. Le coût de conversion, relativement important, est alors très vite rentabilisé à l'utilisation.

## _Sets_

Les _sets_ sont des conteneurs très intéressants, le principe central est l'appartenance (_membership_). Il est commun de créer un _set_ afin de pouvoir tester ultérieurement si une valeur en fait partie.

Il ne peut être construit qu'en insérant des éléments dans un _set_ vide :

```haskell
ghci> import Data.Set as Set
ghci> :t Set.empty
Set.empty :: Set a
ghci> Set.empty
fromList []
ghci> Set.insert 1 (Set.insert 2 (Set.insert 3 Set.empty))
fromList [1,2,3]
```

Ou à partir d'une _list_ :

```haskell
ghci> Set.fromList [4,3,2,1]
fromList [1,2,3,4]
```

On peut noter que les éléments ont été triés après la création du `Set`. L'implémentation concrète des _sets_ en Haskell sont des _binary trees_, qui dépendent de la capacité des données à être triées (_orderable_). On peut voir cette contrainte `Ord a` dans la définition des fonctions du type `Set` comme, par exemple, `insert` ou `fromList`.

```haskell
ghci> :t Set.insert
Set.insert :: Ord a => a -> Set a -> Set a
ghci> :t Set.fromList
Set.fromList :: Ord a => [a] -> Set a
ghci> :t Set.member
Set.member :: Ord a => a -> Set a -> Bool
```
Les _sets_ ont une proprité très utile : il ne peut pas y avoir de duplication en leur sein. Si on insère plusieurs fois la même valeur, il ne se passe rien.

```haskell
ghci> insert1 = Set.insert 1
ghci> insert1 Set.empty
fromList [1]
ghci> insert1 (insert1 Set.empty)
fromList [1]
ghci> insert1 (insert1 (insert1 Set.empty))
fromList [1]
```
Passons à la pratique avec un cas d'utilisation concret.

```haskell
ghci> evens = Set.fromList [0,2..1000000]
ghci> Set.member 7 evens
False
ghci> Set.member 200012 evens
True
ghci> isEven n = Set.member n evens
ghci> isEven 7
False
ghci> isEven 8
True
```

On peut se dire "hmmm une limite à `1000000` pour les nombres pairs est incorrecte", et ce serait pertinent ! Ceci met en évidence une caractéristique des _sets_ en Haskell, ils sont dits "finis". Ceci est dû à la rigidité nécessaire pour construire les _binary trees_ implémentant les _sets_ comme évoqué précédemment. C'est une différence notable avec les _lists_ qui sont potentiellement "infinies" et dites _lazy_ (évaluées à l'utilisation plutôt qu'à la création).

### Opérations sur les _Sets_

La fonction `difference` du module `Data.Set` est un bon moyen de séparer un _set_ en nouveaux _sets_ à partir de la notion d'appartenance (_membership_) de ses éléments avec un autre _set_. Plus simplement, cette fonction retourne un _set_ avec tous les éléments du premier _set_ en retirant les éléments présents dans le second _set_.

```haskell
ghci> set1 = Set.fromList ["a", "b", "c", "1", "2", "3"]
ghci> letters = Set.fromList ["a", "b", "c"]
ghci> nums = Set.fromList ["1", "2", "3"]
ghci> Set.difference set1 letters
fromList ["1","2","3"]
ghci> Set.difference set1 nums
fromList ["a","b","c"]
```

La fonction `union` va combiner les éléments (sans duplication) de deux _sets_.

```haskell
ghci> nums
fromList ["1","2","3"]
ghci> letters
fromList ["a","b","c"]
ghci> Set.union nums letters
fromList ["1","2","3","a","b","c"]
ghci> set1
fromList ["1","2","3","a","b","c"]
ghci> Set.union nums set1
fromList ["1","2","3","a","b","c"]
```

La fonction `intersection` va permettre de récupérer les éléments communs aux deux _sets_.

```haskell
ghci> Set.intersection nums letters
fromList []
ghci> Set.intersection nums set1
fromList ["1","2","3"]
ghci> Set.intersection letters set1
fromList ["a","b","c"]
ghci> Set.intersection (fromList [1, 2]) (fromList [2, 3])
fromList [2]
```

`isSubsetOf` va permettre de vérifier si l'ensemble des éléments d'un _set_ sont contenu dans un autre _set_.

```haskell
ghci> Set.isSubsetOf letters set1
True
ghci> Set.isSubsetOf nums set1
True
ghci> Set.isSubsetOf nums letters
False
ghci> Set.isSubsetOf set1 nums
False
```

Tout _set_ est un contenu par lui-même.

```haskell
ghci> Set.isSubsetOf nums nums
True
ghci> Set.isSubsetOf set1 set1
True
ghci> Set.isSubsetOf letters letters
True
```

## _Maps_

En Haskell, les _Maps_ sont le conteneur privilégié pour le stockage de données sous forme clé-valeur, parfois également appelées dictionnaires (_dictionnaries_)

Une `Map` nécessite que ses clés respectent une contrainte de tri (`Ord k`) tout comme une `Set` pour ses valeurs. La raison est la même que pour les _sets_, un _balanced binary tree_ est utilisé comme structure de données dans l'implémentation bas-niveau du type `Map` en Haskell, qui nécessite cette capacité à trier les clés.

Le type `Map` et les fonctions qui permettent d'intéragir avec sont importés du module `Data.Map` qui fait partie du _package_ `containers`. Pas d'inquiétude pour les dépendances à ce stade, conteneurs est une bibliothèque centrale et est intégrée dans l'interpréteur _ghci_.

Comme les _sets_, une _map_ doit être contruite à partir d'une _map_ vide en insérant des paires clé-valeur :

```haskell
ghci> import Data.Map (Map)
ghci> import qualified Data.Map as Map
ghci> :t Map.empty
Map.empty :: Map k a
ghci> Map.empty
fromList []
ghci> Map.insert "a" 'a' (Map.insert "b" 'b' (Map.insert "c" 'c' Map.empty))
fromList [("a",'a'),("b",'b'),("c",'c')]
```

Ou à partir d'une _list_ :

```haskell
ghci> Map.fromList [(4, '4'), (3, '3'), (2, '2'), (1,'1')]
fromList [(1,'1'),(2,'2'),(3,'3'),(4,'4')]
```

### Mettre à jour une valeur dans une _map_

`adjust` est une fonction utile à connaître. Elle permet de mettre à jour la valeur d'une clé spécifique, uniquement si elle existe, si ce n'est pas le cas il n'y a pas de modification et la _map_ existante est retournée.

```haskell
ghci> Map.adjust (+2) "first" oneItem
fromList [("first",3)]
ghci> :t Map.adjust
Map.adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
ghci> Map.adjust (+2) "second" oneItem
fromList [("first",1)]
```

Un lecteur averti notera qu'en réalité la _map_ d'origine n'est pas mise à jour mais une copie intégrant la modification est retournée. Le second appel à `adjust` retourne la _map_ originelle `oneItem`, ce qui fait sens étant donné que toutes les données en Haskell sont immuables (_immutables_) !

### Quand l'utiliser ?

Les _maps_ sont vraiment très adaptées pour persister en mémoire des états qui devront être récupérés à partir d'une clé, d'un type préalablement définis. Ceci est dû aux bonnes performances de recherche à partir des clés qui caractérisent les _maps_ grâce à la contraite de tri sur leurs clés, on a une complexité asymptotique (`𝛰(log n)`). Un exemple évident est le stockage de session sur un serveur d'une application web. L'état de la session peut être stocké dans une _map_ avec l'id présent dans le _cookie_ de session comme clé. Ainsi, l'état de la session est accessible de manière performante via cette _map_ pour chaque requête. Cette solution ne permet pas de gérer un nombre infini de sessions mais vous serez étonnés de voir à quelle point elle est efficace, pour répondre simplement aux besoins de la plupart des applications !

### Une _Map_ avec "des clés sans contraintes de tri" : _HashMaps_

Dans certains cas, on veut utiliser comme clé, un type qui ne respecte pas la contrainte de tri. Les _Hashmap_ sont faites pour ça. Une _Hashmap_ va "hacher" la clé (quelque soit le type tant qu'il est _hashable_) pour la rendre "ordonnable" !

Le module qui exporte le type `HashMap` et ses fonctions est `Data.HashMap.Strict`, il fait parti du _package_ `unordered-containers`. L'_api_ est identique à celle de `Map` excepté que la contrainte sur la clé est `Hashable k` au lieu de `Ord k`.

## Vectors

Parfois on souhaite avoir une _list_ comme conteneur avec de bonnes performances d'accès aux données. Dans la plupart des langages, les _arrays_ sont la structure de données de base pour gérer des séquences de données. Haskell possède également ce type de structure de données et l'implémentation la plus populaire est le type `Vector` de la bibliothèque `vector`.

Un des aspects les plus intéressants à propos de ce type est qu'il offre un accès aux données de `𝛰(1)`. La bibliothèque offre les deux types d'accesseurs _safe_ et _unsafe_ comme pour les _lists_ vues précédemment.

Voici un exemple d'un conteneur qui nous permet de stocker et récupérer les caractères _ascii_ à partir de leur code.

__Note__: Vous pouvez installer la bibliothèques `vector` dans votre interpréteur _ghci_ grâce aux commandes : `cabal repl --build-depends "vector"` ou `stack exec --package vector -- ghci`

```haskell
ghci> import Data.Vector as V
ghci> asciiChars = V.fromList ['\NUL'..'\DEL']
ghci> asciiChars ! 48
'0'
ghci> asciiChars ! 97
'a'
ghci> asciiChars ! 65
'A'
ghci> asciiChars !? 65
Just 'A'
ghci> asciiChars !? 128
Nothing
ghci> asciiChars !? 127
Just '\DEL'
```

Ils offrent également une capacité à subdiviser un _vector_ en plusieurs autres de manière performante `𝛰(1)`. La fonction `slice` prend un index de départ, la longueur voulue et le _vector_ à subdiviser, elle retourne le nouveau _vector_. Par contre, cette fonction n'est pas sûre : si l'index de départ additionné à la longueur demandée donne un résultat supérieur à la longueur du _vector_ une erreur aura lieu à l'exécution (_runtime error_) !

```haskell
ghci> :t V.slice
V.slice :: Int -> Int -> Vector a -> Vector a
ghci> lowerCase = V.slice 97 26 asciiChars
ghci> lowerCase
"abcdefghijklmnopqrstuvwxyz"
ghci> upperCase = V.slice 65 26 asciiChars
ghci> upperCase
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ghci> nums = V.slice 48 10 asciiChars
ghci> V.length nums
128
ghci> nums
"0123456789"
-- Error case, the asciiChars vectors has length 128, and our slice supposes a
-- length of 97 + 92 (189)
ghci> V.slice 97 92 asciiChars
"*** Exception: ./Data/Vector/Generic.hs:408 (slice): invalid slice (97,92,128)
CallStack (from HasCallStack):
  error, called at ./Data/Vector/Internal/Check.hs:87:5 in vector-0.12.3.0-8cc976946fcdbc43a65d82e2ca0ef40a7bb90b17e6cc65c288a8b694f5ac3127:Data.Vector.Internal.Check
```

## _Overloaded Lists_

Vous aurez noté que l'on utilise souvent la fonction `fromList` pour construire nos conteneurs. Il y a une extension (`OverloadedLists`) qui permet d'utiliser la syntaxe de création des _lists_. L'inconvénient est que si le type n'est pas explicitement fourni les messages d'erreur ne seront pas précis.

```haskell
ghci> :set -XOverloadedLists -- This is the language extension
ghci> [1,2,3] :: Set Int
fromList [1,2,3]
ghci> [1,2,3] :: Vector Int
[1,2,3]
ghci> [('a', 1),('b', 2),('c', 3)] :: Map Char Int
fromList [('a',1),('b',2),('c',3)]
```

Si on n'explicite pas le type voulu...

```haskell
ghci> ["1", "2", "3"]

<interactive>:11:1: error:
    • Illegal equational constraint GHC.Exts.Item l ~ [Char]
      (Use GADTs or TypeFamilies to permit this)
    • When checking the inferred type
        it :: forall {l}.
              (GHC.Exts.IsList l, GHC.Exts.Item l ~ [Char]) =>
              l
```

L'erreur est particulièrement peu explicite, c'est pourquoi cette extension n'est pas activée par défaut. C'est toujours bon à savoir !
