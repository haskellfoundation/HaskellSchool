---
version: 1.3.1
title: Containers
---

_Tuples_, _Lists_, _Assoc Lists_, _Sets_, _Maps/Hashmaps_, et _Vectors_. (Les termes techniques ne sont volontairement pas traduits et seront affichés en _italique_)

{% include toc.html %}

## Quand utiliser tel ou tel _container_ ?

Voici une liste de pense-bêtes permettant de choisir le bon _container_ en fonction du contexte. 
L'objectif est mnémotechnique et fera surement plus sens après avoir lu les chapitres correspondants.

- Besoin d'associer simplement des données de différents types ? Utilisons un _[Tuple](#tuples)_
- Besoin d'ajouter des éléments au début de _container_ (_prepend_) de manière performante ? Utilisons une _[List](#lists)_
- Besoin de garantir l'unicité et l'ordre des éléments ? Utilisons un _[Set](#sets)_
- Besoin de rechercher une valeur à partir d'une clé ? Utilisons une _[Map](#maps)_
- Besoin d'indéxer des données de manière performante ? Utilisons un _[Vector](#vectors)_

## Utiliser les modules (_List_, _Set_, _Map_)

Une fonctionnalité appréciable des modules _List_, _Set_ et _Map_ est leur _api_ identique (ou presque). Ainsi, l'utilisation de ces _containers_ est plus intuitive et plus simple à mémoriser, permettant un accès rapide à un large éventail d'outils pour un investissement relativement faible.

La contre-partie est l'exposition à des conflits dans nos imports (_namespace collisions_). C'est une pratique commune et recommandée d'utiliser des imports nommés (_qualified imports_). Si vous suivez ce chapitre avec votre interpréteur `ghci`, nous vous invitons à importer les modules comme décrit ci-dessous, ainsi tous les modules seront correctement disponibles.

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

Afin de se familiariser avec les messages d'erreur, en prévision du jour où ils arriveront (et ce jour arrivera :wink:), voici ce qui se produit sans import nommé (_unqualified imports_)

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

Les _Tuples_ sont la première structure de donnée que vous allez découvrir en Haskell. C'est une structure de donnée, simple, primitive, avec une syntaxe native et concise. Les champs sont référencés selon leurs positions. Théoriquement, Les _Tuples_ peuvent contenir un nombre infini de champs, c'est ce qu'on appelle *arité* (_arity_). En réalité, les spécifications _Haskell Report_ n'imposent aux compilateurs (et interpréteurs) une taille minimale que de 15 champs (_15-tuple_). `GHC` supporte un nombre de champs allant jusqu'à 62 (_62-tuple_). Le nombre minimum de champs pour un _tuple_ est de 2 (_2-tuple_). C'est sur ce type que nous nous concentreront étant donné qu'Haskell fournit de nombreuses fonctionnalités par défaut pour celui-ci. A titre d'exemple, voici un _tuple_ avec 8 champs (_8-tuple_).

```haskell
ghci> :t ('0', '1', '2', '3', '4', '5', '6', "8-tuple")
('0', '1', '2', '3', '4', '5', '6', "8-tuple")
  :: (Char, Char, Char, Char, Char, Char, Char, [Char])
```
__Note__: `:t` dans l'interpréteur ghci affiche le type de la valeur donnée. Il affichera le message sous la forme `valeur :: Type`.

### Quand l'utiliser ?

Un _Tuple_ est utile pour associer des données, éventuellement hétérogènes : "J'ai besoin d'associer ces élements". Il n'a pas de valeur *sémantique* (sens), mais se concentre sur la *syntaxe* (structure), c'est pourquoi c'est souvent déconseillé d'en abuser. Si la structure de donnée à créer est utilisée à plusieurs endroits de l'application, il est préférable d'utiliser un _Record_. Au contraire, si l'utilisation est isolée et locale (interne à une fonction par exemple) alors un _Tuple_ peut être appropriée!

### Comment le créer ?

Un _Tuple_ peut être créé en utilisant les parenthèses avec chaque élément séparé par une virgule `(a, b)`.

```haskell
ghci> myTuple = (True, "hello")
ghci> :t myTuple
myTuple :: (Bool, [Char])
```

On peut également laisser un champ vide, il se transforme alors en fonction. Cette technique est appelé _tuple sectioning_. Elle requiert l'extension `TupleSections`.

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
__Note__: Pour activer une extension du langage (_language extension_) depuis l'interpréteur ghci on utilise le mot-clé `:set` qui permet de définir une option, le prefixe `-X` permet de déclarer que c'est une extension du langage qui est directement suivi du nom de l'extension en pascal case.

### Comment le manier ?

La principale manière d'utiliser un _tuple_ est en le décomposant. L'approche la plus commune est d'utiliser le _pattern matching_ sur la structure du _tuple_ et d'accéder à son contenu. Cette technique est particulièrement pratique parce qu'elle fonctionne pour tous types (taille) de tuples.

```haskell
ghci> (\(a, b) -> not a) myTuple
False
ghci> (\(a, b) -> b <> " world") myTuple
"hello world"

ghci> (\(a, b, c) -> a <> " " <> b <> " " <> c) ("my", "name", "is")
"my name is"
```

Avec un _tuple_ d'arité de 2 (_2-tuple_), les fonctions `fst` et `snd` sont disponibles dans la bibliothèque standard. Les _2-tuples_ sont les seuls _tuples_ pour lesquels ces fonctions sont disponibles. Pour les autres _tuples_, il faudra écrire ses propres fonctions pour accéder aux élements voulus.

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

La principale limitation des _tuples_ en Haskell est que chaque chaque arité correspond à un type disctinct. Il n'y a donc pas de fonction commune pour ajouter un élément à un _tuple_. Ces fonctions doivent donc être écrites ad hoc (expressément pour chaque usage).

Voici un exemple pour augmenter la taille du _tuple_

```haskell
ghci> twoTupleToThreeTuple c (a, b) = (a, b, c)
ghci> twoTupleToThreeTuple () myTuple
(1, "world", ())
```

Tenter d'appeler notre fonction sur un _tuple_ d'arité différente conduira à une erreur de typage laconique; la fonction attend un _2-tuple_ mais on lui a fournit un _3-tuple_.

```haskell
ghci> twoTupleToThreeTuple True (1, 2, 3)

<interactive>:19:27: error:
    • Couldn't match expected type: (a, b)
                  with actual type: (a0, b0, c0)
                  ...
```

## _Lists_

En terme d'utilisation, les _Lists_ permettent de résoudre le problème d'extension qu'on a observer avec les _Tuples_ (capacité à augmenter la taille du _container_ sans créer une nouvelle instance). Par contre, une _List_ ne peut contenir qu'un unique type de donnée (elles sont dites homogènes). Les _Lists_ sont construites avec une syntaxe spécifique : les crochets avec des virgules séparant chaque éléments.

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
```

### Types inductif/récursif (_Inductive Types_)

Les _Lists_ sont la première introduction aux types dit "inductif", une sous-catégorie des types "recursifs" (nous y reviendrons dans les concepts avancés). Voici un exemple identique à l'implémentation en Haskell sans le sucre syntaxique.

```haskell
data List a = Nil | Cons a (List a)
```

On peut voir que ce type est recursif. `Nil` est la valeur de base, le constructeur `Cons` y "adjoint" `a` et appelle récursivement `List a`. On peut également constater pourquoi les _Lists_ ne peuvent contenir qu'un seul type de données. En effet, la récursion ne porte que sur un type de données `a`. Dasn notre définition on peut remplacer `Nil` par une _List_ vide `[]` et `Cons` par `:` pour retrouver la syntaxe par défaut.

Voici plusieurs exemples équivalents pour illustrer cette mécanique : la création par défaut d'une _List_, la création avec le constructeur `:`, et une créaton manuelle avec le constructeur `Cons`.

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
ghci> 1 : 2 : 3 : 4 : []
[1,2,3,4]
ghci> Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
```

### Quand l'utiliser ?

Les _Linked lists_ sont des structures de données très présentes en programmation fonctionnelle, par conséquent vous y serez très souvent confronté en Haskell. C'est en général le premier _container_ auquel on pense. De part, la faible performence de l'ajout d'élément en fin de _List_ (_append_) et la relative faible performance d'accès par index (𝛰(n) avec n l'index), les _Lists_ sont généralement utilisées lorsque l'on sait que l'on va devoir itérer sur les élements de celles-ci et où l'ordre des élements est important.

Un bon exemple d'utilisation des _Linked lists_ : implémenter une _Stack_ parce que l'ajout et le retrait d'élément se font avec une complexité 𝛰(1) (_Last In First Out_).

Un mauvais exemple d'utilisation des _Linked lists_ : implémenter une _Queue_ parce que l'ajout ou le retrait d'élement se fait avec une complexité 𝛰(n) (_First In First Out_).

Un exemple concret que l'on rencontre souvent dans des applications est la créationd de requêtes en base de données. Une requête peut retourner soit aucun résultat `[]` soit un certain nombre potentiellement ordonnés`[entity..]`. Les bibliothèques d'accès aux base de données ont rarement besoin de garantir un accès par index performant, ainsi elles laissent cette responsabilité à la fonction appelante.

### Concatenation de _Lists_

Pour concater deux listes (ou plus), on utilise l'opérateur `++` :

```haskell
ghci> [1, 2] ++ [3, 4, 1]
[1, 2, 3, 4, 1]
```

### Les fonctions utilitaires : _Head_ / _Tail_

Lorsque l'on utilise des _Lists_, il est commun de travailler avec le premier élément (_head_) et le reste des éléments (_tail_).

Haskell fournit deux fonctions simples pour accéder à ces éléments de la liste : `head` et `tail` :

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

Pour couvrir ces possibilités de manière sure, il est commun en Haskell d'utiliser le type `Maybe`. Il permet de notifier au programme que les cas qui ne sont pas couverts doivent retourner `Nothing`. L'appelant de la fonction retournant un type _maybe_ est alors forcé de gérer les cas où `Nothing` sera retourné mais se prémunit des exceptions lors de l'éxecution du programme (_runtime exceptions_).

```haskell
ghci> :i Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Maybe’
...
```
__Note__: `:i` dans l'interpréteur ghci donne des informations à propos du type, la première ligne est son implémentation.

On peut à présent définir des fonctions _head_ et _tail_ sures, prenant en compte tous les cas possibles sans exceptions, (_total functions_) en utilisant le _pattern matching_!

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
__Note__: `:{` et `:}` permettent d'écrire des définition sur plusieures lignes dans l'interpréteur ghci.

Youpi! Plus d'exceptions.

Une autre manière de s'assurer que `head` et `tail` sont sures est d'utiliser les _NonEmpty Lists_ :

```haskell
ghci> import Data.List.NonEmpty
ghci> :i NonEmpty
data NonEmpty a = a .:| [a]
```

En regard de plus près la définition, on s'aperçoit que `NonEmpty` oblige la présence d'un premier élément. Le symbole `:|` est un constructeur comme `:`, dans les faits la définition de `NonEmpty` est identique à la définition des _Lists_ vues précédemment, à la nuance prêt qu'elle omet le cas `[]` (_Nil_).

Cette manière aborde le problème avec une approche opposée à la solution précédente avec `Maybe`. Plutôt que de forcer l'appelant de se prémunir des cas manquants lorsqu'il reçoit le résultat, elle force l'appelant à construire sa liste avec des données valides (lorsqu'il appelle la fonction).

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

Haskell implémente les liste comme des _Linked Lists_. Le constructeur `:` (appelé _cons_ abbrégé de _constructor_) sert de lien (_link_) entre les éléments de la liste. Ce choix a pour conséquences que certaines opérations soient rapides et d'autres lentes : 

Ajouter un élément au début d'une liste est facile et rapide. Il suffit de "lier" un nouvel élément à la liste existante avec le constructeur `:`.

```haskell
prepend value list = value : list
```

Par contre, étant donné que la liste peut être vide (`[]` ou `Nil`) ou qu'un élément peut être lié au reste de la liste (comme on l'a vu précédemment), celle-ci ne comporte pas d'information sur sa taille ou de référence vers la fin de la liste.

Par conséquent, pour récupérer la longueur d'une liste on doit iétrer sur l'ensemble de celle-ci pour compter le nombre d'élément. Pour trouver un élément à un index spécifique, on doit traverser la liste jusqu'à l'atteindre.

De même, pour ajouter un élément à la fin d'une liste existante on doit ajouter un constructeur à la fin de celle-ci et "lier" l'élément :

```haskell
append originalList newList =
    case originalList of
        [] -> newList
        x : xs -> x : append xs newList
```

La fonction `append` definit ci-dessus est identique à l'opérateur `++`, il faut donc être prudent lorsque l'on veut ajouter un élément en fin de liste. Une concaténation avec `++` dans une boucle donne une compléxité de 𝛰(n²) !

En raison de l'implémentation avec des _Linked List_, la majeure partie des opérations sur les listes ont une complexité linéaire en temps (_linear time complexity_, `𝛰(n)`). Dans la plupart des cas ces opérations sont plus lentes qu'avec d'autres _containers_, une bonne raison d'apprendre à connaitre chacun avec leurs avantages et leurs inconvénients !

## _Assoc(iation) lists_

Jusqu'à maintenant on a vu l'accès aux valeurs d'une _List_ à partir de ses indexes ou aux valeurs d'un _Tuple_ par _pattern matching_. Cependant, un des cas d'utilisation les plus répandus dans l'utilisation des _containers_ est le stockage de paires clés-valeurs (_Key-Value_). Les _Assoc(iation) Lists_ (_assoc_ dans la litérature) permettent ce stockage en combinant _List_ et _Tuple_ (_2-tuple_). Etant donné que ces structures de données ne sont que la combinaison des deux autres, la seule chose nécessaire est une fonctionalité de recherche (_lookup_) qui est fournit par le `Data.List` de la bibliothèque par défaut.   

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

On retrouve ici la gestion du cas d'erreur en cas d'absence de résultat grâce au type `Maybe`. En effet, il est toujours *possible* de rechercher une clé absente de la liste. Dans ce cas, aucun résultat ne peut être retourner et on obtient la valeur `Nothing`.

Il est intéressant de noter que la contrainte `Eq a` sur la clé permet à la fonction de recherche _lookup_ de réaliser une comparaison d'égalité entre le critère de recherche et les clés de la liste.

Tandis que les _assoc lists_ sont une première introduction aux _containers_ de type clé-valeur comme elles sont construites à partir des types vus précédemment, elles ne sont en réalité que peu utiles au quotidien (il y a des solutions plus adaptées). Comme évoqué précédemment, les listes ne sont pas très performantes pour les fonctions de recherches et la complexité asymptotique est, dans le pire cas de figure, linéaire `𝛰(n)`. Les _assoc lists_ sont souvent utilisées comme structures de données intermédiaires avec pour objectif d'être transformées en `Map`. La conversion en elle même a une complexité de `𝛰(n*log n)` mais la recherche par clé sur une `Map` se fera alors avec une complexité de `𝛰(log n)`. Le coût de conversion, relativement important, est alors très vite rentabilisé à l'utilisation.

## _Sets_

Les _Sets_ sont des _containers_ très intéressants, le principe central d'un _Set_ est l'appartenance (_membership_). Il est commun de créer un _Set_ afin de pouvoir tester ultérieur si une valeur en fait partie.

Un _Set_ ne peut être construit qu'en insérant des élements dans un _Set_ vide :

```haskell
ghci> import Data.Set as Set
ghci> :t Set.empty
Set.empty :: Set a
ghci> Set.empty
fromList []
ghci> Set.insert 1 (Set.insert 2 (Set.insert 3 Set.empty))
fromList [1,2,3]
```

ou à partir d'une _List_ :

```haskell
ghci> Set.fromList [4,3,2,1]
fromList [1,2,3,4]
```

On peut noter que les éléments ont été triés après la création du `Set`. L'implémentation concrète des _Sets_ en Haskell sont des _binary trees_, qui dépendent de la capacité des données à être triées (_orderable_). On peut voir cette contrainte `Ord a` dans la définition des fonctions du type `Set` comme `insert` ou `fromList` par exemple.

```haskell
ghci> :t Set.insert
Set.insert :: Ord a => a -> Set a -> Set a
ghci> :t Set.fromList
Set.fromList :: Ord a => [a] -> Set a
ghci> :t Set.member
Set.member :: Ord a => a -> Set a -> Bool
```
Les _Sets_ ont une proprité très utile : il ne peut pas y avoir de duplication en leur sein. Si on insère plusieurs fois la même valeur, il ne se passe rien.

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

On peut se dire "hmmm une limite à `1000000` pour les nombres pairs est incorrect", et ce serait pertinent! Ceci met en évidence une caractéristique des _Sets_ en Haskell, elles sont dites "finies" de part la rigueur nécessaire des structure de données bas niveau permettant de les créer. C'est une différence notable avec les _Lists_ qui sont potentiellement "infinies" et dites _lazy_ (évaluées à l'utilisation plutôt qu'à la création).

### Opérations sur les _Sets_

La fonction `difference` du module `Data.Set` est un bon moyen de séparer un _Set_ en nouveaux _Sets_ à partir de la notion d'appartenance (_membership_) de ses éléments avec un autre _Set_. Plus simplement, cette fonction retourne un _Set_ avec tous les éléments du premier _Set_ en retirant les éléments présents dans le second _Set_.

```haskell
ghci> set1 = Set.fromList ["a", "b", "c", "1", "2", "3"]
ghci> letters = Set.fromList ["a", "b", "c"]
ghci> nums = Set.fromList ["1", "2", "3"]
ghci> Set.difference set1 letters
fromList ["1","2","3"]
ghci> Set.difference set1 nums
fromList ["a","b","c"]
```

La fonction `union` va combiner les éléments (sans duplication) de deux _Sets_.

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

La fonction `intersection` va permettre de récupérer les éléments communs aux deux _Sets_.

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

`isSubsetOf` va permettre de vérifier si l'ensemble des éléments d'un _Set_ sont contenu dans un autre _Set_.

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

Tout _Set_ est un contenu par lui-même.

```haskell
ghci> Set.isSubsetOf nums nums
True
ghci> Set.isSubsetOf set1 set1
True
ghci> Set.isSubsetOf letters letters
True
```
