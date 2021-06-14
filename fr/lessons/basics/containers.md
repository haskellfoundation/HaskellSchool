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

## Tuples

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

## Lists

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

### Head / Tail

When using lists, it is common to work with a list's head and tail. The head is
the list's first element, while the tail is a list containing the remaining
elements.

Haskell provides two helpful functions, `head` and `tail`, for working with
these parts:

```haskell
ghci> head ["Orange", "Banana", "Apple"]
"Orange"
ghci> tail ["Orange", "Banana", "Apple"]
["Banana","Apple"]
```

Unfortunately these functions reveal an ugly part of the language's base
library; they may raise an exception, even when given an argument with the
appropriate type. The cause of these exceptions is that they do not cover the
full domain of possible inputs.

```haskell
ghci> head []
*** Exception: Prelude.head: empty list
ghci> tail []
*** Exception: Prelude.tail: empty list
```

We can use a common idiom in Haskell for covering partial functions in a safe
way, the `Maybe` type. This allows us to say that unhandled inputs return a
`Nothing`. Now the caller of this maybe-returning-function must handle the
`Nothing` case, but in return they are not faced with a nasty runtime exception.

```haskell
ghci> :i Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Maybe’
...
```
__Note__: `:i` in ghci will give you some information about the type, the first
line is the implementation.

Now we can define a total head and tail function using pattern matching!

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
__Note__: `:{` and `:}` allow you to write multiline definitions in ghci.

Hooray! No more exceptions.

Another way to ensures that `head` and `tail` are safe is the non-empty list:

```haskell
ghci> import Data.List.NonEmpty
ghci> :i NonEmpty
data NonEmpty a = a .:| [a]
```

From its definition we can see that `NonEmpty` requires the first element to be
present. This symbol `:|` is a constructor just like `:`, in fact the definition of
`NonEmpty` is identical to that of lists, except it omits the `[]` (Nil) case.

This handles the partiality problem the exact opposite way as the
`Maybe` solution. Instead of forcing the caller of the function to guard against
the ill-defined case when handling the result of the function, it forces them to
construct a valid input up front (when calling the function).

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

Notice that this time the error is not a runtime exception but a type error, the
compiler is telling us that we tried to use a (potentially empty) list rather
than the required `NonEmpty` type.

### List Performance

Haskell implements lists as linked lists. The cons cells (the operator `:` is
called cons, short for constructor) act as the links. This dictates which
operations can be done quickly and which can be slow:

Prepending a value to a list is easy and fast - all we have to do is create a
new cons cell with the element we want to prepend and point it to the existing
list.

```haskell
prepend value list = value : list
```

On the other hand, since the list data type (as shown above) can be either
empty (`[]` or `Nil`) or a cons cell that will point to the rest of the list,
it does not contain information about the length of the list, or a reference to
the end of the list.

Because of that, in order to retrieve the length of a list we must walk
each cons cell and count until we reach the end of the list. To find the
value at a specific index we need to traverse the list until we reach it.

Similarly, in order to append a list to an existing list, we need to go to the
end of the existing list, and add a cons cell that points to the new list:

```haskell
append originalList newList =
    case originalList of
        [] -> newList
        x : xs -> x : append xs newList
```

The append function defined here is really the same as the `++` operator, as you
might have deduced we need to be careful when using list append. Particularly
`++` inside of loops has quadratic performance!

By virtue of the linked list data structure, many list operations run in linear
time (`𝛰(n)`). In many cases the same operation is significantly slower for
lists than for other containers, this is a great reason to be familiar with each
and their tradeoffs!
