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
