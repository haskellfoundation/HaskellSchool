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
