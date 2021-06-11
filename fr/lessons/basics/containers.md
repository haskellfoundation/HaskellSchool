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
