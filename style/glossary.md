## What is this?

This file is targeted at the translators and serves as a basic tutorial for translating this project. It includes two lists of terms:
  - terms which do not get translated at all
  - terminology translations (glossary)

### What does not get translated?

Class names and proper nouns do not get translated.


### Table of Contents

- [Glossary - Russian](#glossary-russian)
  - [Elixir-related terms](#russian-elixir-related-terms)
  - [CS terms](#russian-cs-terms)

### <a name="glossary-russian"></a> Glossary - Russian

This Translation table is adapted from the [Russian Haskell community wiki](https://github.com/ruHaskell/ruhaskell/wiki/Translation).

#### <a name="russian-terms"></a> Термины

| Иностранное | Русское |
|---|---|
| ad hoc (polymorphism) | специализированный [Брон14] |
| aldebraic data type, ADT | алгебраический тип данных, АТД |
| alias | псевдоним |
| applicative | аппликативный |
| arrow | стрелка |
| bifunctor | бифунктор |
| bind | (монадическое) связывание |
| boxed | упакованный |
| category | категория |
| class | класс [Виль15] |
| closure | замыкание [Виль15] |
| coerce | приводить |
| coercible | приводимый |
| coercion | приведение |
| concurrency | конкурентность [Браг13М] |
| constraint | ограничение |
| contravariant | контравариантный |
| cost center | центр затрат [Виль15] |
| covariant | ковариантный |
| currying | каррирование |
| default definition | определение по умолчанию |
| data type | тип данных |
| derive (an instance) | вывести (экземпляр) |
| dual | дуальный |
| equational reasoning | эквациональное рассуждение [Браг13, Виль15] |
| fixed point | неподвижная точка |
| fold, unfold | свёртка, развёртка [Виль15] |
| foldable | свёртываемый [Виль15] |
| force a thunk | интерпретировать задумку [Браг16]; вычислить переходник [Виль15]; вынудить [Доб06] |
| force an evaluation | форсировать вычисление |
| foreign | внешний, заграничный |
| free monad | свободная монада |
| freer monad | монада посвободнее |
| function application | применение функции |
| functor | функтор |
| generalized algebraic data type, GADT | обобщённый алгебраический тип данных, ОАТД |
| guard (in a pattern match) | страж |
| identity (element) | нейтральный элемент |
| identity function, morphism | тождественное преобразование |
| inhabit | населять |
| instance | экземпляр [Виль15] |
| instantiated (`fmap` instantiated for `Maybe`) | реализованный |
| inverse (element) | обратный элемент |
| isomorphism | изоморфизм |
| kind | вид [лекции Брагилевского]; род; сорт |
| lazy evaluation | ленивое вычисление |
| lens | линза |
| list comprehension and specifiers | формирователь списка и спецификаторы [Виль15] |
| map | проекция [Виль15]; отображение |
| monad | монада |
| monad transformer | монадный трансформер |
| monoid | моноид |
| morphism | морфизм |
| mutable, immutable | изменяемый, неизменяемый |
| natural mapping | естественное отображение |
| natural number | натуральное число |
| natural transformation | естественное преобразование |
| optic | оптика, оптический |
| package | пакет |
| parallelism | распараллеливание [Браг13М] | 
| parametric polymorphism | параметрический полиморфизм [Виль15] |
| partial | частичный |
| pattern matching | сопоставление с образцом [Виль15] |
| point-free style | бесточечный стиль [Виль15] |
| polymorphism | полиморфизм |
| prelude | прелюдия |
| prism | призма |
| product type | тип-произведение |
| profunctor | профунктор |
| promotion (of a type) | продвижение (типа) [Виль15] |
| qualified import | импорт с квалификацией [Виль15] |
| record | запись [Виль15] |
| record puns | уплотнение записей [Виль15] |
| referential transparency | ссылочная прозрачность |
| refutable, irrefutable pattern | ???, бесспорный образец [Виль15] |
| resolver | решатель |
| row (polymorphism) | строчный [Брон14]; рядный |
| section (частичное применение оператора) | сечение |
| semigroup | полугруппа |
| smart constructor | умный конструктор |
| strict/eager (evaluation) | строгое; энергичное [Зеф09] (вычисление) |
| strong typing | сильная типизация |
| subtyping | подтипизация |
| sum type | тип-сумма |
| tagged union | помеченное объединение |
| thunk | задумка [Браг16]; санк [Доб06] |
| total function | тотальная; всюду определённая функция |
| total order | полный порядок |
| traversable | проходимый |
| traversal | обход; проход |
| traverse | проходить |
| type class | класс типов [Виль15] |
| type family | семейство типов [Виль15] |
| type synonym | синоним типа |
| unification | унификация |
| view pattern | отображаемый образец [Виль15] |

#### Персоналии

| Иностранное | Русское |
|---|---|
| Alejandro Serrano Mena | Алехандро Серано Мена [Виль15] |
| Bartosz Milewski | Бартош Милевски |
| Edward Kmett | Эдвард Кметт |
| Gabriel Gonzalez | Габриэль Гонсалес |
| Haskell Curry | Хаскелл Карри |
| Haskell | Хаскель [Душ12] |
| Michael Snoyman | Майкл Снойман |
| Miran Lipovača | Миран Липовача [Душ12] |
| Moses Schönfinkel | Моисей Эльевич Шейнфинкель |
| Per Martin-Löf | Пер Мартин-Лёф |
| Richard Eisenberg | Ричард Айзенберг |
| Simon Marlow | Саймон Марлоу [Браг13М] |
| William Alvin Howard | Уильям Ховард |

#### Источники

* **Доб06**: «Структура и интерпретация компьютерных программ».
  Харольд Абельсон, Джеральд Сассман.
  Перевод издательства «Добросвет»
* **Зеф09**: «Лень бояться». Сергей Зефиров.
  «Практика функионального программирования», 2009, выпуск 1.
  http://fprog.ru/2009/issue1/serguey-zefirov-lazy-to-fear/
* **Душ12**: Изучай Haskell во имя добра! Миран Липовача.
  Редакторы перевода: Роман Викторович Душкин, Виталий Николаевич Брагилевский.
* **Браг13**: Жемчужины проектирования алгоритмов: функциональный подход.
  Ричард Бёрд.
  Перевод: Виталий Николаевич Брагилевский, Артём Михайлович Пеленицын.
* **Браг13М**: Параллельное и конкурентное программирование на языке Haskell.
  Саймон Марлоу. Перевод: Виталий Николаевич Брагилевский.
* **Брон14**: Типы в языках программирования. Бенджамин Пирс.
  Перевод: Георгий Бронников, Алекс Отт.
* **Виль15**: Изучаем Haskell. Алехандро Серано Мена. Перевод: Н. Вильчинский.
* **Браг16**: Введение в теорию языков программирования.
  Жиль Довек, Жан-Жак Леви.
  Перевод: Виталий Николаевич Брагилевский, Артём Михайлович Пеленицын.
* **Куз19**: Степан Львович Кузнецов.
  Программа дисциплины «Функциональное программирование».
  https://www.hse.ru/ba/ami/courses/292683695.html
