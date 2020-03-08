---
title: Writing a Type-Level Switch Statement in TypeScript
author: Peter Stuart
---

I recently came across a situation where I needed to write a very long conditional type[^conditional-types] that looked something like this:

[^conditional-types]: Conditional types have the format <span class="nobr">`T extends U ? X : Y`</span>. Read more about them in [the TypeScript docs][conditional-types].

```typescript
type Cond<Value,
  Match1, Result1,
  Match2, Result2,
  Match3, Result3,
  ...> =
  Value extends Match1
    ? Result1
    : Value extends Match2
      ? Result2
      : Value extends Match3
        ? Result3
        : ...

type Test1 = Cond<string,
  number, "number",
  boolean, "boolean",
  string, "string"
>
// Test1 = "string"
```

Rather than nesting `extends` expressions, which only supports a fixed number of conditions and results in a [pyramid of doom][pyramid-of-doom] in the definition of `Cond`, this could be better expressed using a `Switch` type:

```typescript
type Switch<Value, [
  [Match1, Result1],
  [Match2, Result2],
  [Match3, Result3],
  ...
]> = ...

type Test1 = Switch<string, [
  [number, "number"],
  [boolean, "boolean"],
  [string, "string"]
]>;
// Test1 = "string"
```

`Switch` supports an arbitrary number of conditions, and we can write a recursive definition for it.

## Defining Switch

To iterate through the array of conditions, I use the `List.Head` and `List.Tail` types from the excellent [ts-toolbelt][ts-toolbelt] library:

```typescript
import {List} from 'ts-toolbelt';

type Test1 = List.Head<boolean, string, number>;
// Test1 = boolean

type Test2 = List.Tail<boolean, string, number>;
// Test2 = [string, number]
```

The most obvious way to write the `Switch` type would be something like this:

```typescript
type Switch<T, Conditions extends Array<[any, any]>> = 
  List.Head<Conditions> extends never
    ? never
    : T extends List.Head<Conditions>[0]
      ? List.Head<Conditions>[1]
      : Switch<T, List.Tail<Conditions>>;
```

... but TypeScript doesn't allow recursive type definitions:

```typescript
// Error: Type alias 'Switch' circularly references itself
```

To work around that, we need to add a level of indirection by indexing into an object:

```typescript
import {List} from 'ts-toolbelt';

type Switch<T, Conditions extends Array<[any, any]>> =
  {
    hasCondition:
      T extends List.Head<Conditions>[0]
        ? List.Head<Conditions>[1]
        : Switch<T, List.Tail<Conditions>>;
    hasNoConditions:
      never;
  }[List.Head<Conditions> extends never
    ? 'hasNoConditions'
    : 'hasCondition'];
```

The compiler is happy, and we can confirm that it works with a few test types:

```typescript
type Test1 = Switch<string, [
  [number, "number"],
  [boolean, "boolean"],
  [string, "string"]
]>;
// Test1 = "string"

type Test2 = Switch<object, [
  [number, "number"],
  [boolean, "boolean"],
  [string, "string"]
]>;
// Test2 = never

type Test3 = Switch<string, []>;
// Test3 = never
```

For explanations of conditional types, implementing recursive types, and other advanced type techniques, check out these articles:

- [Advanced Types][advanced-types] in the TypeScript docs
- [How to master advanced TypeScript patterns][master-advanced-typescript-patterns] by Pierre-Antoine Mills (the author of [ts-toolbelt][ts-toolbelt])

[pyramid-of-doom]: https://en.wikipedia.org/wiki/Pyramid_of_doom_(programming)
[ts-toolbelt]: https://github.com/pirix-gh/ts-toolbelt
[advanced-types]: https://www.typescriptlang.org/docs/handbook/advanced-types.html
[conditional-types]: https://www.typescriptlang.org/docs/handbook/advanced-types.html#conditional-types
[master-advanced-typescript-patterns]: https://www.freecodecamp.org/news/typescript-curry-ramda-types-f747e99744ab/

