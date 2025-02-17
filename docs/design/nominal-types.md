# Nominal Types

```ts
class Foo<T> {
  msg: T;
}
class Bar<T> {
  msg: T;
}
const foo = new Foo<string>();
const bar = new Bar<string>();
const invalid: Bar<string> = foo; // Error
```

Schemes don't include the name of the type.
Relying on the name is insufficient because we could be importing the type from another module
and it could have the same name in that module but be imported as a different name here.

We can't track the nominality of a type by adding a 'Nominal' field to 'Scheme'.  This is because
we create schemes with placeholder types.

Instead, we need to track the nominality of a type by adding a 'Nominal' field to our object
type. 

Additionally, when we need to change how we expand types when check if two nominal types are
equal.  Right now we create new object types and replace all type params with appropriate type
arguments.  We'd like to avoid creating a new object type since it's expensive but also because
then we wouldn't be able to check if the two object types are refrencially equal.
