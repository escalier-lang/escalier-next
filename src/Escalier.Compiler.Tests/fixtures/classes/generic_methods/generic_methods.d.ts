// @escType - {new fn () -> Foo, snd fn <A, B>(a: A, b: B) -> B}
const Foo: {
  new (): Foo;
  snd<A, B>(a: A, b: B): B;
};
