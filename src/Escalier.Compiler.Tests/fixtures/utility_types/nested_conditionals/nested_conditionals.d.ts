type Extends<X, Y, Z> = (X extends Y ? X : (Y extends Z ? Y : never));
type Foo = 5 | 3;
