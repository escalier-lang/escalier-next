type Extends<X, Y, Z> = (X extends Y ? X : (Y extends Z ? Y : never));
// expansion - 5 | 3
type Foo = Extends<5 | 10, 2 | 3 | 5 | 7, 3 | 6 | 9>;
