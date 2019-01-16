public class T2 {
  void assertF(String o) { o.toString(); }

  void f(Object x, Object y) {
    if (x != y) assertF(null);
  } // gives warning even if g is commented out
  /*
  void g() {
    f(1, 2); // ok
    f(null, null); // nok
  }
  */
}
