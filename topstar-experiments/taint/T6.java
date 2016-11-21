import java.util.Random;

class ToplProperty { // global
  static private int state = 0;
  static private Object r0 = null;

  static void get_ret(Object l0) {
    while (true);/*
    if (state == 0) {
      if (maybe()) {
        // stay here
      } else {
        state = 1;
        r0 = l0;
      }
    }*/
  }

  static void go_call(Object l0) {
    if (state == 1 && r0 == l0) {
      state = 2;
    } else if (state == 2 && r0 == l0) {
      while (true);
    }
  }

  static boolean maybe() {
    return random.nextBoolean();
  }
  static private Random random = new Random();
}

class O {
  private O() {}
  public static O get() {
    O r = new O();
    ToplProperty.get_ret(r);
    return r;
  }
  public void go() {
    ToplProperty.go_call(this);
  }
}

public class T6 {
  public static void main(String[] args) {
    O x = O.get();
    O y = O.get();
    y.go();
    x.go();
    x.go();
  }
}
