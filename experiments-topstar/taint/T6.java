import java.util.Random;

class ToplProperty { // global
  static private int state;
  static private Object r0;

  static void start() {
    state = 0;
    r0 = null;
  }

  static void get_ret(Object l0) {
    if (state == 0) {
      if (maybe() && true) {
        // stay here
      } else if (maybe() && true) {
        state = 1;
        r0 = l0;
      } else if (true) {
        // stay here
      } else if (true) {
        state = 1;
        r0 = l0;
      }
    }
  }

  static void go_call_1(Object l0) {
    if (maybe() && r0 == l0) {
      state = 2;
    } else if (r0 == l0) {
      state = 2;
    }
  }

  static void go_call_2(Object l0) {
    if (maybe() && r0 == l0) {
      while (true);
    } else if (r0 == l0) {
      while (true);
    }
  }

  static void go_call(Object l0) {
    if (state == 0) {
      // skip
    } else if (state == 1) {
      go_call_1(l0);
    } else if (state == 2) {
      go_call_2(l0);
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
    ToplProperty.start();
    O x = O.get();
    O y = O.get();
    y.go();
    x.go();
    x.go();
  }
}
