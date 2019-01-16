// Infer doesn't warn on the bug.

import java.util.Random;

class Event {
    final int id;
    final Object[] values;

    StackTraceElement[] callStack;

    public Event(int id, Object[] values) {
        this.id = id;
        this.values = values;
    }
}


class ToplProperty { // global
  static int state = 0;
  static Object r0 = null;

  static void check(Event event) {
    if (event.id == 0) { // get_ret
      if (maybe()) {
        // stay here
      } else {
        state = 1;
        r0 = event.values[0];
      }
    } else if (event.id == 1) { // go_call
      if (state == 1 && r0 == event.values[0]) {
        state = 2;
      } else if (state == 2 && r0 == event.values[0]) {
        while (true);
      }
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
    ToplProperty.check(new Event(0, new Object[]{r}));
    return r;
  }
  public void go() {
    ToplProperty.check(new Event(1, new Object[]{this}));
  }
}

public class T5 {
  public static void main(String[] args) {
    O x = O.get();
    O y = O.get();
    y.go();
    x.go();
    x.go();
  }
}
