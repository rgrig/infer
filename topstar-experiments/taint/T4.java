import java.util.Random;

class G { // global
  static private Object tracked = null;
  static private int state = 0;
  static void get_ret(Object x) {
    if (state == 0) while (true);
    //if (maybe()) tracked = x;
  }
  static void go_ret(Object x) {
  }
  static void go_call(Object x) {
    if (x == tracked) {
      if (state == 0) state = 1;
      else if (state == 1) {while (true);}
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
    G.get_ret(r);
    return r;
  }
  public void go() {
    G.go_call(this);
  }
}

public class T4 {
  public static void main(String[] args) {
    O x = O.get();
    O y = O.get();
    y.go();
    x.go();
    x.go();
  }
}
