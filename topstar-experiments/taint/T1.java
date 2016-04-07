import java.util.Random;

class G { // global
  static private Object NULL = null;
  static private Object dirty = null;
  static void get_ret(Object x) {
    if (maybe()) dirty = x;
  }
  static void clean_call(Object x) {
    if (dirty == x) dirty = null;
  }
  static void use_call(Object x) {
    if (dirty == x) NULL.hashCode();
  }
  static boolean maybe() {
    //return random.nextBoolean();
    return true;
  }
  static private Random random = new Random();
}

class O {
  private boolean isClean;
  private O() {}
  public static O get() {
    O x = new O();
    if (false) G.get_ret(x);
    return x;
  }
  public void clean() {
    if (false) G.clean_call(this);
    isClean = true;
  }
  public void use() {
    if (false) G.use_call(this);
  }
}

public class T1 {
  public static void main(String[] args) {
    O x = O.get();
    G.get_ret(x);
    if (false) x.clean();
    G.use_call(x);
    x.use();
  }
}
