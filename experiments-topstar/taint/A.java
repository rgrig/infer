class B { static void f() { assert false; } }
class C { static void f() { while (true); } }
public class A { public static void main() { B.f(); C.f(); } }
