// expected output: 15

class Sum {
    public static void main(String[] a) {
        // f(x,y) = x + sum{i=1..y) i
        System.out.println((new SumClass()).f(false, 5));
    }
}

class SumClass {
    boolean a; // remember last sum encountered in f

    public int f(boolean y, int y) {



      if (!a) {
        System.out.println(2);
      } else {}

        return 0;
    }

    public int g(int x) {
        return x;
    }
}
