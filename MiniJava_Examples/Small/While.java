class While {
    public static void main(String[] argv) {
        System.out.println(new WhileClass().clear());
    }
}

class WhileClass {

    int size;
    int[] elements;


    public int clear() {
      int i;
      int x;
      int b;
      int c;
      int d;

      x =10;
      b =5;
      c = 4;
      d = 12;

      i = x+ (x*b) -c + d;

      System.out.println(i);

      return x;

    }

}
