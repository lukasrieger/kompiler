// expected output: 15

class Plus {
    public static void main(String[] a) {
        // f(x,y) = x + sum{i=1..y) i
        System.out.println((new PlusClass()).f(5));
    }
}

class PlusClass {

    public int f(int x) {

      return x+5;
    }
}
