public class NestedIfTest {
    public static void main(String[] args) {
        int a = 1;
        int b = 2;

        if (a > 0) {
            int c = a + b;
            if (c > 2) {
                int d = c + a;
                if (d < 765) {
                    if (d > 4) {
                        int e = d + b;
                    }
                    int s = 333;
                }

                if (c == 5){
                    if (c == 77){
                        int e = b;
                    }
                }
            }
        }
    }
}