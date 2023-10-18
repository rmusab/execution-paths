public class MainClass {
    public static int getISmtokenPos(String str, int savedPos) {
        int num = -1;
        for (int i = savedPos - 1; i >= 0; i--) {
            if (isDSeparator(str.charAt(i))) {
                if (i >= 2 && isDSeparator(str.charAt(i - 2))) {
                    num = i - 2;
                } else {
                    num = i;
                }
            }
            if (num < 1) {
                num = -10;
            }
        }
        return num;
    }
}