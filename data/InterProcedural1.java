import java.awt.Desktop;
import java.io.File;
import java.io.IOException;

public class MainClass {
    public static void main(String[] args) {
        String userInput = getSourceFromUser();
        processInput(userInput);
    }

    public static String getSourceFromUser() {
        // This could be a user-provided value or any other source.
        return "sampleInput";
    }

    public static void processInput(String input) {
        int position = calculatePosition(input);
        File file = getFileFromPosition(input, position);
        if (file != null) {
            accessFile(file);
        }
    }

    public static int calculatePosition(String str) {
        int savedPos = str.length();
        return getISmtokenPos(str, savedPos);
    }

    public static File getFileFromPosition(String str, int savedPos) {
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

        return new File("/Users/pankaj/source.txt" + Integer.toString(num));
    }

    public static void accessFile(File file) {
        if (!Desktop.isDesktopSupported()) {
            System.out.println("Desktop is not supported");
            return;
        }
        Desktop desktop = Desktop.getDesktop();
        try {
            if (file.exists()) {
                desktop.open(file);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean isDSeparator(char c) {
        return c == '.'; // Sample implementation; modify as needed.
    }
}