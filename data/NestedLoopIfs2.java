import java.awt.Desktop;
import java.io.File;
import java.io.IOException;

public class MainClass {
    public static File getISmtokenPos(String str, int savedPos) {
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

        File file = new File("/Users/pankaj/source.txt" + Integer.toString(num));
        if (!Desktop.isDesktopSupported()) {
            System.out.println("Desktop is not supported");
            return;
        }
        Desktop desktop = Desktop.getDesktop();
        if (file.exists()) desktop.open(file);
        return file;
    }
}