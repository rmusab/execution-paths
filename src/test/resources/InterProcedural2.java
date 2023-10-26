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
        return input.toUpperCase();
    }
}