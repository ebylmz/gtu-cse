import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;

public class randomGenerator {
    public static void main(String[] args) {
        
        try (FileWriter fWriter = new FileWriter("random.txt")) {
            int n = (args.length == 1) ? Integer.parseInt(args[0]) : 100;

            Random rand = new Random();
            for (int i = 0; i < n; ++i)
                fWriter.write(rand.nextInt(100) - 50 + " ");
            System.out.printf("%d new random number generated\n", n);
        } 
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
