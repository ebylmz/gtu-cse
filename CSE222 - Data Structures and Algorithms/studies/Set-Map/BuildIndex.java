import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;
import java.util.StringTokenizer;

public class BuildIndex {
    /** Driver for BuildIndex class */
    public static void main(String[] argv) {
        BuildIndex bi = new BuildIndex();
        try {
            bi.build(new Scanner(new File("Text.txt")));
            var word = "Rabbit";
            System.out.println("Indexes of " + word);
            System.out.println(bi.findIndexes(word));

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    /** 
     * A map to reach all the indexes of a word
     * key: word
     * value: all the indexes of the word
     */
    private HashMap<String, ArrayList<Integer>> index;
    /** Number of lines */
    private int numLine;

    public BuildIndex() {
        index = new HashMap<>();
    }

    public void build(Scanner scan) {
        while (scan.hasNextLine()) {
            ++numLine;
            StringTokenizer st = new StringTokenizer(scan.nextLine());
            while (st.hasMoreTokens()) {
                String word = st.nextToken();
                // make sure to get the mapping value of word
                var lines = index.get(word);
                if (lines == null) {
                    lines = new ArrayList<>();
                    index.put(word, lines);
                }
                // to do not add the same line for same word
                if (! lines.contains(numLine))
                    lines.add(numLine);
            }
        }
    }

    public String findIndexes(String word) {
        var lines = index.get(word);
        if (word == null)
            return null;
        StringBuilder sb = new StringBuilder();
            for (int i = 0; i < lines.size(); ++i) {
                sb.append(lines.get(i));
                sb.append("\n");
            }
        return sb.toString();
    }
}
