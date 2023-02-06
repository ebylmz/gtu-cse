import java.util.StringTokenizer;

public class TestRBT {
    
    public static void main(String[] args) {
        test1();    
        // test2();    
    }

    public static void test1() {
        RedBlackTree<Integer> t = new RedBlackTree<>();
        // create a dataset to insert into the tree
        Integer[] dataset = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // add items to the tree
        for (var e : dataset)
            System.out.println("add " + e + " : " + t.add(e));
        // dipslay the tree
        System.out.println(t);
    }

    public static void test2() {
        RedBlackTree<String> t = new RedBlackTree<>();
        String sentence = "The quick brown fox jumps over the lazy dog";
        StringTokenizer st = new StringTokenizer(sentence);
        // add each word to the tree
        while (st.hasMoreTokens())
            t.add(st.nextToken());
        // dipslay the tree
        System.out.println(t);
    }
}
