public class Arrays {
    public static void main (String[] args) {
        // arrays are objects and they're created dynamicly with keyword new
    
        int[] a1 = new int[20];
        int[] a2 = {1, 2, 3, 4, 5, 6};

        System.out.println("a1:");
        for (var v : a1) System.out.printf("%d ", v);
        System.out.println();
     
        System.out.println("a2:");
        for (var v : a2) System.out.printf("%d ", v);
        System.out.println();

        // create an 2D array 
        int[][] a3 = new int[10][10];

        for (int i = 0; i < a3.length; ++i)
            for (int j = 0; j < a3[i].length; ++j)
                a3[i][j] = i * a3.length + j;

        System.out.println("a3:");
        for (int i = 0; i < a3.length; ++i) {
            for (int j = 0; j < a3[i].length; ++j)
                System.out.printf("%2d ", a3[i][j]);
            System.out.println();
        } 
    }

    // create an 4D array 
    int[][][][] a4 = new int[5][10][15][20];
}
