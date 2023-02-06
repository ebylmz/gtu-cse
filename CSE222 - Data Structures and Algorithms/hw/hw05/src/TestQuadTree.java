package src;

/** TEST CASES
 * 1. Insert a point which is inside the boundary of the quadtree.
 * 2. Insert a point which is out of quadtree region.
 * 3. Search an existing item.
 * 4. Search a non existing item.
 * 5. Insert points (30,30), (20,15), (50,40), (10,12), (40,20), (25,60), (15,25)
 */

public class TestQuadTree {
    public static void test1() {
        // define an QuadTree which is bounded (0, 0) as topLeft, (100, 100) bottomRight
        QuadTree t = new QuadTree(0, 0, 100, 100);
        System.out.println("Initial quadtree");
        System.out.println(t);
        insertPoint(t, 30, 30);
        insertPoint(t, 20, 15); 
        insertPoint(t, 115, 25); // invalid point
        insertPoint(t, -5, 17); // invalid point 
        insertPoint(t, 0, -35); // invalid point 
        insertPoint(t, 50, 40); 
        insertPoint(t, 10, 12); 
        insertPoint(t, 40, 20); 
        insertPoint(t, 25, 60); 
        insertPoint(t, 15, 25); 
        // find the points in QuadTree
        System.out.println(t);
        System.out.printf("Find (15, 25): %b\n", t.find(15, 25));
        System.out.printf("Find (50, 40): %b\n", t.find(50, 40));
        System.out.printf("Find (30, 30): %b\n", t.find(30, 30));
        System.out.printf("Find (50, 70): %b\n", t.find(50, 70));
        System.out.printf("Find (150, 870): %b\n", t.find(150, 870));
    }

    private static void insertPoint(QuadTree t, int x, int y) {
        boolean r = t.insert(x, y);
        System.out.println(t);
        System.out.printf("INSERT (%d, %d): ", x, y);
        System.out.println(r ? "SUCCESS\n" : "FAILURE\n");
    }
}