package src;

/**
 * The quadtree is a tree data structure in which each internal node
 * has exactly four children. Quadtrees are used to partition a two  
 * dimensional space by recursively subdividing it into four quadrants/regions. 
 */

public class QuadTree {
    /** Region/Qadrant positions T: TOP, D: DOWN, R: RIGHT, L: LEFT */ 
    private static final int TL = 0;
    private static final int TR = 1;
    private static final int BL = 2;
    private static final int BR = 3;

    /**
     * There are 3 types of node
     * 
     * 1. Point node: Used to represent of a point. Is always a leaf node.
     * 
     * 2. Empty node: Used as a leaf node to represent that no point exist in the region it represent.
     * The point (-1, -1) indicates an empty node.
     * 
     * 3. Region node: It has 4 children nodes that can either be a point node or empty node.
     * Null value for field "point" indicates an region node.
     */

    /** Point field used to indicate type of the node 
     * for region node: null  
     * for empty node : (-1, -1)
     * for point node : (x, y), (x != -1 && y != -1)  */
    private Point point;

    /** Quadtree region boundries (only region node has boundry information) */   
    private Point topLeft;
    private Point bottomRight;

    /** 4 children of Quadtree (only region node has children) */
    private QuadTree[] children; 

    /** Constructs an empty node QuadTree */
    public QuadTree() {
        // to indicate an empty node set point as (-1, -1)
        // boundries (topLeft, bottomRight) and childs are not used in empty node
        point = new Point(); // (-1, -1)
    }

    /**
     * Constructs a point node QuadTree
     * @param x x position of point
     * @param y y position of point
     */
    public QuadTree(int x, int y) {
        point = new Point(x, y);
    }

    /**
     * Constructs a region node QuadTree with given cordinates of rectangular space
     * @param x1 x value of top left
     * @param y1 y value of top left
     * @param x2 x value of bottom right
     * @param y2 y value of bottom right
     */
    public QuadTree(int x1, int y1, int x2, int y2) {
        // make sure given values are proper for rectangular space
        if (x2 < x1 || y2 < y1)
            throw new IllegalArgumentException("Correct coordinates: x1 < x2 and y1 < y2\n");
        // set point to null to indicate this is an region node 
        point = null;
        // define the space boundries
        topLeft = new Point(x1, y1);
        bottomRight = new Point(x2, y2);
        // region node has four children and initialy all of are empty node
        children = new QuadTree[4]; 
        for (int i = 0; i < 4; ++i) 
            children[i] = new QuadTree(); // empty node QuadTree
    }

    /**
     * Inserts given point to the QuadTree
     * @param x x position of point
     * @param y y position of point
     * @return True if the point is inserted successfully, otherwise false
     */
    public boolean insert(int x, int y) {
        // make sure given point is not out of boundry
        if (!isInBoundry(x, y))
            return false;
        else
            return insert(new Point(x, y));    
    }

    /**
     * Inserts given point to the QuadTree
     * Insertion can be done in 3 ways
     * 1. Insertion at region node: Find the related subregion recursivly and insert 
     * 2. Insertion at empty node: Set point as given point
     * 3. Insertion at point node: Create an subregion and insert there the existing point
     * and the new one
     * @param p The Point which is inside of the region boundry
     * @return True for succesfull insertion
     */
    private boolean insert(Point p) {
        int q = quadrantOf(p); // related quadrant of the given point p
        
        if (children[q].point == null) { // region node
            // for a region node, insert the point to the related quadrant
            return children[q].insert(p);
        } 
        else if (children[q].point.x == -1 && children[q].point.y == -1) { // empty node
            // insert the point as new point node QuadTree
            children[q].point = p;
            return true;
        }
        else { // point node
            // there already an exist an point and since point node can only hold single point  
            // convert point node to region node to fit two point inside it (at most 4 point)
            int midx = (topLeft.x + bottomRight.x) / 2;
            int midy = (topLeft.y + bottomRight.y) / 2;
            // save the existing point to insert new created region
            Point tmp = children[q].point;
            // create a new region/quadrant
            switch (q) {
                case TL:
                    children[q] = new QuadTree(topLeft.x, topLeft.y, midx, midy);
                    break;
                case TR: 
                    children[q] = new QuadTree(midx + 1, topLeft.y, bottomRight.x, midy);
                    break;   
                case BL: 
                    children[q] = new QuadTree(topLeft.x, midy + 1, midx, bottomRight.y);
                    break;   
                case BR: 
                    children[q] = new QuadTree(midx + 1, midy + 1, bottomRight.x, bottomRight.y);
                    break;   
            }
            // insert the two points to the new created region
            children[q].insert(tmp);
            children[q].insert(p);
            return true;
        }
    }

    /**
     * Searchs the given point inside of the QuadTree
     * @param x x position of point
     * @param y y position of point
     * @return True if QuadTree contains the point (x, y), otherwise false
     */
    public boolean find(int x, int y) {
        return isInBoundry(x, y) ? find(new Point(x, y)) : false; 
    }

    /**
     * Searchs the given target point inside of the QuadTree
     * @param point Target point that is searching
     * @return True if QuadTree contains the point p, otherwise false
     */
    private boolean find(Point p) {
        // find the quadrant of the point
        QuadTree subQuad = children[quadrantOf(p)];
        // for region node continue searching in subregion
        if (subQuad.point == null)
            return subQuad.find(p); 
        // encounturing empty node indicates point doesn't exist
        else if (subQuad.point.x == -1) 
            return false; 
        // for point node compare the existing point and the target point
        else  
            return p.x == subQuad.point.x && p.y == subQuad.point.y;
    }

    /**
     * Checks if the given cordinates inside the Quadtree 
     * @param x x position
     * @param y y position
     * @return True if the Quadtree contains the point (x, y), otherwise false
     */
    public boolean isInBoundry(int x, int y) {
        return topLeft.x <= x && x <= bottomRight.x &&
            topLeft.y <= y && y <= bottomRight.y;
    }

    /**
     * Finds the quadrant of given point
     * pre: current node must be region node 
     * @param p The point
     * @return The quadrant of the point p
     */
    private int quadrantOf(Point p) {
        int midx = (topLeft.x + bottomRight.x) / 2;
        int midy = (topLeft.y + bottomRight.y) / 2;

        if (p.y < midy) { // top quadrant
            if (p.x < midx)
                return TL;
            else
                return TR;
        }
        else { // bottom quadrant
            if (p.x < midx) 
                return BL;
            else
                return BR;
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        toString(this, 1, sb);
        return sb.toString();
    }

    private void toString(QuadTree t, int depth, StringBuilder sb) {
        for (int i = 0; i < depth; ++i)
            sb.append("  ");
        // there exist three different node and each of them has different output
        // first decide the type of the node
        if (t.point == null) {
            // convert string the subregion
            sb.append("*\n"); // indicates region node
            for (var child : t.children)
                toString(child, depth + 1, sb);
        }
        else if (t.point.x == -1) 
            sb.append("null\n");
        else { // just append the point
            sb.append(t.point.toString());
            sb.append("\n");
        } 
    }

    /** Static inner class Point two represent 2D locations */
    public static class Point {
        private int x; // x position in 2D space
        private int y; // y position in 2D space

        /** Construct a new point (x, y) 
         * @param x x position of point 
         * @param y y position of point 
        */
        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        /** Construct a new point (-1, -1) */
        public Point() {
            this(-1, -1);
        }

        @Override
        public String toString() {
            return String.format("(%d, %d)", x, y);
        }
    }
}
