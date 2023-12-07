package src.entry;

import src.graph.Vertex;

/**
 * A class to keep the edge information such as 
 * destination vertex and the weight of the edge 
 */
public class DestEntry extends EdgeEntry {
    /** Default weight for the edge */
    public static final double DEFAULT_WEIGHT = Double.POSITIVE_INFINITY; 
    /** Weight of the edge betweeen source to dest vertices */
    private double weight; 

    /**
     * Constructs an DestEntry with given parameters
     * @param dest The destination vertex of the edge
     * @param weight Weight of the edge
     */
    public DestEntry(Vertex dest, double weight) {
        super(dest);
        this.weight = weight;
    }

    /**
     * Constructs an DestEntry given destination vertex. 
     * Sets the weight of the edge as DEFAULT_WEIGHT.
     * @param dest The destination vertex of the edge
     * @param weight Weight of the edge
     */
    public DestEntry(Vertex dest) {
        this(dest, DEFAULT_WEIGHT);
    }

    /**
     * Returns the destionation vertex of the edge
     * @return Destination vertex of the edge
     */
    public Vertex getDestVertex() {
        return v;
    }

    /**
     * Returns the weight of the edge between source and dest vertices 
     * @return Weight of the edge between source and dest vertices
     */
    public double getEdgeWeight() {
        return weight;
    }

    /**
     * Sets the weight of the edge
     * @param weight Weight of the edge
     * @return Previous edge weight
     */
    public double setEdgeWeight(double weight) {
        double r = this.weight;
        this.weight = weight;
        return r;
    }

    /**
     * Determines whether the two objects are equal or not.
     * Two DesEntry objects are equal if their destination vertices are equal.
     * @return If the two DestEntry object has same 
     * vector dest, then returns true. Otherwise returns false
     */
    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}