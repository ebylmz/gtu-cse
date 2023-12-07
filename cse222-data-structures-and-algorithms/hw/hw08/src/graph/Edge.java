package src.graph;

public class Edge {
    /** The source vertex */
    private int source;

    /** The destination vertex */
    private int dest;
    
    /** Weight of the edge */
    private double weight;

    /**
     * Construct an edge with given source and destination IDs.
     * Sets the weight 0.0 as default value.  
     * @param source The ID of the source vertex
     * @param dest The ID of the destination vertex
     */
    public Edge(int source, int dest) {
        this.source = source;
        this.dest = dest;
        weight = 0l;
    }

    /**
     * Construct an edge with given weight, source and destination IDs.
     * @param source The ID of the source vertex
     * @param dest The ID of the destination vertex
     * @param weight The weight of the edge
     */
    public Edge(int source, int dest, double weight) {
        this.source = source;
        this.dest = dest;
        this.weight = weight;
    }

    /**
     * Returns the ID of the source vertex
     * @return The ID of the source vertex
     */
    public int getSource() {
        return source;
    }
    
    /**
     * Returns the ID of the destination vertex
     * @return ID of the destination vertex
     */
    public int getDest() {
        return dest;
    }
    
    /**
     * Returns the weight of the edge
     * @return The weight of the edge
     */
    public double getWeight() {
        return weight;
    }

    @Override
    public String toString() {
        return String.format(
            "{%d, %d, %.1f}", 
            source, dest, weight);
    }
    
    /**
     * Return true if two edges are equal. Edges
     * are equal if the source and destination
     * are equal. Weight is not considered.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj instanceof Edge) {
            Edge other = (Edge) obj;
            return other == this || 
                (dest == other.dest && source == other.source);
        }
        return false;
    }

    /**
     * Return a hash code for an edge. 
     * The hash code is the source shifted
     * 16 bits exclusive or with the dest.
     * @return A hash code for an edge
     */
    @Override
    public int hashCode() {
        return (source << 16) ^ dest;
    }
}