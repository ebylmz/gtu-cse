package src.entry;

import src.graph.Vertex;

public abstract class EdgeEntry {
    /** Source vertex */
    protected Vertex v;

    /**
     * Constructs an SourceEntry with given parameters
     * @param v A vertex of the edge
     */
    public EdgeEntry(Vertex v) {
        this.v = v;
    }

    /**
     * Returns the source vertex of the edge
     * @return Vertex of the edge
     */
    public Vertex getVertex() {
        return v;
    }

    /**
     * Determines whether the two objects are equal or not.
     * Two EdgeEntry objects are equal if their source vertices are equal.
     * @return If the two EdgeEntry object has same 
     * vector v, then returns true. Otherwise returns false
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof EdgeEntry))
            return false;
        return v.equals(((EdgeEntry) obj).getVertex());
    }

    @Override
    public int hashCode() {
        return v.hashCode();
    }
}
