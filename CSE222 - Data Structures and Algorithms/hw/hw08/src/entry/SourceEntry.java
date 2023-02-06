package src.entry;

import java.util.LinkedList;

import src.graph.Vertex;

/**
 * A class to keep the edge information such as 
 * source vertex and all the edges related to it 
 * as list of DestEntry objects 
 */
public class SourceEntry extends EdgeEntry {
    /** Destination vertices */
    private LinkedList<DestEntry> destEntries; 

    /**
     * Constructs an SourceEntry with given parameters
     * @param source The source vertex of the edge
     * @param weight Weight of the edge
     */
    public SourceEntry(Vertex source, LinkedList<DestEntry> destEntries) {
        super(source);
        this.destEntries = destEntries;
    }

    /**
     * Returns the source vertex of the edge
     * @return Source vertex of the edge
     */
    public Vertex getSourceVertex() {
        return v;
    }

    /**
     * Returns the list of edges as list of DesEntry objects
     * @return List of DestEntry objects
     */
    public LinkedList<DestEntry> getDestEntries() {
        return destEntries;
    }

    /**
     * Determines whether the two objects are equal or not.
     * Two SourceEntry objects are equal if their source vertices are equal.
     * @return If the two SourceEntry object has same 
     * vector source, then returns true. Otherwise returns false
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