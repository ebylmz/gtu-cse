package src.graph;

import java.util.HashMap;

public class Vertex {
    /** Default weight for the vertex */
    public static final double DEFAULT_WEIGHT = Double.POSITIVE_INFINITY; 

    /** Vertex index or ID */
    private int id;

    /** Label of the vertex */
    private String label;
    
    /** Weight of the vertex */
    private double weight;
    
    /** User-defined additional properties */
    private HashMap<String, String> properties; 

    /**
     * Constructs a vertex with given properties 
     * @param id Vertex index or ID
     * @param label Label of the vertex
     * @param weight Weight of the vertex  
     */
    public Vertex(int id, String label, double weight) {
        this.id = id;
        this.label = label;
        this.weight = weight;
        properties = new HashMap<>();
    }

    /**
     * Constructs a vertex with given properties 
     * @param id Vertex index or ID
     * @param label Label of the vertex
     */
    public Vertex(int id, String label) {
        this(id, label, DEFAULT_WEIGHT);
    }

    /**
     * Constructs a vertex with the given ID. 
     * Note that default value for label is null,
     * and 0 for weight. 
     * @param id Vertex index or ID
     */
    public Vertex(int id) {
        this(id, null, DEFAULT_WEIGHT);
    }

    /**
     * Returns the vertex ID of the the graph
     * @return The vertex ID
     */
    public int getID() {
        return id;
    }
    
    /**
     * Returns the label of the vertex
     * @return The label of the vertex
     */
    public String getLabel() {
        return label;
    }
    
    /**
     * Returns the weight of the vertex
     * @return The weight of the vertex
     */
    public double getWeight() {
        return weight;
    }
    
    /**
     * Returns a map of user-defined additional properties
     * @return A map of user-defined additional properties
     */
    public HashMap<String, String> getProperties() {
        return properties;
    }

    /**
     * Sets the map of user-defined additional properties
     * @param properties A map of user-defined additional properties
     */
    public void setProperties(HashMap<String, String> properties) {
        this.properties = properties;
    }

    /**
     * Adds a property to the vertex
     * @return If the key is found then return the previous value of 
     *         property otherwise returns null.
     */
    public String addProperty(String key, String value) {
        return properties.put(key, value);
    }

    /**
     * Removes a property of the vertex
     * @return If the key is found then return the value of 
     *         removed property otherwise returns null.
     */
    public String removeProperty(String key) {
        return properties.remove(key);
    }

    /**
     * Get the additional property specified with the key
     * @param key The key 
     * @return If the key is found then returns the value which 
     *         paired with the key. Otherwise returns null.
     */
    public String getProperty(String key) {
        return properties.get(key);
    } 

    /**
     * Sets the ID of the vertex. 
     * Note: ID should be nonnegative integer
     * @param id Vertex ID
     * @return if the ID is proper (id >= 0)
     */
    public boolean setID(int id) {
        if (id < 0)
            return false;
        this.id = id;
        return true;
    }

    /**
     * Sets the label of the vertex
     * @param label New label
     * @return Previous label
     */
    public String setLabel(String label) {
        var r = label;
        this.label = label;
        return r;
    }

    /**
     * Sets the weight of the vertex
     * @param weight New weight
     * @return Previous weight
     */
    public double setWeight(double weight) {
        var r = weight;
        this.weight = weight;
        return r;
    }

    /**
     * Returns the string representation of the Vertex object.
     * @return "(id, label, weight)"
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("(" + id);
        if (label != null) sb.append(", " + label);
        if (weight != DEFAULT_WEIGHT) sb.append(", " + weight);
        sb.append(")");
        return sb.toString();
    }

    /**
     * Two vertices are equal if their IDs are same
     */
    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj instanceof Vertex) {
            Vertex other = (Vertex) obj;
            return id == other.getID();
/*
            // compare the properties
            if (properties != null) {
                if (other.properties == null || !properties.equals(other.properties))
                    return false;
            }
            else if (other.properties != null) // this.properties is null
                return false;
            // compare the id, label and weight properties
                return id == other.id && label.equals(other.label) && weight == other.weight;
  */
        }
        return false;
    }

    /**
     * Calculates the hash code of the Vertex object 
     * according to only vertex ID. Note that two 
     * vertices are equal if their IDs are same.
     * @return vertex ID
     */
    @Override
    public int hashCode() {
        // var w = Double.doubleToLongBits(weight);
        // return id + (int)(w ^ w >>> 32) + label.hashCode();
        return id;
    }
}