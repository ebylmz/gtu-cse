package src.hashing;

/** The Entry class for key-value pairs */
public class Entry<K, V> {
    /** The key which is an comparable object */
    protected K key;  
    /** The value */
    protected V value;    

    /**
     * Constructs a new key-value pair
     * @param key The key
     * @param value The value
     */
    public Entry(K key, V value) {
        this.key = key;
        this.value = value;
    }

    /**
     * Retrieves the key 
     * @return The key
     */
    public K getKey() {
        return key;
    }

    /**
     * Retrieves the value
     * @return The value
     */
    public V getValue() {
        return value;
    }

    /**
     * Sets new value 
     * @param value
     * @return The value before setting new value
     */
    public V setValue(V value) {
        V oldVal = value;
        this.value = value;
        return oldVal;
    }

    /*** Converts an Entry object to string as (key, value) */
    @Override
    public String toString() {
        return String.format("(%s, %s)", key, value);
    }
}
