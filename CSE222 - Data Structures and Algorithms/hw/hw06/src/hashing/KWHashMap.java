/** 
 * An interface for HashMap
 *  @author Koffman and Wolfgang
 */

package src.hashing;

public interface KWHashMap <K, V> {
    /**
     * Gets the value associated with the given key
     * @param key The key will be searched 
     * @return If key is found returns the value associated with the key otherwise, null
     */
    V get(K key);

    /**
     * Puts a new key-value pair or modifies the current pair if the given key exist
     * @param key The key
     * @param value The Value
     * @return The old value associated with this key if found otherwise, null
     */
    V put(K key, V value);

    /**
     * Removes the given key-value pair
     * @param key The key will be searched
     * @return If the key is found then returns removed value 
     * associated with the key otherwise, null
     */
    V remove(K key);

    /**
     * Size of the hash-map
     * @return Size of the hash-map
     */
    int size();

    /**
     * Checks whether the map is empty or not
     * @return True if the map is empty
     */
    boolean isEmpty();
}