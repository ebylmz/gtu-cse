package src.hashing;

import src.bst.BinarySearchTree;

public class HashTableChaningTree<K extends Comparable<K>, V> implements KWHashMap<K, V> {
    /** An array of BST to chaining */
    private BinarySearchTree<CEntry<K, V>>[] table;
    /** The number of keys */
    private int numKeys;
    /** The initial capacity */
    private static final int INIT_CAPACITY = 13;
    /** The maximum load factor */
    private static final double LOAD_THRESHOLD = 3.0;

    @SuppressWarnings("unchecked")
    public HashTableChaningTree() {
        table = new BinarySearchTree[INIT_CAPACITY];
    }

    @Override
    public V get(K key) {
        // find the related slot of the given key
        int index = getTableSlot(key);
        var t = table[index];
        if (t == null) // table slot is empty, no key exist
            return null;
        else {
            // search the key from the tree. if it's found return the value
            CEntry<K, V> e = t.find(keyEntry(key));
            return (e != null) ? e.getValue() : null;
        }
    }

    @Override
    public V put(K key, V value) {
        V preVal = null; // previos value of key-value pair
        // find the slot of the key and insert the key-pair to the tree
        int index = getTableSlot(key);
        if (table[index] == null)
            table[index] = new BinarySearchTree<>();
        var t = table[index]; // related tree with key
        // add the key-value pair
        if (t.add(new CEntry<K, V>(key, value))) {
            // increase the number of keys since new entry inserted
            ++numKeys;
            // to decrease probabilty of collision, check if rehashing needed.
            // if so resize and rehash all the key-value pairs in the tree
            if (numKeys > (LOAD_THRESHOLD * table.length))
                rehash();
        } else {
            // BST does not allow dublicated item. if the key already placed in the
            // tree regardless of it's value, method add returns false.
            // So find the entry and modify its value
            CEntry<K, V> e = t.find(keyEntry(key)); // always returns non-null referance
            preVal = e.getValue();
            e.setValue(value);
        }
        return preVal;
    }

    @Override
    public V remove(K key) {
        int index = getTableSlot(key);
        if (table[index] == null) // key does not exist
            return null;
        else {
            CEntry<K, V> e = table[index].delete(keyEntry(key));
            if (e == null) // key-value pair does not exist
                return null;
            else {
                // decrease the number of keys and return removed entry value
                --numKeys;
                return e.getValue();
            }
        }
    }

    /**
     * Returns the index location of the given key
     * 
     * @param key The key
     * @return Table location of the key
     */
    private int getTableSlot(K key) {
        int index = key.hashCode() % table.length;
        // make sure index is non-negative
        return index < 0 ? index + table.length : index;
    }

    /**
     * Returns an key-value pair which value is null
     * An keyEntry is required for searching an entry with only key
     * 
     * @param key The key
     * @return A key entry
     */
    private CEntry<K, V> keyEntry(K key) {
        return new CEntry<K, V>(key, null);
    }

    /***
     * Expands the table size and rehash all the entries to the new resized table
     */
    @SuppressWarnings("unchecked")
    private void rehash() {
        // double the table size and is an odd number
        var oldTable = table;
        table = new BinarySearchTree[oldTable.length * 2 - 1];
        numKeys = 0; // reset the number of keys
        // rehash all the entries
        for (var bst : oldTable)
            if (bst != null)
                for (var e : bst)
                    put(e.getKey(), e.getValue());
    }

    @Override
    public int size() {
        return numKeys;
    }

    @Override
    public boolean isEmpty() {
        return numKeys == 0;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < table.length; ++i) {
            sb.append(String.format("h%-6s: ", "[" + i + "]"));
            sb.append(table[i] == null ? "null" : "\n" + table[i].toString());
            sb.append('\n');
        }
        return sb.toString();
    }

    /** The Comparable Entry class for key-value pairs */
    private static class CEntry<K extends Comparable<K>, V>
        extends Entry<K, V>  implements Comparable<CEntry<K, V>> {
        /**
         * Constructs a new key-value pair
         * @param key The key
         * @param value The value
         */
        public CEntry(K key, V value) {
            super(key, value);
        }

        @Override
        public int compareTo(CEntry<K, V> o) {
            return key.compareTo(o.key);
        }
    }
}