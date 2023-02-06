package src.hashing;

public class HashTableCoalesced<K, V> implements KWHashMap<K, V> {
    /** Hash table */
    private EEntry<K, V>[] table;
    /** Initial capacity */
    private static final int INIT_CAPACITY = 10; 
    private static final double LOAD_THRESHOLD = 0.75;
    private int numKeys;
    private int primeNum = 7; 

    @SuppressWarnings("unchecked")
    public HashTableCoalesced() {
        table = new EEntry[INIT_CAPACITY];
    }

    @Override
    public V get(K key) {
        int i = find(key)[1];
        return table[i] != null ? table[i].getValue() : null; 
    }

    /**
     * Inserts the given key value pair to the table
     * @param key The key
     * @param value The value 
     * @return If the key is already exist then returns the 
     * old value of entry otherwise, returns null
     */
    @Override
    public V put(K key, V value) {
        V modified = null;
        int[] index = find(key);
        int prev = index[0]; 
        int cur = index[1]; 
        if (table[cur] == null) { // insert a new entry
            table[cur] = new EEntry<K, V>(key, value);
            if (prev != cur) {
                table[cur].prev = prev;
                table[prev].next = cur;
            }
            ++numKeys;
            // make sure the load factor does not exceed
            if (loadFactor() >= LOAD_THRESHOLD) 
                rehash();
            modified = null; // no modified old value
        }
        else {
            modified = table[cur].getValue();
            table[cur].setValue(value);
        }
        return modified;
    }

    private int[] find(K key) {
        int collisionIndex = doubleHash(key, 1); 
        return find(key, 1, collisionIndex, collisionIndex, collisionIndex);
    }

    private int[] find(K key, int p, int collisionIndex, int curIndex, int prevIndex) {
        // return if an emtp slot find or the key is found
        if (table[curIndex] == null || table[curIndex].getKey().equals(key)) 
            return new int[] {prevIndex, curIndex};
        // follow the chain
        else if (doubleHash(table[curIndex].getKey(), 1) == collisionIndex) {
            int nextIndex = 0;
            
            // continue chaining
            if (table[curIndex].hasNext()) 
                nextIndex = table[curIndex].next;
            else {
                ++p;
                nextIndex = doubleHash(key, p);
            }
            return find(key, p, collisionIndex, nextIndex, curIndex);
        }
        else { // continue probing
            int nextIndex = doubleHash(key, p + 1);
            return find(key, p + 1, collisionIndex, nextIndex, prevIndex);
        } 
    }

    /**
     * Removes given key, if the key is found
     * @param key Key value
     * @param i Table index
     * @return If the key is found and deleted then returns the deleted value  
     */
    @Override
    public V remove(K key) {
        V removed = null;
        int i = find(key)[1];
        
        if (table[i] != null) {
            removed = table[i].getValue();
            if (!table[i].hasPrevious()) { // remove the head of the chain
                if (!table[i].hasNext())
                    table[i] = null;
                else {
                    int next = table[i].next;
                    table[i] = table[next];
                    // no previus value
                    if (table[i] != null) {
                        table[i].prev = -1;
                    }
                    table[next] = null;
                }
            }
            else { // remove at the middle or at the end of the chain
                int prev = table[i].prev;
                int next = table[i].next;
                // link the previos and next entries
                if (table[i].hasNext())
                    table[next].prev = prev;
                if (table[prev] != null)
                    table[prev].next = next;
                // remove the middle entry
                table[i] = null;
            }
            --numKeys;
        }
        return removed;
    }

    @Override
    public int size() {
        return numKeys;
    }

    @Override
    public boolean isEmpty() {
        return numKeys == 0;
    }

    private int hash1(K key) {
        return key.hashCode() % table.length;
    }

    private int hash2(K key) {
        return primeNum - (key.hashCode() % primeNum);
    }

    private int doubleHash(K key, int n) {
        return (hash1(key) + n * hash2(key)) % table.length;
    }

    /**
     * Calculates the load factor of hash table
     * @return loadfactor = numkeys / tablesize
     */
    private double loadFactor() {
        return (double) numKeys / (double) table.length;
    }

    @SuppressWarnings("unchecked")
    private void rehash() {
        // double the table size which is odd
        EEntry<K, V>[] oldTable = table;
        table = new EEntry[oldTable.length * 2 - 1];
        // set the prime number according to new table size
        primeNum = nextPrime();
        // rehash entries to new allocated table
        numKeys = 0;
        for (var e : oldTable)
            if (e != null)
                put(e.key, e.value);
    }

    /**
     * Finds the largest prime number smaller than (0.8 * tablesize)
     * @return The largest prime number smaller than (0.8 * tablesize)
     */
    private int nextPrime() {
        int n = (int) (0.8 * table.length);
        while (n > primeNum && !isPrime(n))
            --n;
        return n;
    }

    private boolean isPrime(int n) {
        if (n < 2)
            return false;
        else if (n == 2)
            return true;
        else {
            // Check from 2 to square root of n
            int d = (int) Math.sqrt(n);
            for (int i = 2; i <= d; i++)
                if (n % i == 0)
                    return false;
            return true;
        }
    }
 

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("%-10s %-10s %-10s %-10s\n\n", "HashValue", "Key", "Next", "Prev")); 
        for (int i = 0; i < table.length; ++i) {
            sb.append(String.format("%-10d ", i)); 
            var e = table[i];
            if (e == null) 
                sb.append(String.format("%-10s %-10s\n", "null", "null"));
            else {
                sb.append(String.format("%-10s ", e.getKey()));
                sb.append(String.format("%-10s ", e.next == -1 ? "null" : e.next));
                sb.append(String.format("%-10s\n", e.prev == -1 ? "null" : e.prev));
            }
        }

        sb.append(String.format("--------------------------------------------------\n")); 
        sb.append(String.format("numKeys: %-4d | capacity: %-4d | load factor: %-4.2f\n\n", 
            numKeys, table.length, loadFactor())); 

        return sb.toString();
    }

    /*** Enchanced Entry class for key-value pairs */
    private static class EEntry<K, V> extends Entry<K, V> {
        /** Index of the entry which collide with this entry */
        private int next;
        private int prev;
        private static final int END = -1;

        /**
         * Constructs an Entry which has no next entry
         * @param key
         * @param value
         */
        public EEntry(K key, V value) {
            super(key, value);
            prev = next = END;
        }

        /**
         * Constructs an Entry has a next entry
         * @param key The key
         * @param value The value
         * @param next Next field
         */
        public EEntry(K key, V value, int next, int prev) {
            super(key, value);
            this.next = next;
            this.prev = prev;
        }

        /**
         * Determines whether this entry has next entry
         * @return True if there is a next entry
         */
        public boolean hasNext() {
            return next != END;
        }

        /**
         * Determines whether this entry has previos entry
         * @return True if there is a previous entry
         */
        public boolean hasPrevious() {
            return prev != END;
        }
    }
}