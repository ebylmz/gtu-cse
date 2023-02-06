import java.util.Arrays;
import java.util.Random;

public class SkipList<E extends Comparable<E>> {
    /** Head of the skip-list */
    private SLNode<E> head;
    /** The number of item the list contains */
    private int size;
    /** The level of a skip-list is defined as its highest node level */
    private int maxLevel; 
	/*** 
     * Smallest power of 2 that is greater than the current skip-list size 
     * to enable operation efficiency O(logn) in avagerage case
    */
    private int maxCap;
    /*** Natural log of 2 */
    private static final double LOG2 = Math.log(2.0);
    /*** Minimum possible integer value for the head */
    private static final int MIN = Integer.MIN_VALUE;
    
    @SuppressWarnings({"unchecked", "rawtypes"})
    public SkipList() {
        size = 0;
		maxLevel = 0;
		maxCap = computeMaxCap(maxLevel);
		head = new SLNode(MIN, maxLevel);
    }

    /**
     * Adds the given item 
     * @param item Item to be inserted
     */
    public void add(E item) {
        // get the predecessor to link new item 
        SLNode<E>[] pred = search(item);
        ++size; 
        // make sure size does not exceed maxCap 
        if (size > maxCap) {
            // increase the list size and allocate memory for new node-list
            ++maxLevel;
            maxCap = computeMaxCap(maxLevel);
            head.links = Arrays.copyOf(head.links, maxLevel);
			pred = Arrays.copyOf(pred, maxLevel);
            // since the max-level does not contain any item 
            // yet, head becomes max-level precessor 
            pred[maxLevel - 1] = head;
        }
        // to insert an item create a node whose level determined randomly
        SLNode<E> newNode = new SLNode<E>(item, logRandom()); 
        // insert the new node into skip-list 
        for (int i = 0; i < newNode.links.length; ++i) {
            newNode.links[i] = pred[i].links[i];
            pred[i].links[i] = newNode;
        }
    }

	/**
	 * Generates a logarithmic distributed integer between 1 and maxLevel
	 *  I.E. 1/2 of the values are 1, 1/4 are 2, etc.
	 * @return A random logarithmic distributed int between 1 and maxLevel
	 */
	private int logRandom(){
		Random rand = new Random();
        int r = rand.nextInt(maxCap);
		int k = (int) (Math.log(r + 1) / LOG2);
		if(k > maxLevel - 1)
			k = maxLevel - 1;
		return maxLevel - k;
	}
	
    /**
     * Smallest power of 2 that is greater than the current skip-list size
     * @param level Level of the list
     * @return 2^n - 1 (n: level of the list)
    */
	private int computeMaxCap(int level){
		return (int) Math.pow(2, level) - 1;
	}

    /**
     * Removes the given item if it is exist in the list
     * @param item Item to be removed
     * @return A reference to the removed item
     */
    public E remove(E item) {
        E removedItem = null;
        SLNode<E>[] pred = search(item);
        // don't re-adjust maxCap and level, as we may have nodes at these levels
        --size; 
        if (pred[0].links != null && pred[0].links[0].data.compareTo(item) == 0) {
            // use level-1 predecessor to reach the target/removed node
            SLNode<E> deleteNode = pred[0].links[0];
            removedItem = deleteNode.data;
            // re-adjust the links
            for (int i = 0; i < deleteNode.links.length; ++i)
                pred[i].links[i] = deleteNode.links[i];
        }	
        return removedItem;
    }

	/**
	 * Finds an item in the skip-list
	 * @param target The item being sought
	 * @return A reference to the item in the skip-list that matches
	 * 		   the target. If not found, null is returned 
	 */
    public E find(E target) {
        // If the object is found than the bottom level predecessor should points the target  
        var pred = search(target)[0]; 
        return (pred.links != null && pred.links[0] != null && pred.links[0].data.compareTo(target) == 0) ?
             pred.links[0].data : null;
    }

    /**
     * Searchs the target item in the list
     * @param target The item being sought
     * @return An SLNode array which references the predecessors of the target at each level
     */
    @SuppressWarnings("unchecked")
    private SLNode<E>[] search(E target) {
        /** Keeps the predecessors of the target */
        SLNode<E>[] pred = new SLNode[maxLevel];
        /** Keeps the current list */
        SLNode<E> current = head;
        // a search always begin with the highest level list (the list with the fewest elements)
        for (int i = current.links.length - 1; i >= 0; --i) {
            // skip the lists whose data is smaller than target value
            while (current.links[i] != null && current.links[i].data.compareTo(target) < 0)
                current = current.links[i];
            // record the level i predecessor
            pred[i] = current;
        }

        return pred;
    }

    @Override
    public String toString(){
		if(size == 0)
			return "Empty";
		StringBuilder sb = new StringBuilder();

        for (int i = 0; i < maxLevel; ++i) {
                sb.append(String.format("%-3s ", i + 1 + ":"));
            
            SLNode<E> levelList = head.links[i];
            var firstLevelList = head.links[0];
            while (levelList != null) {
                // adjust the blanks
                while (i != 0 && ! firstLevelList.data.equals(levelList.data)) {
                    sb.append(String.format("%-5s", "--"));
                    firstLevelList = firstLevelList.links[0];
                }
                sb.append(String.format("%-5s", levelList.data));
                // here levelList.data and firstLevelList.dat are equal
                // continue with next node
                levelList = levelList.links[i];
                firstLevelList = firstLevelList.links[0];
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    /** SkipList Node class */
    private static class SLNode<E> {
        /** Next nodes */
        private SLNode<E>[] links;
        /** The data field of current list */
        E data;

        /**
		 * Constructs a node of level n 
		 * @param n The level of the node (level n nodes contains n links)
		 * @param data The data to be stored
		 */
        @SuppressWarnings("unchecked")
        public SLNode(E data, int n) {
            // create level n skip-list node
            links = (SLNode<E>[]) new SLNode[n];
            this.data = data;
        }

        @Override
		public String toString(){
			return (data.toString() + " |" + links.length + "|"); 
		}
    }
}
