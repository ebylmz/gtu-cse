package src.skiplist;

import java.util.Arrays;
import java.util.Random;

/**
  * • A new version of the skip list data structure, CustomSkipList.
  * • The CustomSkipList has two level lists in default.
  * • A new level list is added to the skip list each time
  *   when the size of the first level list reaches powers of 10.
  * • When a new level is added to the skip list, the tall items
  *   (contained in the more than one level) are appended to a one-level upper list. 
  * • An item can be also appended to upper-level lists during insertion operation. 
  * • The item is always appended to the first level list, and it is appended to
  *   higher levels by increasing chance with the number of items in the range between
  *   its closest tall neighbors on the left side and the right side. The probability
  *   of appending an item to a higher level is calculated by dividing the number of 
  *   items between two tall neighbors by 10.
 */
public class CustomSkipList<E extends Comparable<E>> {
    /** Head node of the skip-list */
    private SLNode<E> head;
    /** Initial level of skip-list */
    public static final int INIT_SLIST_LEVEL = 2;
    /** The number of item the skip-list contains */
    private int size;

    /**
     * Constructs an empty skip-list
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public CustomSkipList() {
        // The CustomSkipList has two level lists in default.
        head = new SLNode(Integer.MIN_VALUE, INIT_SLIST_LEVEL);
        size = 0;
    }

    /**
     * Inserts the item to the skip-list 
     * Note: dublication is allowed
     * @param item The item that is being inserted
     */
    public void add(E item) {
        // get the predecessors of the item
        var pred = search(item);
        // when the size of the first level list reaches multiple of 10
        // a new level list is added to the skip list
        int maxlevel = size / 10 + INIT_SLIST_LEVEL;
        if (maxlevel > head.links.length) {
            // resize predecessor array
            pred = Arrays.copyOf(pred, maxlevel);
            pred[maxlevel - 1] = head;
            // resize the skip-list
            resize();
        }

        // create a new level-node
        SLNode<E> newNode = createNewSLNode(pred, item);

        // insert the new level-node by re-adjusting links in each level-list (same as linked-list)
        for (int i = 0; i < newNode.links.length; ++i) {
            // linking to the successor 
            newNode.links[i] = pred[i].links[i];
            // linking to the predecessor 
            pred[i].links[i] = newNode;
        }
        // new item inserted to the skip-list
        ++size; 
    }    

    /**
     * Creates a new list-node which level (1 or 2) is determined by random selection. 
     * 
     * The item is always appended to the first level
     * list, and it is appended to higher levels by 
     * increasing chance with the number of items in
     * the range between its closest tall neighbors on
     * the left side and the right side. The probability 
     * of appending an item to a higher level is calculated
     * by dividing the number of items between two tall neighbors by 10.
     * Consider only one side if the item will be appended to the head or tail of the list.
     * 
     * @param pred The predecessors of insertion poisition
     * @param data The data of level-node
     * @return A new list-node
     */
    private SLNode<E> createNewSLNode(SLNode<E>[] pred, E data) {
        Random rand = new Random();
        // find the number of items between two tall neighbors
        int n = inBetween(pred);

        // the probability that the generated random number is less than n %(10 * n)
        // and this is same as creating a second level list-node  
        int newNodeLevel = rand.nextInt(10) < n ? 2 : 1;
        return new SLNode<E>(data, newNodeLevel);        
    }

    /**
     * Adds a new level-list to the skip-list.
     * The tall items (contained in the more than one level) 
     * are appended to a one-level upper list. 
     */
    private void resize() {
        for (var trav = head; trav != null; trav = trav.links[0]) {
            // maxLevel of list-node
            int maxLevel = trav.links.length;
            // increase the level of tall nodes
            if (maxLevel > 1) {
                trav.links = Arrays.copyOf(trav.links, maxLevel + 1);
                // adjust the links
                trav.links[maxLevel] = trav.links[maxLevel - 1];
                SLNode<E> next = trav.links[0];
                if (next != null) {
                    // make sure next is tall node (level is larger than 1)
                    int nextMaxLevel = next.links.length;
                    if (nextMaxLevel > 1 && maxLevel > nextMaxLevel) 
                        trav.links[nextMaxLevel] = next;
                }
            }
        }   
    }



    /**
     * Finds the number of items between two tall neighbors.
     * 
     * Note: Consider only one side if the item will be 
     *       appended to the head or tail of the list.
     * 
     * @param pred The predecessors
     * @return the number of items between two tall neighbors
     */
    private int inBetween(SLNode<E>[] pred) {
        // head node is always tall and if no tall node at right side,
        // then only count the number of nodes at left side

        // determine the closes (level 2 pred/succ) tall neigbors
        var left = pred[1];
        // if no tall successor, then set right as the level 1 successor to indicate end point 
        var right = pred[1].links[1] != null ? pred[1].links[1] : pred[0].links[0]; 

        int count = 0;
        // count the number of items between right and left tall neighbors
        for (var it = left; it.links[0] != right; it = it.links[0]) 
            ++count;
        return count;
    }

    /**
     * Removes the item if it's exist in the skip-list
     * @param item The item that is being removed
     * @return Returns a referance to removed item if it's found in the skip-list
     *         otherwise returns null 
     */
    public E remove(E item) {
        if (size > 0) {
            // get the predecessors of the item
            var pred = search(item);
            var targetNode = pred[0].links[0];
            // make sure that the given item is exist in the skip-list
            if (targetNode != null && targetNode.data.equals(item)) {
                // re-adjust the links to remove targetNode
                for (int i = 0; i < targetNode.links.length; ++i)
                    if (pred[i].links[i] != null)
                        pred[i].links[i] = pred[i].links[i].links[i]; 
                // return the removed item
                return targetNode.data;    
            }
        }
        // item does not exist in the list 
        return null; 
    }   

    /**
     * Finds the target item in the skip-list 
     * @param target The target that is being sought
     * @return If it's found returns a referance to the target otherwise returns null. 
     */
    public E find(E target) {
        // if the target is exist, then the bottom level predecessor must point it
        SLNode<E> pred = search(target)[0]; // bottom level predecessor
        return (pred.links[0] != null && pred.links[0].data.equals(target)) ?
            pred.links[0].data : null;
    }

    /**
     * Searchs the target item in the skip-list and
     * returns all the level predecessors of it
     * @param target The target item
     * @return Array of predecessors according to increasing level order
     */
    @SuppressWarnings("unchecked")
    private SLNode<E>[] search(E target) {
        SLNode<E>[] pred = (SLNode<E>[]) new SLNode[head.links.length];
        // start from top level to bottom level
        for (int i = head.links.length - 1; i >= 0; --i) {
            SLNode<E> trav = head;
            // traverse the current level-list till the found item is larger than or equal the target
            while (trav.links[i] != null && trav.links[i].data.compareTo(target) < 0)
                trav = trav.links[i]; // move the next node
            // record the current level predeccessor
            pred[i] = trav;
        }
        // return all the recording predecessor during search
        return pred;
    }

    @Override
    public String toString(){
		if(size == 0)
			return "Empty";
		StringBuilder sb = new StringBuilder();
        // print the skip-list from first level to last level
        for (int i = 0; i < head.links.length; ++i) {
            // print the level order
            sb.append(String.format("%-3s ", i + 1 + ":"));
            /** current traverses inside the current level-list */
            SLNode<E> current = head.links[i];
            /** first traverses inside the first level-list */
            SLNode<E> first = head.links[0];
            // print the level-list
            while (current != null) {
                // since current is starts ahead of first
                // first cannot be null before current be 

                // add some margin between nodes
                while (first != current) {
                    sb.append(String.format("%-5s", "--"));
                    first = first.links[0];
                }
                sb.append(String.format("%-5s", current.data));
                // first reach the current, continue with next node at current level-list
                current = current.links[i];
                first = first.links[0];
            }
            sb.append("\n");
        }
        return sb.toString();
    }



    /** Skip-List Node */
    private static class SLNode<E> {
        /** Keeps the links of the skip-list */
        private SLNode<E>[] links;
        /** Keeps the data of the SLNode object */
        private E data;

        /**
         * Constructs a level n skip-list node
         * Note: level n nodes can connect n level-lists
         * @param data The data
         * @param n Level of the node
         */
        @SuppressWarnings("unchecked")
        public SLNode(E data, int n) {
            this.data = data;
            links = (SLNode<E>[]) new SLNode[n];
        }
    }
}