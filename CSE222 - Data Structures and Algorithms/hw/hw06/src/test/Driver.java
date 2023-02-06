package src.test;

import src.hashing.HashTableChaningTree;
import src.hashing.HashTableCoalesced;

public class Driver {
    public static void main(String[] args) {
        System.out.println("Tests hash table implementataions");
        HashTableTest.test_validation(new HashTableChaningTree<>());
        HashTableTest.test_validation(new HashTableCoalesced<>());
        // HashTableTest.test_runtime();
        System.out.println("Tests sorting alogorithm implementataions");
        SortingTest.test_validation();
        SortingTest.test_runtime();
    }
}
