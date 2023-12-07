package test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.NoSuchElementException;

import src.cityplanner.CityPlanner;
import src.street.*;
import src.structure.*;
import src.util.Time;

public class Test {
    public static void main(String[] args) {
        System.out.println("\n<<<<<<<<<< TEST0 - LDLinkedList >>>>>>>>>>\n");
        LDLinkedListTest.testAll();

        int streetLen = 40;
        System.out.println("\n<<<<<<<<<< TEST1 - StreetArray >>>>>>>>>>\n");
        test1(new StreetArray(streetLen));
        System.out.println("\n<<<<<<<<<< TEST1 - StreetArrayList >>>>>>>>>>\n");
        test1(new StreetArrayList(streetLen));
        System.out.println("\n<<<<<<<<<< TEST1 - StreetLinkedList >>>>>>>>>>\n");
        test1(new StreetLinkedList(streetLen));
        System.out.println("\n<<<<<<<<<< TEST1 - StreetLDLinkedList >>>>>>>>>>\n");
        test1(new StreetLDLinkedList(streetLen));
        enterToContinue();

        streetLen = 30;
        System.out.println("\n<<<<<<<<<< TEST2 - StreetArray >>>>>>>>>>\n");
        test2(new StreetArray(streetLen));
        System.out.println("\n<<<<<<<<<< TEST2 - StreetArrayList >>>>>>>>>>\n");
        test2(new StreetArrayList(streetLen));
        System.out.println("\n<<<<<<<<<< TEST2 - StreetLinkedList >>>>>>>>>>\n");
        test2(new StreetLinkedList(streetLen));
        System.out.println("\n<<<<<<<<<< TEST2 - StreetLDLinkedList >>>>>>>>>>\n");
        test2(new StreetLDLinkedList(streetLen));
        enterToContinue();
    
        System.out.println("\n<<<<<<<<<< TEST3 - StreetArray >>>>>>>>>>\n");
        RunTimeTest.testAll(new StreetArray(), new StreetArray());
        enterToContinue();
        System.out.println("\n<<<<<<<<<< TEST3 - StreetArrayList >>>>>>>>>>\n");
        RunTimeTest.testAll(new StreetArrayList(), new StreetArrayList());
        enterToContinue();
        System.out.println("\n<<<<<<<<<< TEST3 - StreetLinkedList >>>>>>>>>>\n");
        RunTimeTest.testAll(new StreetLinkedList(), new StreetLinkedList());
        enterToContinue();
        System.out.println("\n<<<<<<<<<< TEST3 - StreetLDLinkedList >>>>>>>>>>\n");
        RunTimeTest.testAll(new StreetLDLinkedList(), new StreetLDLinkedList());
        enterToContinue();

        // to play other versions, comment out
        streetLen = 64;
/**
         System.out.println("\n<<<<<<<<<< TEST4 - StreetArray >>>>>>>>>>\n");
        test4(new StreetArray(streetLen));
        System.out.println("\n<<<<<<<<<< TEST4 - StreetArrayList >>>>>>>>>>\n");
        test4(new StreetArrayList(streetLen));
        System.out.println("\n<<<<<<<<<< TEST4 - StreetLinkedList >>>>>>>>>>\n");
        test4(new StreetLinkedList(streetLen));
 */
        System.out.println("\n<<<<<<<<<< TEST4 - StreetLDLinkedList >>>>>>>>>>\n");
        test4(new StreetLDLinkedList(streetLen));
    }

    // tests Street class methods
    public static void test1(StreetInterface street) {
        System.err.printf("New street which length is %d\n", street.getLength());
        
        // create a house at location 5 whose width is 4 height is 7
        House house = new House(5, 4, 7, "Alice", 5, "white");
        
        // create an office at location -10 whose width is 5 height is 5
        Office office1 = new Office(-10, 5, 5, "Bruce", "Information Technology");
        
        // create a market at location 30 whose width is 9 height is 5
        Time openingTime = new Time(9, 0);
        Time closingTime = new Time(22, 55);
        Market market = new Market(12, 5, 5, "Justin", openingTime, closingTime);

        // create a playground at location -6 whose width is 4
        Playground playground = new Playground(-6, 4);
        
        // create an office at location -30 whose width is 10 height is 12
        Office office2 = new Office(-30, 10, 12, "IstM", "Architect");

        // these strucutes are not cause any superposition
        // so expected result is all of them are added succesfully  
        debug_add(street, house);
        debug_add(street, office1);
        debug_add(street, playground);
        debug_add(street, market);
        debug_add(street, office2);

        System.out.printf("Number of structure in street: %d\n\n", street.getLandCount());
        
        // try to add a structure which is not inside the boundry of the street
        debug_add(street, new Market(street.getLength(), 6, 4, "Far From Any Road")); 

        // try to add any structure that cause superposition
        // superposition with Alice's house 
        debug_add(street, new Office(17, 6, 4, "Harley", "Architecture")); 
        // superposition with Bruce's office 
        debug_add(street, new Market(6, 5, 4, "KIM"));

        System.out.printf("Number of structure in street: %d\n", street.getLandCount());

        // display skyline silhouette
        street.displaySkylineSilhouette();

        // display all the added structures
        street.list();

        // focus on the object at location -10
        int loc = -27;
        System.out.printf("\nFocusing at location %d\n", loc);
        street.focus(loc);
        System.out.println();

        // remove the first structure of list
        debug_remove(street, 0);

        // try to remove structure that doesn't exist in the street
        debug_remove(street, new Office(13, 4, 5, "Joker"));
        debug_remove(street, street.getLandCount());

        System.out.println();

        // try to find Bruce's office 
        loc = -12;
        var l = street.find(loc);
        if (l == null) 
            System.out.printf("No structure exist at location %d", loc);
        else {
            System.out.printf("There exist a structure at location %d\nFocus information\n", loc);
            street.focus(loc);
        } 
        System.out.println();
        debug_remove(street, l);

        // display all the structures
        street.list();

        // clear the street
        street.clear();
        System.out.printf("\nNumber of structure in street: %d\n", street.getLandCount());
    }

    // tests Street class methods
    public static void test2(StreetInterface street) {
        // StreetArray street = new StreetArray(30);
        System.err.printf("New street which length is %d\n", street.getLength());
        
        Land[] landPlan = {
            new House(5, 4, 3, "Alice"),
            new Market(-10, 5, 4, "Migros"),
            new House(-6, 12, 7, "Bruce"),
            new Playground(20, 4),        
            new Office(10, 5, 5, "Elon"),
            new Office(-15, 4, 4, "Yusuf"),
            new House(7, 6, 5, "Jordan"),
            new Playground(-6, 4)
        };

        // try to add all the planned lands
        for (var land : landPlan)
            debug_add(street, land);
        System.out.println();

        street.list();
        street.remove(5 - 1);
        street.list();

        // display all the buildings in the street
        street.list();
        street.displaySkylineSilhouette();

        street.getAnalysis(new House());

        // remove all the landPlans (some of them placed, some of them not)
        for (var land : landPlan)
            debug_remove(street, land);
        System.out.println();
    }

    // tests CityPlanner
    public static void test4(StreetInterface street) {
        Collection<Land> c = new ArrayList<>();
        c.add(new House(5, 4, 3, "Alice"));
        c.add(new Market(-12, 6, 10, "Migros", new Time(9, 0), new Time(23, 55)));
        c.add(new Market(-42, 5, 11, "Carrefour", new Time(9, 0), new Time(23, 55)));
        c.add(new Playground(20, 4));        
        c.add(new Playground(40, 15));        
        c.add(new Office(10, 5, 5, "Elon", "Space"));
        c.add(new House(27, 6, 5, "Jordan"));
        c.add(new House(-32, 6, 15, "Jordan"));
        c.add(new Office(34, 6, 9, "MMN", "Architect"));
        c.add(new Playground(-6, 4));

        CityPlanner planner = new CityPlanner(street);
        planner.start();
    }

    public static void debug_add(StreetInterface street, Land land) {
        System.out.printf("%-30s add(): ", land.getDimensionInfo());
        
        if (street.add(land))
            System.out.printf("SUCCESS\n");
        else    
            System.out.printf("FAIL\n");
    }

    public static void debug_remove(StreetInterface street, Land land) {
        System.out.printf("%-30s remove(): ", land.getDimensionInfo());

        if (street.remove(land))
            System.out.printf("SUCCESS\n");
        else    
            System.out.printf("FAIL (No such element)\n");
    }

    public static void debug_remove(StreetInterface street, int index) {
        try {
            Land land = street.get(index);
            System.out.printf("%-30s remove(): ", land.getDimensionInfo());
            if (street.remove(land))
                System.out.printf("SUCCESS\n");
            else    
                System.out.printf("FAIL (No such element)\n");
        } 
        catch (IndexOutOfBoundsException e) {
            System.out.printf("remove(): FAIL (Index out of bounds (max index: %d))\n", street.getLandCount() - 1);
        }
        catch (NoSuchElementException e) {
            System.out.printf("remove(): FAIL (No such element (max index: %d))\n", street.getLandCount() - 1);
        }
    }

    /** To stop the execution and give user enough time to see the screen */
    private static void enterToContinue() {
        System.out.printf("Enter to continue ");
        try {
            System.in.read();
        }
        catch(Exception e) {
            // just continue
        }
    }

}