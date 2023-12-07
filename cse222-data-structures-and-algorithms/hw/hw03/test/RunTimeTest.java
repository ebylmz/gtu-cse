package test;

import src.street.StreetInterface;
import src.structure.Land;
import src.util.LDLinkedList;

public class RunTimeTest {
    public static final String STR_FORMAT = "%-25s %15s %25s\n";
    public static final String DIG_FORMAT = "%-25s %15d %25.2f";

    public static LDLinkedList<String> testAll(StreetInterface smallStreet, StreetInterface largeStreet) {
        LDLinkedList<String> list = new LDLinkedList<>();
        // problem sizes
        int psize1 = 10;
        int psize2 = 100;

        try {
            list.add(add(smallStreet, psize1));
            list.add(add(largeStreet, psize2));
            list.add(remove(smallStreet, psize1));
            list.add(remove(largeStreet, psize2));
            // add lands street for next tests
            add(smallStreet, psize1);
            add(largeStreet, psize2);
            list.add(getRemainingSpace(smallStreet, psize1));
            list.add(getRemainingSpace(largeStreet, psize2));
            list.add(get(smallStreet, psize1));
            list.add(get(largeStreet, psize2));
            list.add(find(smallStreet, psize1));
            list.add(find(largeStreet, psize2));
            list.add(list(smallStreet, psize1));
            list.add(list(largeStreet, psize2));
            list.add(displaySkylineSilhouette(smallStreet, psize1));
            list.add(displaySkylineSilhouette(largeStreet, psize2));
            list.add(clear(smallStreet, psize1));
            list.add(clear(largeStreet, psize2));
        } catch (IllegalStateException e) {
            System.out.println(e);
        }

        // display the results
        System.out.printf(STR_FORMAT, "Method", "Problem Size", "Run Time (msec)");
        for (var r : list)
            System.out.println(r);
        return list;
    }

    public static String add(StreetInterface street, int problemSize) {
        // before adding lands make sure enough street space exist
        street.setLength(problemSize * 3);

        long runTime = 0l;
        long startTime = 0l;
        long stopTime = 0l;
        
        int h = 2; // height of land
        int w = 2; // width of land
        int loc = 1;
        for (int i = 0; i < problemSize; ++i) {
            Land land = new Land(loc, w, h);
            loc += 3;
            startTime = System.nanoTime();
            street.add(land);
            stopTime = System.nanoTime();
            runTime += stopTime - startTime;
        }
        // System.out.printf("add() run time for problem size %7d: %15.2f\n", problemSize, runTime / 1000.0);
        return String.format(DIG_FORMAT, "add", problemSize, runTime / 1000f);
    }

    public static String remove(StreetInterface street, int problemSize) {
        check(street, problemSize);
        
        long runTime = 0l;
        long startTime = 0l;
        long stopTime = 0l;

        while (street.getLandCount() > 0) {
            startTime = System.nanoTime();
            // remove the first item 
            street.remove(0);
            stopTime = System.nanoTime();
            runTime += stopTime - startTime;
        }
        return String.format(DIG_FORMAT, "remove", problemSize, runTime / 1000f);
    }

    public static String clear(StreetInterface street, int problemSize) {
        check(street, problemSize);
        long startTime = System.nanoTime();
        street.clear();
        long stopTime = System.nanoTime();
        long runTime = stopTime - startTime;
        return String.format(DIG_FORMAT, "clear", problemSize, runTime / 1000f);
    }
    
    public static String getRemainingSpace(StreetInterface street, int problemSize) {
        check(street, problemSize);
        long startTime = System.nanoTime();
        street.getRemainingSpace();
        long stopTime = System.nanoTime();
        long runTime = stopTime - startTime;
        return String.format(DIG_FORMAT, "getRemainingSpace", problemSize, runTime / 1000f);
    }

    public static String get(StreetInterface street, int problemSize) {
        check(street, problemSize);
        long startTime = System.nanoTime();
        street.get(street.getLandCount() / 2); 
        long stopTime = System.nanoTime();
        long runTime = stopTime - startTime;
        return String.format(DIG_FORMAT, "get", problemSize, runTime / 1000f);
    }

    public static String find(StreetInterface street, int problemSize) {
        check(street, problemSize);
        Land land = street.get(street.getLandCount() / 2); 
        long startTime = System.nanoTime();
        street.find(land);
        long stopTime = System.nanoTime();
        long runTime = stopTime - startTime;
        return String.format(DIG_FORMAT, "find", problemSize, runTime / 1000f);
    }

    public static String list(StreetInterface street, int problemSize) {
        check(street, problemSize);
        long startTime = System.nanoTime();
        street.list();
        long stopTime = System.nanoTime();
        long runTime = stopTime - startTime;
        return String.format(DIG_FORMAT, "list", problemSize, runTime / 1000f);
    }

    public static String displaySkylineSilhouette(StreetInterface street, int problemSize) {
        check(street, problemSize);
        long startTime = System.nanoTime();
        street.displaySkylineSilhouette();
        long stopTime = System.nanoTime();
        long runTime = stopTime - startTime;
        return String.format(DIG_FORMAT, "displaySkylineSilhouette", problemSize, runTime / 1000f);
    }

    private static void check(StreetInterface street, int problemSize) {
        if (street.getLandCount() != problemSize)
            throw new IllegalStateException();
    }
}