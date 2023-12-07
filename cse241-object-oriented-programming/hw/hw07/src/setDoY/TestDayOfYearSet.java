/**
 * @file    TestDayOfYearSet.java
 * @author  Emirkan Burak Yılmaz 
 * @brief   DayOfYearSet Driver Program   
 * @version 0.1
 * @date    2022-01-04
 * 
 * @copyright Copyright (c) 2021
 */

package setDoY;

import java.util.ArrayList;
import java.util.Scanner;
import setDoY.DayOfYearSet.DayOfYear;

public class TestDayOfYearSet {
    public static void main (String[] args) {
         test0();
         test1();
         test2();
         test3();
         test4();
         test5();
         test6();
         test7();
        System.out.printf("============== END OF DRIVER PROGRAM ==============\n");
    }

    /**
     * Tests method nextDay abd preDay (DayOfYearSet.DayOfYear) 
     */
    public static void test0 () {
        System.out.printf("======================== Test0 ========================\n");

        DayOfYear today = new DayOfYear(12, 31);

        System.out.printf("Date of Yesterday is %s\n", today.preDay());
        System.out.printf("Today is new year's eve (%s)\n", today);
        System.out.printf("Tomarrow will be first day of new year (%s)\n", today.nextDay());
        System.out.printf("====================== Test DONE ======================\n\n");
    }

    /**
     * Tests method equal, dayBetween (DayOfYearSet.DayOfYear) 
     */
    public static void test1 () {
        System.out.printf("======================== Test1 ========================\n");        

        Scanner reader = new Scanner(System.in);
        
        System.out.printf("Enter today's date (mm dd): " );
        int m = reader.nextInt();
        int d = reader.nextInt();
        DayOfYear today = new DayOfYear(m, d);

        System.out.printf("Enter your birthday (mm dd): " );
        m = reader.nextInt();
        d = reader.nextInt();
        DayOfYear birthday = new DayOfYear(m, d);

        System.out.printf("\n\nToday: %s\n", today);
        System.out.printf("Your birthday: %s\n", birthday);

        
        if (today.equals(birthday))
        System.out.printf("Happy birthday to youu\n");
        else {
            int difference = today.dayBetween(birthday);
            
            if (today.preceding(birthday))
                System.out.printf("There %d day(s) to your birthday\n", difference);
            else
                System.out.printf("%d days ago was your birthday\n", difference);
        }
            
        reader.close();
            
        System.out.printf("====================== Test DONE ======================\n\n");
    }


    /**
     * Tests set operation methods union, intersection, difference... (DayOfYearSet.DayOfYear) 
     */
    public static void test2 () {
        System.out.printf("======================== Test2 ========================\n");

        System.out.printf("\nDayOfYear objects alive in all the sets: %d\n\n", DayOfYearSet.total());

        DayOfYearSet s1 = new DayOfYearSet();
        s1.add(new DayOfYear(1, 1));
        s1.add(new DayOfYear(2, 2));
        s1.add(new DayOfYear(3, 3));
        s1.add(new DayOfYear(4, 4));
        s1.add(new DayOfYear(5, 5));
        s1.add(new DayOfYear(6, 6));
        s1.add(new DayOfYear(7, 7));
        System.out.printf("s1: %s\n", s1);
        
        DayOfYearSet s2 = new DayOfYearSet();
        s2.add(new DayOfYear(12, 12));
        s2.add(new DayOfYear(11, 11));
        s2.add(new DayOfYear(10, 10));
        s2.add(new DayOfYear(9, 9));
        s2.add(new DayOfYear(8, 8));
        s2.add(new DayOfYear(7, 7));
        s2.add(new DayOfYear(6, 6));
        s2.add(new DayOfYear(5, 5));
        System.out.printf("s2: %s\n", s2);
        
        System.out.printf("Are s1 and s2 equivalent? ");
        if (s1.equals(s2))
            System.out.println("Yes");
        else
            System.out.println("No");
        
        System.out.printf("\nDayOfYear objects alive in all the sets: %d\n\n", DayOfYearSet.total());
        
        System.out.printf("s1 u s2: %s\n", s1.union(s2));
        System.out.printf("s1 ∩ s2: %s\n", s1.intersection(s2));
        System.out.printf("s1 - s2: %s\n", s1.difference(s2));
        System.out.printf("s2 - s1: %s\n", s2.difference(s1));
        
        System.out.printf("\nDayOfYear objects alive in all the sets: %d\n", DayOfYearSet.total());
        
        // System.out.printf("~s1: %s\n", s1.complement());
        
        // print the complement set to the txt file
        s1.complement().write("sets/s5.txt");

        System.out.printf("====================== Test DONE ======================\n\n");
    }

    /** Tests sort and remove method (DayOfYearSet) */
    public static void test3 () {
        System.out.printf("======================== Test3 ========================\n");
        
        DayOfYearSet s1 = new DayOfYearSet("sets/s1.txt");
        DayOfYear d = new DayOfYear(5, 21);
        
        s1.add(new DayOfYear(5, 15));
        s1.add(d);
        System.out.printf("s1: %s\n", s1);

        s1.sort();
        System.out.printf("\ncall s1.sort()\n");
        System.out.printf("s1: %s\n\n", s1);

        System.out.printf("Remove %s, %s, %s\n", d, s1.at(0), s1.at(s1.size() - 1));
        s1.remove(d);
        s1.remove(0);
        s1.remove(s1.size() - 1);
        s1.remove(s1.size() + 1000);    // out of index
        s1.remove(s1.size() + 2000);    // out of index
        
        
        System.out.printf("s1: %s\n", s1);
        System.out.printf("====================== Test DONE ======================\n\n");
    }

    /** Tests class constructor which takes an Arraylist (DayOfYearSet) */
    public static void test4 () {
        System.out.printf("======================== Test4 ========================\n");
        ArrayList<DayOfYear> arr = new ArrayList<DayOfYear>();
        
        arr.add(new DayOfYear(1, 12));
        arr.add(new DayOfYear(1, 32));
        arr.add(new DayOfYear(5, 22));
        arr.add(new DayOfYear(4, 12));
        arr.add(new DayOfYear(11, 25));
        arr.add(new DayOfYear(4, 1));
        arr.add(new DayOfYear(3, 12));
        // add dublicated values
        arr.add(new DayOfYear(1, 14));
        arr.add(new DayOfYear(1, 14));

        System.out.printf("ArrayList content: %s\n", arr);
        
        DayOfYearSet set = new DayOfYearSet(arr); 

        System.out.printf("set content: %s\n", set); 
        System.out.printf("====================== Test DONE ======================\n\n");
    }    

    /** Tests De Morgan laws in DayOfYearSet (DayOfYearSet) */
    public static void test5 () {
        System.out.printf("======================== Test5 ========================\n");
        
        DayOfYearSet s1 = new DayOfYearSet("sets/s1.txt");
        DayOfYearSet s2 = new DayOfYearSet("sets/s2.txt");
        boolean verify = false;

        s1.add(new DayOfYear(12, 31));        
        s1.add(new DayOfYear(1, 25));        

        s2.add(new DayOfYear(12, 29));        
        s2.add(new DayOfYear(5, 23));        

        // Note that for sets s1 and s2, De Morgan says that !(s1 + s2) == !s1 ^ !s2
        DayOfYearSet s3 = s1.union(s2).complement();
        DayOfYearSet s4 = s1.complement().intersection(s2.complement()); 
        
        if (s3.equals(s4)) {
            System.out.println("The complement of the union of two sets is the same as the intersection of their complements");
            s3 = s1.intersection(s2).complement();
            s4 = s1.complement().union(s2.complement());
            if (s3.equals(s4)) {
                System.out.println("The complement of the intersection of two sets is the same as the union of their complements");
                System.out.println("DayOfYearSet verify the De Morgan Laws");     
                verify = true;
            }
        }
        if (!verify)
            System.out.println("DayOfYearSet doesn't verify the De Morgan Laws");     

        System.out.printf("====================== Test DONE ======================\n\n");
    }

    /** Tests method write (DayOfYearSet) */
    public static void test6 () {
        System.out.printf("======================== Test6 ========================\n");
        DayOfYearSet s1 = new DayOfYearSet("sets/s1.txt");
        DayOfYearSet s2 = new DayOfYearSet("sets/s2.txt");
        DayOfYearSet s3 = s1.intersection(s2);

        System.out.printf("s1: %s\n", s1);
        System.out.printf("s2: %s\n", s2);
        System.out.printf("s1 ∩ s2: %s\n", s3);
    
        String fname = "sets/s1_s2.txt";
        s3.write(fname);
        System.out.printf("Check the file \"%s\". It contains s1 ∩ s2\n", fname);

        System.out.printf("====================== Test DONE ======================\n\n");
    }

    /** Test method modify  (DayOfYearSet) */
    public static void test7 () {
        System.out.printf("======================== Test7 ========================\n");
    
        DayOfYearSet s1 = new DayOfYearSet("sets/s4.txt");
        System.out.printf("s1: %s (size: %d)\n", s1, s1.size());

        s1.modify(0, 3, 17);
        s1.modify(1, 9, 12);
        s1.modify(s1.size() - 1, new DayOfYear(12, 31));

        System.out.printf("\nSome modification on s1\n");
        System.out.printf("s1.modify(0, 3, 17)\n");
        System.out.printf("s1.modify(1, 9, 12)\n");
        System.out.printf("s1.modify(s1.size() - 1, new DayOfYear(12, 31))\n\n");
        
        System.out.printf("s1: %s (size: %d)\n", s1, s1.size());
        
        System.out.printf("====================== Test DONE ======================\n\n");
    }
}
