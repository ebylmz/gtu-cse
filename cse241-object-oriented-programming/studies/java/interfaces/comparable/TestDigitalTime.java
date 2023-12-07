package comparable;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.Scanner;

public class TestDigitalTime {
    public static void main (String[] args) {
        test2();
    }

    public static void sortDigitalTime (final DigitalTime[] arr) {
        // implements insertion sort
        for (int i = 1; i < arr.length; ++i) {
            DigitalTime cur = arr[i];   // no need to clone just keep the address
            int j = i - 1;
            
            while (j >= 0 && arr[j].compareTo(cur) > 0) {
                arr[j + 1] = arr[j];
                --j;
            } 
            arr[j + 1] = cur;
        }
    }

    public static void test1 () {
        DigitalTime[] time = new DigitalTime[10];
        time[0] = new DigitalTime(23, 17);
        time[1] = new DigitalTime(1, 15);
        time[2] = new DigitalTime(12, 22);
        time[3] = new DigitalTime(23, 31);
        time[4] = new DigitalTime(4, 45);
        time[5] = new DigitalTime(16, 5);
        time[6] = new DigitalTime(6, 24);
        time[7] = new DigitalTime(17, 37);
        time[8] = new DigitalTime(8, 58);
        time[9] = new DigitalTime(19, 29);

        for (var t : time)
            System.out.printf("(%s) ", t);
        System.out.println();

        sortDigitalTime(time);

        for (var t : time)
            System.out.printf("(%s) ", t);
        System.out.println();

        for (var t : time)
            System.out.printf("(%s) ", t.toUniversalString());
        System.out.println();    
    }

    public static void test2 () {
        ArrayList<DigitalTime> time = new ArrayList<DigitalTime>();
        Scanner scanner = new Scanner(System.in);
        String str_exit = "";

        do {
            try {
                System.out.printf("hour   : ");
                int h =  scanner.nextInt();
                System.out.printf("minute : ");
                int m = scanner.nextInt();
                scanner.nextLine(); // scan the rest of the line

                time.add(new DigitalTime(h, m));

                // ask for exit input entering
                System.out.printf("Do you want to continue(yes/no): ");
                str_exit = scanner.next();
            }
            catch (IllegalArgumentException e) {
                System.err.printf("%s, please enter legal values\n", e);
                // scanner.nextLine(); // scan the rest of the line
            }
            catch (InputMismatchException e) {
                System.err.printf("Please enter yes or no");
                scanner.nextLine(); // scan the rest of the line
            }
        } while (!str_exit.toUpperCase().equals("NO"));

        scanner.close();

        System.out.println("DigitalTime ArrayList:");
        for (var t : time)
            System.out.printf("(%s) ", t);
        System.out.println();
    }
}