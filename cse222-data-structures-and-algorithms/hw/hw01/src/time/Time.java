package src.time;

import java.util.InputMismatchException;
import java.util.Scanner;

public class Time implements Cloneable {
    private int h;
    private int m;
    
    /** Initializes the object as h:m */
    public Time(int h, int m) {
        h = (0 <= h && h <= 23) ? h : 0; 
        m = (0 <= m && m <= 59) ? m : 0; 
    }

    /** Initializes the object as 0:0 */
    public Time() {
        h = 0; 
        m = 0;
    }

    /**
     * sets the hour component of the time
     * @param h hour
     * @throws IllegalArgumentException
     */
    public void setHour(int h) throws IllegalArgumentException {
        if (0 <= h && h <= 23)
            this.h = h;
        else
            throw new IllegalArgumentException();
    }

    /**
     * Hour
     * @return hour
     */
    public int getHour() {return h;}

    /**
     * sets the minute component of the time
     * @param m minute
     * @throws IllegalArgumentException
     */
    public void setMinute(int m) throws IllegalArgumentException {
        if (0 <= m && m <= 59)
            this.m = m;
        else
            throw new IllegalArgumentException();
    }

    /**
     * Reads the time from given input source
     * @param scanner input source
     */
    public void readTime(Scanner scanner) {
        boolean repeat = true;
        do {
            try {
                String line = scanner.nextLine();
                String[] v = line.split(":");
                
                // System.out.printf("size : %d\n", v.length);
                // for (var s : v) System.out.println(s);

                if (v.length >= 2) {
                    setHour(Integer.parseInt(v[0]));
                    setMinute(Integer.parseInt(v[1]));
                    repeat = false;
                }
                else
                    System.out.println("Please enter proper values");
            }
            catch (InputMismatchException e) {
                System.out.println("Please enter proper values");
            }
            catch (IllegalArgumentException e) {
                System.out.println("Please enter proper values");
            }
        } while (repeat);
    }

    /**
     * Minute component of current time
     * @return minute
     */
    public int getMinute() {return m;}

    @Override
    public String toString() {
        return String.format("%02d:%02d", h, m);
    }

    @Override
    public Time clone() {
        try {
            // shallow copy
            return (Time) super.clone();
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        else if (obj == null || obj.getClass() != this.getClass())
            return false;
        Time other = (Time) obj;
        return other.h == h && other.m == m;   
    }   

    @Override
    public int hashCode() {
        return 31 * (m + h);
    }
}
