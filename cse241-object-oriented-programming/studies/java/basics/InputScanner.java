import java.util.Scanner;   
// no header file, no include statement
// javadoc used for creating header file as *.html or *.pdf

public class InputScanner {
    public static void main (String args[]) {
        // Each object created on heap with operator new
        // there is no delete operator, garbage collector 
        // frees all the allocated memory. It doesn't known
        // exatcly when garbage collector comes but it can 
        // be call with System.gc(); 
        
        // Money m1; // this is a reference of Money object not a money object
        // so everthing in java handles with references thats because
        // assignment operator very efficient just copies address
        // be carefull m1 is referance ant it's init vlaue is null 
        
        // The object references are all initialized to null in Java. However 
        // in order to do anything useful with these references, you must set 
        // them to a valid object, else you will get NullPointerExceptions 
        // everywhere you try to use such default initialized references

        Scanner input = new Scanner(System.in);
        
        int n1, n2;
        System.out.print("Enter first integer: ");
        n1 = input.nextInt();

        System.out.print("Enter second integer: ");
        n2 = input.nextInt();

        System.out.printf("Sum is %d\n", n1 + n2);
        input.close();
    }
}
