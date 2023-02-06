// package java.basic; // must be the very first executable statement in the file
// package name should consist of your Internet domain name in
// reverse order followed by other names for the package

// since Java is pure OOP language, there is no global function
// so main function always be in the class whose name is same as file name
// then JVM can call main function exactly filename.main()
public class HelloWorld {
    public static void main (String[] args) {
        // main is a static function to be able to
        // call without creating object of it
        System.out.printf("Hello, ");
        
        if (args.length == 0) 
            System.out.printf("World!");
        else 
            for (String v : args)
                System.out.printf("%s ", v);
        System.out.println();
    }
}