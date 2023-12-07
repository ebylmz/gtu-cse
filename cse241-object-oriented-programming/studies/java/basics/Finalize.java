// warning: [deprecation] finalize() in Object has been deprecated

public class Finalize {
    public static class Employee {

        private String fname;
        private String lname;
        private static int count = 0;
        
        public Employee (String first, String last) {
            fname = first;
            lname = last;
            ++count;
            
            System.out.printf("Employee constructor: %s %s; count = %d\n", fname, lname, count);
        }
        
        protected void finalize () {
            --count;
            System.out.printf("Employee finalizer: %s %s; count = %d\n", fname, lname, count);
        }
        
        public static int getCount () {return count;}
    }
        
    public static void main (String[] args) {
        Employee e1 = new Employee("Arda", "Akkas");
        Employee e2 = new Employee("Hüseyin", "Akkas");
        Employee e3 = new Employee("Mustafa", "Akkas");
        Employee e4 = new Employee("Makbule", "Akkas");
    
        System.out.printf( "via Employee.getCount(): %d\n", Employee.getCount());

        e1 = null;
        e2 = null;

        System.gc(); // ask for garbage collection to occur now

        System.out.printf( "Employees after System.gc(): %d\n", Employee.getCount());
    }
}