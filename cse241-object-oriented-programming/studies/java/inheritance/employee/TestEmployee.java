package employee;

public class TestEmployee {
    public static void main (String args[]) {
        Employee e[] = new Employee[4];
        e[0] = new SalariedEmployee("Emirkan Burak", "Yilmaz", "111-11-1111", 800.00);
        e[1] = new HourlyEmployee("Bugs", "Bunny", "222-22-2222", 16.75, 40);
        e[2] = new CommissionEmployee("Mary", "Jane", "333-33-3333", 10000, .06);
        e[3] = new BasePlusCommissionEmployee("Bob", "Lewis", "444-44-4444", 5000, .04, 300);
        
        for (Employee current : e) {
            // determine whether element is a BasePlusCommissionEmployee
            if (current instanceof BasePlusCommissionEmployee) {
                // downcast Employee reference to BasePlusCommissionEmployee reference
                BasePlusCommissionEmployee employee = (BasePlusCommissionEmployee) current;
                double oldBaseSalary = employee.baseSalary();
                employee.setBaseSalary( 1.10 * oldBaseSalary );
                System.out.printf("new base salary with 10%% increase is: $%,.2f\n", employee.baseSalary() );            
            }
            System.out.printf("earned $%,.2f\n\n", current.earnings() );        
        }

        // print the types of employee objects
        for (int i = 0; i < e.length; ++i)
            System.out.printf("Employee %d is a %s\n", i, e[i].getClass().getName());
    }
}
