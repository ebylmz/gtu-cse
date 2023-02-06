package employee;

public class TestBasePlusCommisionEmployee {
    public static void main (String args[]) {
        BasePlusCommissionEmployee e = new BasePlusCommissionEmployee(
            "Emirkan Burak", "Yilmaz", "1901042659", 5000 , .04 , 300);

        System.out.println("Employee information: ");
        System.out.printf("%s %s\n", "First Name:", e.firstName());
        System.out.printf("%s %s\n", "Last Name:", e.lastName());
        System.out.printf("%s %s\n", "Social Security Number:", e.socialSecurityNumber());
        System.out.printf("%s %.2f\n", "Gross Sales:", e.grossSales());
        System.out.printf("%s %.2f\n", "Commision rate:", e.commissionRate());
        System.out.printf("%s %.2f\n", "Base Salary:", e.baseSalary());

        e.setBaseSalary(1000);

        System.out.printf("\n%s:\n\n%s\n", 
            "Updated employee information obtained by toString",
            e.toString() );
    }
}
