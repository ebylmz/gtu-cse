package payable;

public class TestPayable {
    public static void main (String[] args) {
        // 
        Payable pays[] = new Payable[4];
        pays[0] = new SalariedEmployee("Emirkan Burak", "Yilmaz", "111-11-1111", 1800.00);
        pays[1] = new SalariedEmployee("Bugs", "Bunny", "222-22-2222", 1675.00);
        pays[2] = new Invoice("01234", "seat", 2, 375.00);
        pays[3] = new Invoice("56789", "tire", 4, 79.95);
            
        System.out.println("Invoices and Employees processed polymorphically:\n");

        for (Payable p : pays) {
            // increase salary of SalariedEmployees by 10%
            if (p instanceof SalariedEmployee) {
                SalariedEmployee emp = (SalariedEmployee) p;
                emp.setWeeklySalary(emp.weeklySalary() * 1.1);
            }

            // print the payment
            System.out.printf("%s \n%s: $%,.2f\n\n", 
            p, "payment due", p.paymentAmount());
        }

        for (int i = 0; i < pays.length; ++i)
            System.out.printf("pays[%d] is a %s\n", i, pays[i].getClass().getName());
    }
}