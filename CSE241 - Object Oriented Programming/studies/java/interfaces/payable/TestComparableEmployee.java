package payable;

public class TestComparableEmployee {
    public static Employee maxPayment (Employee[] e) throws NullPointerException, IllegalArgumentException {
        if (e == null)
            throw new NullPointerException();
        else if (e.length == 0) 
            throw new IllegalArgumentException("empty array");
        
        Employee max = e[0];
        for (int i = 1; i < e.length; ++i)
            if (e[i].compareTo(max) > 1)
                max = e[i];

        return max;
    }
    public static void main (String[] args) {
        Employee emp[] = new Employee[4];
        emp[0] = new SalariedEmployee("Emirkan Burak", "Yilmaz", "111-11-1111", 1800.00);
        emp[1] = new SalariedEmployee("Bugs", "Bunny", "222-22-2222", 1970.00);
        emp[2] = new SalariedEmployee("Mary", "Jane", "333-33-3333", 250.00);
        emp[3] = new SalariedEmployee("Bob", "Lewis", "444-44-4444", 500.00);

        for (Employee e : emp)
            System.out.printf("%s\n\n", e);
        System.out.printf("Highes Salary Employee \n%s\n", maxPayment(emp));
    }
}
