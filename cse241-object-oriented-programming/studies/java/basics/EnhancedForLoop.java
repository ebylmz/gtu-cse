public class EnhancedForLoop {
    // compiler converts enhanced for loop (range based loop in C++)
    // for (Object o : list) {
    //     // do stuff
    // }
  
    // Iterator<Object> it = list.iterator();
    // while (it.hasNext()) {
    //     Object o = it.next();
    //     // do stuff
    // }    

    // variable lenght argument list
    // numbers actually double[]
    public static double average (double ... numbers) {
        if (numbers.length == 0)
            throw new IllegalArgumentException("no value provided");
        
        double total = 0.0;
        for (double d : numbers)
            total += d;
        return total / numbers.length;
    }


    public static void main (String[] args) {
        System.out.printf("average %.2f\n", average(1.0, 2.0, 3.0, 4.0));

        double[] n = {1.1, 2.2, 3.3, 4.4, 5,5};
        System.out.printf("average %.2f\n", average(n));
    }    
}
