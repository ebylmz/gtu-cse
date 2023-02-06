public class CommandLineArguments {
    public static void main (String[] args) {
        if (args.length != 3)
            System.out.println(
                "Error: Please re-enter the entire command, including\n" +
                "an array size, initial value and increment.");
        else {
            int length = Integer.parseInt(args[0]);
            int initVal = Integer.parseInt(args[1]);    
            int increment = Integer.parseInt(args[2]);    
            
            int[] arr = new int[length];
            for (int i = 0; i < arr.length; ++i)
                arr[i] = initVal + increment * i;
            
            System.out.println("Here your array");
            for (var v : arr)
                System.out.printf("%d ", v);
            System.out.println();
        }
    }
}
