public class PrintArr {
    public static <E> void printArr (E[] arr) {
        for (E element : arr)
            System.out.printf("%s ", element);
        System.out.println();
    }

    public static void main (String[] args) {
        // primitiver types (int, char ...) cannot be used as generic type
        // anything that is used as generics has to be convertable to object
        Integer[] intArr = {1, 2, 3, 4, 5};
        Double[] doubleArr = {1.1, 2.2, 3.3, 4.4, 5.5};
        Character[] charArr = {'C', 'O', 'U', 'R', 'A', 'G', 'E'};
        
        System.out.println("Array intArr contains:");
        printArr(intArr);
        System.out.println( "Array doubleArr contains:");
        printArr(doubleArr);
        System.out.println("Array charArr contains:");
        printArr(charArr);
    }
}
