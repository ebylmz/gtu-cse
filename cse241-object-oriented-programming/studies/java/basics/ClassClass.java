public class ClassClass {
    public static void main (String[] args) {
        String str = "";
        System.out.println(str.getClass().getName());
        

        // wrapper class Double initialize like primitive type
        Double doubleObj = 1.1;
        doubleObj.getClass().getName();
        
        int[][][] arr = new int[2][4][8];
        System.out.println(arr.getClass().getName());
        
        Object obj = new Object();
        System.out.println(obj.getClass().getName());


        // Element Type	     Encoding
        // boolean              Z
        // byte                 B
        // char                 C
        // class or interface	Lclassname;
        // double               D
        // float                F
        // int                  I
        // long                 J
        // short                S
    }        
}
