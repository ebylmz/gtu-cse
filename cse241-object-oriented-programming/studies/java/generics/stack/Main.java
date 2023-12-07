public class Main {
    public static void main (String[] args) {
        Stack<String> s = new Stack<String>();
        s.push("EBY");  // Emirkan Burak Yılmaz
        s.push("MKA");  // Mustafa Kemal Ataturk
        s.push("JBP");  // Jordan B Peterson
        s.push("YSA");  // Yusuf Sinan Akgul
        s.push("YG");   // Yakup Genc
        s.push("LV");   // Leonardo da Vinci  
        s.push("CR");   // Cristiano Ronaldo               
        s.push("AE");   // Albert Einstein
        s.push("GW");   // George Washington
        s.push("AL");   // Abraham Lincoln
        s.push("MJ");   // Mary Jane

        // s.display();
        // s.displayReverse();

        System.out.printf("stack: %s\n", s);
    }
}