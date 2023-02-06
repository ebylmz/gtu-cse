public class SingleLinkedListTest {
    public static void main(String[] args) {    
        test1();
        System.out.println();
        test2();
        System.out.println();
    }

    public static void test1() {
        String[] ingredientList = {
            "White meat", "Egg", "Yoghurt", "Banana", "Turkey", "Oat", "Salmon",
            "Honey", "Vinegar", "Rice", "Peanut butter", "Nuts", "Bitter chocolate"
        };

        SingleLinkedList<String> list = new SingleLinkedList<>();

        for (int i = 0; i < ingredientList.length; ++i)
            list.add(ingredientList[i]);

        System.out.printf("List size: %d\n", list.size());
        
        list.remove("Yoghurt");
        list.remove("Rice");
        list.remove("Hamburger");   // not in list
        list.remove("Nutella");     // not in list
        list.remove("Pizza");       // not in list
        
        Object[] mealList = list.toArray();
        System.out.println("Meal list: ");
        for (int i = 0; i < mealList.length; ++i)
            System.out.print(mealList[i] + ((list.size() != i + 1) ? ", " : "\n"));
        System.out.printf("List size: %d\n", list.size());

        list.clear();
        System.out.printf("List size: %d\n", list.size());
    }

    public static void test2() {
        SingleLinkedList<Integer> list = new SingleLinkedList<>();
        list.add(10);       // list: {10}
        list.addFirst(5);   // list: {5, 10}
        list.add(20);       // list: {5, 10, 20}
        list.addLast(25);   // list: {5, 10, 20, 25} 
        list.add(2, 15);    // list: {5, 10, 15, 20, 25}

        System.out.println(list);
        System.out.printf("List size: %d\n", list.size());
        
        int e1 = 13, e2 = 15, e3 = 20;
        System.out.printf("List has entry %d: %s\n", e1, list.contains(e1));
        System.out.printf("List has entry %d: %s\n", e2, list.contains(e2));
        // list.remove(3); // remove 4th entry which is 20
        System.out.printf("List has entry %d: %s\n", e3, list.contains(e3));

        for (var e : list)
            list.remove(e);
        System.out.printf("List size: %d\n", list.size());
    }
}