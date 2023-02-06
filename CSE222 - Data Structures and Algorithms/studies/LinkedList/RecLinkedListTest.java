public class RecLinkedListTest {
    public static void main(String[] args) {
        // add item at the end of the list
        RecLinkedList<String> list = new RecLinkedList<>();
        list.add("Jack");
        list.add("Jordan");
        list.add("Jenifer");
        list.add("Madonna");
        list.add("David");
        list.add("Maria");
        System.out.println("add item at the end of the list");
        System.out.println(list);
        // remove existing and non existing items
        list.remove("non exist");
        list.remove("ABC");
        list.remove("Jenifer");
        list.remove("Maria");
        list.remove("Jack");
        System.out.println("remove existing and non existing items");
        System.out.println(list);
        // add item by using index
        list.add(0, "Emirkan");
        list.add(2, "Faruk");
        list.add(3, "Ugurcan");
        list.add(list.size(), "Esra");
        // list.add(-1, "Mary");
        // list.add(12, "Mary");
        System.out.println("add item by using index");
        System.out.println(list);
        // remove item by using index
        list.remove(0);
        list.remove(list.size() / 2);
        list.remove(list.size() - 1);
        System.out.println("remove item by using index");
        System.out.println(list);
        // clear the list
        System.out.println("clear the list");
        list.clear();
        System.out.println(list);
    }
}
