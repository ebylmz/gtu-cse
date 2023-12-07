package bagPack;

import itemPack.Item;

public class ArrayBagTest {
    public static void main(String[] argv) {
        Item[] items = {new Item("Bird feeder", 20.50),
                        new Item("Squirrel guard", 15.47),
                        new Item("Bird bath", 44.99),
                        new Item("Sunflower seeds", 12.95)};

        BagInterface<Item> shoppingCart = new ArrayBag<Item>();
        
        // simulate getting item from shopper
        for (int i = 0; i < items.length; ++i)
            shoppingCart.add(items[i]);
       
        // simulate checkout
        double totalCost = .0;
        while (!shoppingCart.isEmpty()) {
            Item nextItem = shoppingCart.remove();
            totalCost += nextItem.getPrice();
            System.out.println(nextItem);
        }
        System.out.println("______________________________________");
        System.out.println(String.format("%-30s $%.2f", "Total cost: ", totalCost));
    }
}
