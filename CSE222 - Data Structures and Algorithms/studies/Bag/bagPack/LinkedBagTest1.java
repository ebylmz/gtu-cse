package bagPack;

public class LinkedBagTest1 {
    public static void main(String[] args) {
        String[] ingredientList = {
            "White meat", "Egg", "Yoghurt", "Banana", "Turkey", "Oat", "Salmon",
            "Honey", "Vinegar", "Rice", "Peanut butter", "Nuts", "Bitter chocolate"
        };

        BagInterface<String> bag = new LinkedBag<String>();
        
        for (int i = 0; i < ingredientList.length; ++i)
            bag.add(ingredientList[i]);

        //! this should be Object otherwise java interpreter yells us 
        Object[] mealList = bag.toArray();

        System.out.println("Meal list: ");
        for (int i = 0; i < mealList.length; ++i)
            System.out.println(mealList[i]);
    }
}
