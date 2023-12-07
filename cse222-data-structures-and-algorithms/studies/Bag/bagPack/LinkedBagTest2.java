package bagPack;

public class LinkedBagTest2 {
    public static void main(String[] argv) {
        String[] mostUsedPasswords = {
            "12345", "qwerty", "asdfg", "54321", "dragon", "sunshine",
            "monkey", "princess", "iloveyou1", "password1", "abc123"
        };

        BagInterface<String> crackerList = new LinkedBag<String>();

        for (int i = 0; i < mostUsedPasswords.length; ++i)
            crackerList.add(mostUsedPasswords[i]);
        
        String[] removePasswords = {"monkey", "lionKing", "cr7123", "12345"};

        for (var pass : removePasswords)
            if (crackerList.remove(pass))
                System.out.printf("removed %s\n", pass);
            else
                System.out.printf("can't removed %s\n", pass);

        
        crackerList.remove("monkey");
        crackerList.remove("princes");
        // crackerList.clear();

        while (! crackerList.isEmpty())
            System.out.printf("try password: %-16s ...\n", crackerList.remove());
        System.out.println("All the password(s) are tried");
    }
}