package application;

import java.util.ArrayDeque;
import java.util.Deque;

public class PalindromeFinder {
    /**
     * The class Stack extends class Vector, which is the historical predecessor
     * of ArrayList. Just as they suggest using class Deque instead of the Stack class
     * in new applications, the Java designers recommend using class ArrayList instead of class Vector.
     */

    /** The clas Stack extends class Vector which implements the List interface. 
     * With this implementation Stack becomes much more than Stack. User can add/remove/set
     * new values by using inherited methods comes from List interface. So using Deque
     * as stack much more meaningfull because it does not implement List interface. 
     * It still problemetic but Deque only allow add first and addLast */

    private static Deque<Character> fillStack(String inputStr) {
        Deque<Character> charStack = new ArrayDeque<>();
        for (int i = 0; i < inputStr.length(); ++i)
            charStack.push(inputStr.charAt(i));
        return charStack;
    }

    private String buildReverse(String inputStr) {
        var charStack = fillStack(inputStr);
        StringBuffer buffer = new StringBuffer();
        while (!charStack.isEmpty())
            buffer.append(charStack.pop());
        return buffer.toString();
    }

    public boolean isPalindrome(String inputStr) {
        return inputStr.equals(buildReverse(inputStr));
    }
}