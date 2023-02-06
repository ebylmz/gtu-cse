package itemPack;

public class Item implements Cloneable {
    private String name;
    private double price;

    public Item(String name, double price) {
        this.name = name;
        this.price = price;
    }

    public Item(String name) {this(name, 0);}

    public Item() {this(null, 0);}

    public String getName() {return name;}

    public double getPrice() {return price;}

    @Override
    public Item clone() {
        try {
            // shallow copy works since Strings are immutable and price is double  
            return (Item) super.clone();
        }
        catch (CloneNotSupportedException e) {
            // this will never happen
            return null;
        }
    }

    @Override
    public boolean equals(Object other) {
        boolean r;
        if (this == other)
            r = true;
        else if (other instanceof Item) {
            Item o = (Item) other;
            r = getName().equals(o.getName()) && getPrice() == o.getPrice();
        }
        else
            r = false;
        return r;
    }

    @Override
    public String toString() {
        return String.format("%-30s $%.2f", name, price);
    }
}