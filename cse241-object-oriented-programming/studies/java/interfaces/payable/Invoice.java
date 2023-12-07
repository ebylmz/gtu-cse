package payable;

public class Invoice implements Payable {
    private String _partNumber;
    private String _partDescription;
    private int _quantity;
    private double _pricePerItem;

    public Invoice (String partNumber, String partDescription, int quantity, double pricePerItem) {
        _partNumber = partNumber;
        _partDescription = partDescription;
        setQuantity(quantity);
        setPricePerItem (pricePerItem);
    }

    public void setPartNumber (String partNumber) {_partNumber = partNumber;}
    
    public void setpartDescription (String description) {_partDescription = description;}

    public void setQuantity (int quantity) {
        _quantity = quantity < 0 ? 0 : quantity;
    }

    public void setPricePerItem (double price) {
        _pricePerItem = price < 0.0 ? 0.0 : price;
    }

    public String partNumber () {return _partNumber;}
    
    public String partDescription () {return _partDescription;}

    public int quantity () {return _quantity;}

    public double pricePerItem () {return _pricePerItem;}

    public String toString () {
        return String.format("%s: \n%s: %s (%s) \n%s: %d \n%s: $%,.2f", 
            "invoice", "part number", partNumber(), partDescription(),
            "quantity", quantity(), "price per item", pricePerItem());
    }

    public double paymentAmount () {
        return quantity() * pricePerItem();
    }
}