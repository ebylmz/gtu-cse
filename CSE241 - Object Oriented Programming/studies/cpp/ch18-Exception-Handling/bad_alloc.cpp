#include <iostream>
#include <new>
using std::bad_alloc;

template<class T>
struct Node {
    T data;
    Node * next;
};

int main (void) {
    

    Node<int> * head = new Node<int>[1000];
    Node<int> ** trav = &head;
    int i = 0;
    while (1) {
        try {
            (*trav)->next = new Node<int>[1000];
            (*trav)->data = ++i;
            trav = &(*trav)->next;
        }
        catch (bad_alloc) {
            std::cout << "Run out of memory at " << i << std::endl;
            break;
        }
    }
}