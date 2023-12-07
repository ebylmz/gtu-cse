/**
 * @file    dayOfYearSet.h
 * @author  Emirkan Burak Yılmaz 
 * @brief   Interface file for DayOfYearSet class   
 * @version 0.1
 * @date    2021-12-04
 * 
 * @copyright Copyright (c) 2021
 */

#ifndef DAY_OF_YEAR_SET
#define DAY_OF_YEAR_SET

#include <vector>
#include <fstream>

using std::vector;
using std::ostream;
using std::istream;
using std::ofstream;
using std::ifstream;

namespace DoYGTU {
    class DayOfYearSet {
    public:
        // inner class DayOfYear
        class DayOfYear {
        public:
            DayOfYear (int month = 1, int day = 1);
            // default DayOfYear objects initialized as January 1

            int setDay (int day);
            // in case of invalid day returns EXIT_FAILURE
            // and current date does not change
            int setMonth (int month);
            // in case of invalid month returns EXIT_FAILURE
            // and current date does not change

            int getDay () const;
            int getMonth () const;

            int dayBetween (const DayOfYear & other) const; 
            // returns the date between to date
            // return negative values in case of other day later than this day

            int daySoFar () const;
            // returns the number of the day passed so far

            void print () const;
            // prints the day as words format

            friend ostream & operator<< (ostream & outs, const DayOfYear & d);
            friend istream & operator>> (istream & ins, DayOfYear & d);

            friend bool operator== (const DayOfYear & d1, const DayOfYear & d2);
            friend bool operator!= (const DayOfYear & d1, const DayOfYear & d2);

            DayOfYear operator+ (int forward) const; 
            // returns the date of the forward day later
            DayOfYear operator- (int backward) const; 
            // returns the date of the backward day later
            
            DayOfYear operator++ ();    // pre increment 
            DayOfYear operator++ (int); // post increment
            // set the day as next day (tomarrow)  
            DayOfYear operator-- ();    // pre decrement
            DayOfYear operator-- (int); // post decrement 
            // set the day as previos day (yesterday)
        private:
            int dayInMonth () const;
            // returns the total day in current month
            int dayInMonth (int month) const;
            // returns the total day in given month
            
            int _day;
            int _month;
        }; // inner class DayOfYear

        DayOfYearSet (const DayOfYearSet & s);
        // copy constructor        
        DayOfYearSet (int capacity = 0);
        // initialize the set with specific capacity, defaulty it's empty set
        DayOfYearSet (const vector<DayOfYear> & v);
        // construct the DayOfYearSet object same values as given set
        DayOfYearSet (const char * filename);
        // construct the DayOfYearSet object according to data at given file
        ~DayOfYearSet ();
        // deconstructor

        bool isInSet (const DayOfYear & element) const;
        // checks if set contains given element 
        
        int add (int month, int day);
        int add (const DayOfYear & element);
        // returns EXIT_SUCCESS: successful execution
        // returns EXIT_FAILURE: unsuccessful execution (dublicated values or insuffucient memory)

        int remove (const DayOfYear & element);
        int remove (int index);
        // returns EXIT_SUCCESS: successful execution
        // returns EXIT_FAILURE: unsuccessful execution (non-member element)

        void empty ();
        // deletes all the set and set it as empty set

        int resize (int newsize);
        // resize the size of the set

        int load (const char * filename);
        // construct the DoY set with given elements of set 
        int save (const char * filename) const;
        // saves the elements of the current DoY set to the given file 

        int size () const;
        // returns the number of alive element inside of the set
        int capacity () const;
        // returns the current capacity of the set
        static int AllDoY ();
        // returns the total number of DayOfYear objects alive in all the sets

        friend ostream & operator<< (ostream & outs, const DayOfYearSet & s);

        DayOfYearSet operator= (const DayOfYearSet & other);
        bool operator== (const DayOfYearSet & other);
        bool operator!= (const DayOfYearSet & other);
        // two sets are equal if their elements are equal regardless of the keeping order  

        DayOfYearSet operator+ (const DayOfYearSet & other);
        // returns the union set
        DayOfYearSet operator- (const DayOfYearSet & other);
        // returns the difference set
        DayOfYearSet operator^ (const DayOfYearSet & other);
        // returns the intersection set
        DayOfYearSet operator! ();
        // returns the complement set

        DayOfYear & operator[] (int index);             
        // returns the element at given position as "lvalue"
        const DayOfYear & operator[] (int index) const;  
        // returns the element at given position as "rvalue"
    private:
        static int _AllDoY;   // total number of DayOfYear objects alive in all the sets
        int _size;           
        int _capacity;
        DayOfYear * _set;
    }; // class DayOfYearSet
} // namespace DoYGTU 

#endif