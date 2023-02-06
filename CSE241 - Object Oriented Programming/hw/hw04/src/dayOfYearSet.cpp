/**
 * @file    dayOfYearSet.cpp
 * @author  Emirkan Burak Yılmaz 
 * @brief   Implementation file for DayOfYearSet class   
 * @version 0.1
 * @date    2021-12-04
 * 
 * @copyright Copyright (c) 2021
 */

#include <iostream>
#include <vector>
#include "dayOfYearSet.h"

using std::vector;
using std::ostream;
using std::cout;
using std::cin;
using std::cerr;
using std::endl;

namespace DoYGTU {

    /******************************************************************************
     *                              DayOfYearSet::DayOfYear
     ******************************************************************************/
    
    DayOfYearSet::DayOfYear::DayOfYear (int month, int day) {
        // first set the month, than set the day 
        // since each month don't have same number of days
        bool r = setMonth(month) == EXIT_SUCCESS && setDay(day) == EXIT_SUCCESS;
        if (r == false) {
            cerr << "Aborted (!)\n";
            exit(1); 
        }
    }

    int DayOfYearSet::DayOfYear::DayOfYear::setDay (int day) {
        if (1 <= day && day <= dayInMonth()) {
            _day = day;
            return EXIT_SUCCESS;   
        }
        else {
            cerr << "Invalid day value: " << day << " (!)\n";
            return EXIT_FAILURE;
        }
    }

    int DayOfYearSet::DayOfYear::DayOfYear::setMonth (int month) {
        if (1 <= month && month <= 12) {
            _month = month;
            return EXIT_SUCCESS;   
        }
        else {
            cerr << "Invalid month value: " << month << " (!)\n";
            return EXIT_FAILURE;
        }
    }

    inline int DayOfYearSet::DayOfYear::getDay () const {return _day;}
    inline int DayOfYearSet::DayOfYear::getMonth () const {return _month;}

    int DayOfYearSet::DayOfYear::dayBetween (const DayOfYear & other) const {
        const int DAY_IN_YEAR = 365;
        int diff = other.daySoFar() - daySoFar();
        if (diff < 0) 
            diff += DAY_IN_YEAR;
        return diff;
    } 

    int DayOfYearSet::DayOfYear::daySoFar () const {
        int totalDay = 0;
        for (decltype(totalDay) i = 1; i < getMonth(); ++i)
            totalDay += dayInMonth(i);
        return totalDay + getDay();
    }

    void DayOfYearSet::DayOfYear::print () const {
        switch (getMonth()) {
            case  1: cout << "January";     break;
            case  2: cout << "February";    break;
            case  3: cout << "March";       break;
            case  4: cout << "April";       break;
            case  5: cout << "May";         break;
            case  6: cout << "June";        break;
            case  7: cout << "July";        break;
            case  8: cout << "August";      break;
            case  9: cout << "September";   break;
            case 10: cout << "October";     break;
            case 11: cout << "November";    break;
            case 12: cout << "December";    break;
        }
        cout << " " << getDay();
    }

    ostream & operator<< (ostream & outs, const DayOfYearSet::DayOfYear & d) {
        outs << d.getMonth() << "/" << d.getDay();
        return outs;
    }

    istream & operator>> (istream & ins, DayOfYearSet::DayOfYear & d) {
        int day, month;
        char c;

        ins >> month >> c >> day;
        if (c == '/') {
            d.setMonth(month);
            d.setDay(day);
        }
        return ins;
    }

    bool operator== (const DayOfYearSet::DayOfYear & d1, const DayOfYearSet::DayOfYear & d2) {
        return d1.getDay() == d2.getDay() && d1.getMonth() == d2.getMonth();
    }

    bool operator!= (const DayOfYearSet::DayOfYear & d1, const DayOfYearSet::DayOfYear & d2) {
        return !(d1 == d2);
    }

    DayOfYearSet::DayOfYear DayOfYearSet::DayOfYear::operator+ (int forward) const {
        DayOfYear newDay(*this);

        // calls operator- in case of the stituation like this: day + -12;
        if (forward < 0)
            newDay = *this - -forward;
        else 
            for (int i = 0; i < forward; ++i)
                ++newDay;
        return newDay;
    } 

    DayOfYearSet::DayOfYear DayOfYearSet::DayOfYear::operator- (int backward) const {
        DayOfYear newDay(*this);

        // calls operator+ in case of the stituation like this: day - -12;
        if (backward < 0)
            newDay = *this + -backward;
        else
            for (int i = 0; i < backward; ++i)
                --newDay;
        return newDay;
    } 

    DayOfYearSet::DayOfYear DayOfYearSet::DayOfYear::operator++ () {
        // if today is the last day of month
        // set the day as first day of next month
        if (_day == dayInMonth()) {
            _month = (_month == 12) ? 1 : _month + 1; 
            _day = 1;   
        }
        else 
            ++_day;
        return *this; 
    } 
    DayOfYearSet::DayOfYear DayOfYearSet::DayOfYear::operator++ (int) {
        DayOfYearSet::DayOfYear tmp(*this);
        ++(*this);
        return tmp;
    }

    DayOfYearSet::DayOfYear DayOfYearSet::DayOfYear::operator-- () {
        // if today is the first day of month
        // set the day as last day of previos month
        if (_day == 1) {
            _month = (_month == 1) ? 12 : _month - 1;
            _day = dayInMonth();
        }
        else 
            --_day;
        return *this;
    } 

    DayOfYearSet::DayOfYear DayOfYearSet::DayOfYear::operator-- (int) {
        DayOfYearSet::DayOfYear tmp(*this);
        --(*this);
        return tmp;
    }

    int DayOfYearSet::DayOfYear::dayInMonth () const {
        switch (_month) {
            case  1: return 31;
            case  2: return 28;
            case  3: return 31;
            case  4: return 30;
            case  5: return 31;
            case  6: return 30;
            case  7: return 31;
            case  8: return 31;
            case  9: return 30;
            case 10: return 31;
            case 11: return 30;
            case 12: return 31;
            // returns 0 for invalid month values
            default: return 0;  
        }
    }

    int DayOfYearSet::DayOfYear::dayInMonth (int month) const {
        DayOfYear tmp(month);
        return tmp.dayInMonth();
    }

    /******************************************************************************
     *                             DayOfYearSet
     ******************************************************************************/

    DayOfYearSet::DayOfYearSet (const DayOfYearSet & s)
    : _size(s.size()), _capacity(s.capacity()) {
        _set = new DayOfYearSet::DayOfYear[s.capacity()];
        if (_set != nullptr) {
            for (int i = 0; i < s.size(); ++i)
                _set[i] = s[i];

            _AllDoY += s.size();    // new DoY's created 
        }
        else {
            cerr << "No enough memory. Aborted (!)\n";
            exit(1);
        }
    }
    
    DayOfYearSet::DayOfYearSet (int capacity) 
    : _capacity(capacity), _size(0), _set(nullptr) {
        if (capacity < 0) {
            cerr << "Negative capacity. Aborted (!) \n";
            exit(1);
        }
        else {
            _set = new DayOfYearSet::DayOfYear[_capacity];
            if (_set == nullptr) {
                cerr << "No enough memory. Aborted (!)\n";
                exit(1);
            }
        }
    }

    DayOfYearSet::DayOfYearSet (const vector<DayOfYearSet::DayOfYear> & v) 
    : _capacity(v.capacity()), _size(0) {
        _set = new DayOfYearSet::DayOfYear[capacity()];
        if (_set != nullptr) {
            // dublicated values are ignored rather then exit(1)
            for (int i = 0; i < v.size(); ++i)
                if (add(v[i]) == EXIT_FAILURE)
                    cerr << "Dublicated value, ignored (!)\n";
        }  
        else {
            cerr << "No enough memory. Aborted (!)\n";
            exit(1);
        }
    }

    DayOfYearSet::DayOfYearSet (const char * filename) : _capacity(0), _size(0), _set(nullptr) {
        load(filename);
    }

    DayOfYearSet::~DayOfYearSet () {
        _AllDoY -= size();  // killed DoY's
        delete [] _set;
    }

    bool DayOfYearSet::isInSet (const DayOfYearSet::DayOfYear & element) const {
        for (auto i = 0; i < size(); ++i)
            if (_set[i] == element)
                return true;
        return false;
    }

    int DayOfYearSet::add (int month, int day) {
        return add(DayOfYear(month, day));
    }

    int DayOfYearSet::add (const DayOfYearSet::DayOfYear & element) {
        int r;

        // no duplicates are allowed in a set
        if (!isInSet(element)) {
            // resize the set if needed
            if (size() == capacity())
                r = (capacity() == 0) ? resize(10) : resize(capacity() * 2);
            else
                r = EXIT_SUCCESS;

            // add the new element to the last index
            if (r == EXIT_SUCCESS) {
                _set[size()] = element;
                ++_size;
                ++_AllDoY;     // new active DoY object created
            }
        }
        else r = EXIT_FAILURE;

        return r;
    }

    int DayOfYearSet::remove (const DayOfYearSet::DayOfYear & element) {
        for (int i = 0; i < size(); ++i)
            if (_set[i] == element)
                return remove(i);
        return EXIT_FAILURE;
    }

    int DayOfYearSet::remove (int index) {
        if (0 <= index && index < size()) {
            // to maintain the set order which provided user
            // move each elements towards to deleted place after removing 
            for (int i = index + 1; i < size(); ++i)
                _set[i - 1] = _set[i];
            --_size;    // one element removed
            --_AllDoY;   // one DoY object killed
            return EXIT_SUCCESS;
        }
        else {
            cerr << "Invalid index for remove operation (!)\n";
            return EXIT_FAILURE;
        }
    }

    void DayOfYearSet::empty () {
        // no need to change the capacity
        _AllDoY -= size();  // killed DoY's
        _size = 0;
    }

    int DayOfYearSet::resize (int newCapacity) {
        DayOfYearSet::DayOfYear * tmp = new DayOfYearSet::DayOfYear[newCapacity]; 
        if (tmp != nullptr) {
            _capacity = newCapacity;
            
            // copy the values inside of old array to new allocated array
            for (int i = 0; i < size() /* && set[i] != nullptr */; ++i)
                tmp[i] = _set[i];
            
            delete [] _set;

            _set = tmp;
            return EXIT_SUCCESS;
        }
        else {
            cerr << "No enough memory for new allocation (!)\n";
            return EXIT_FAILURE;
        }
    }

    int DayOfYearSet::load (const char * filename) {
        ifstream ins(filename);
        if (! ins.fail()) {
            DayOfYear tmp;

            while (ins >> tmp)
                add(tmp);   // prevents dublicated values
            
            ins.close();
            return EXIT_SUCCESS;
        }
        else
            return EXIT_FAILURE;
    }

    int DayOfYearSet::save (const char * filename) const {
        ofstream outs(filename);
        
        if (! outs.fail()) {
            for (int i = 0; i < size(); ++i) {
                outs << _set[i];
                if (i + 1 < size()) 
                    outs << endl;
            }
            outs.close();
            return EXIT_SUCCESS;
        }
        else
            return EXIT_FAILURE;
    }

    inline int DayOfYearSet::size() const {return _size;}

    inline int DayOfYearSet::capacity () const {return _capacity;}

    int DayOfYearSet::AllDoY () {return _AllDoY;}

    ostream & operator<< (ostream & outs, const DayOfYearSet & s) {
        cout << "{";

        for (int i = 0; i < s.size(); ++i) {
            outs << s[i];
            if (i + 1 < s.size())
                outs << ", ";
        }
        outs << "} (size: " << s.size() << ")";
            
        return outs;
    }

    DayOfYearSet DayOfYearSet::operator= (const DayOfYearSet & other) {
        // update the capacity of the set
        if (capacity() != other.capacity()) {
            delete [] _set;
            _AllDoY -= size();  // killed DoY's
            _capacity = other.capacity();
            _set = new DayOfYearSet::DayOfYear[capacity()];
        }
        _size = other.size();
        _AllDoY += size();  // killed DoY's

        for (int i = 0; i < size(); ++i)
            _set[i] = other[i];
        
        return *this; 
    }

    bool DayOfYearSet::operator== (const DayOfYearSet & other) {
        for (int i = 0; i < other.size(); ++i) 
            if (!isInSet(other[i]))
                return false;
        return true;
    }

    bool DayOfYearSet::operator!= (const DayOfYearSet & other) {
        return !(*this == other);
    }

    DayOfYearSet DayOfYearSet::operator+ (const DayOfYearSet & other) {
        DayOfYearSet unionSet(*this);  
        for (int i = 0; i < other.size(); ++i)
            unionSet.add(other[i]);  // add functions does not allow dublicated values 
        return unionSet;
    }

    DayOfYearSet DayOfYearSet::operator- (const DayOfYearSet & other) {
        DayOfYearSet diffSet;   // difference set

        // add the elements which are belongs to only this set
        for (int i = 0; i < size(); ++i)
            if (! other.isInSet(_set[i]))  
                diffSet.add(_set[i]);

        return diffSet;
    }

    DayOfYearSet DayOfYearSet::operator^ (const DayOfYearSet & other) {
        DayOfYearSet isection;  // intersection set

        // add the elements which are belongs to both this and other set
        for (int i = 0; i < other.size(); ++i)
            if (isInSet(other[i]))
                isection.add(other[i]);
        return isection;
    }
  
    DayOfYearSet DayOfYearSet::operator! () {
        const int DAY_IN_YEAR = 365;
        DayOfYearSet::DayOfYear day;  // initialized as January 1
        DayOfYearSet complementSet;

        // check all the day in a year
        for (int i = 0; i < DAY_IN_YEAR; ++i, ++day)
            if (!isInSet(day)) // add all the days except the days in this set 
                complementSet.add(day);
        return complementSet;
    }

    // implementation for operator[] as left hand side
    DayOfYearSet::DayOfYear & DayOfYearSet::operator[] (int index) {
        // desired index should be less than size not capacity
        // because other DoY object could be used and removed
        // remember remove operation just decrease the size 
        if (index < size()) 
            return _set[index];
        else {
            cerr << "Invalid set index. Aborted (!)\n";
            exit(1);
        }
    }

    // implementation for operator[] as right hand side
    const DayOfYearSet::DayOfYear & DayOfYearSet::operator[] (int index) const {
        if (index < size())
            return _set[index];
        else {
            cerr << "Invalid set index. Aborted (!)\n";
            exit(1);
        }
    }
} // namespace DoYGTU 