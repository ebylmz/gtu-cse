#ifndef MEMORY_H
#define MEMORY_H

#include <iostream>
#include <fstream>
#include <unordered_map>
#include <pthread.h>
#include <list>

#define WSCLOCK_TAU 5

enum AccessMode {
    ACC_READ, ACC_WRITE
};

class PTE {
public:
    PTE(int virtualFrameID, int physicalFrameID);
    
    ~PTE();

    void reset(int virtualFrameID, int physicalFrameID);

    void setVirtualFrameID(int virtualFrameID); 
    
    void setPhysicalFrameID(int physicalFrameID); 

    void setReferenced(bool referenced); 
    
    void setModified(bool modified); 

    void setTime(unsigned long int time); 

    bool getReferenced(); 
    
    bool getModified(); 

    int getPhysicalFrameID(); 
    
    int getVirtualFrameID(); 
    
    unsigned long int getTime(); 

private:
    bool referenced; 
    bool modified;
    bool presentAbsent;
    int virtualFrameID;
    int physicalFrameID; 
    unsigned long int time;
};

class InvertedPageTable {
public:
    enum PageReplacementAlgo {SC, LRU, WSClock};

    InvertedPageTable(int capacity, int frameSize, PageReplacementAlgo replacement, unsigned int tau = 20);
    
    ~InvertedPageTable();

    long map(long virtualAddr, enum AccessMode mode);

    std::list<PTE *>::iterator findEntry(int virtualFrameID);

    int getSize();

    bool isFull();

    void printTable();

    bool addPage(PTE *entry);
    
    std::list<PTE *> getPageList();

    PTE *removePage(int virtualFrameID);

    PTE *evictPage(std::vector<PTE *> &modifiedPages);

    PTE *evictPage_SC();

    PTE *evictPage_LRU();

    PTE *evictPage_WSClock(std::vector<PTE *> &modifiedPages);

    void clearReferenceBit();

private:
    int capacity;
    int frameSize; 
    unsigned long int sysclock;
    const unsigned int tau;
    PageReplacementAlgo replacement;
    std::unordered_map<int, int> pageTable; 
    std::list<PTE *> pageList; 
};

class MemStats {
public:
    unsigned long int numRead;
    unsigned long int numWrite;
    unsigned long int numPageMiss;
    unsigned long int numPageReplacement;
    unsigned long int numDiskPageWrite;
    unsigned long int numDiskPageRead;
    unsigned long int workingSetFunction;

    MemStats();

    ~MemStats();

    void print();
};

class MemoryManagement
{
public:
    MemoryManagement(
        unsigned long int frameSize, 
        unsigned long int numPhysicalFrames, 
        unsigned long int numVirtualFrames, 
        InvertedPageTable::PageReplacementAlgo replacement,
        const char *diskFileName);

    ~MemoryManagement();

    int getVirtualMemSize();
    
    int getPhysicalMemSize();
    
    int getFrameSize();

    int read(long virtualAddr);
    
    int write(long virtualAddr, int data);
    
    void printStats();

private:
    unsigned long int frameSize;
    unsigned long int numVirtualFrames;
    unsigned long int numPhysicalFrames;
    const char *diskFileName;
    int *physicalMem;
    InvertedPageTable pageTable;
    pthread_mutex_t mutex;
    MemStats stats;

    unsigned long int access(long virtualAddr, AccessMode mode); 

    unsigned long int diskPosition(int virtualFrameID);

    unsigned long int memoryPosition(int physicalFrameID); 

    int getFrameID(unsigned long int addr);

    int getFrameOffset(unsigned long int addr);

    void initDisk();

    void readDisk(PTE *entry);  

    void writeDisk(PTE *entry); 
};

#endif