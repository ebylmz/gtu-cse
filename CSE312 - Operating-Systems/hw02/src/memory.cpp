#include <iostream>
#include <fstream>
#include <vector>
#include <list>
#include "memory.h"

PTE::PTE(int virtualFrameID, int physicalFrameID) { reset(virtualFrameID, physicalFrameID); }

PTE::~PTE() {}

void PTE::reset(int virtualFrameID, int physicalFrameID)
{
    referenced = false;
    modified = false;
    presentAbsent = false;
    this->virtualFrameID = virtualFrameID;
    this->physicalFrameID = physicalFrameID;
}

void PTE::setVirtualFrameID(int virtualFrameID) 
{ 
    this->virtualFrameID = virtualFrameID; 
} 

void PTE::setPhysicalFrameID(int physicalFrameID) 
{ 
    this->physicalFrameID = physicalFrameID; 
} 

void PTE::setReferenced(bool referenced) 
{ 
    this->referenced = referenced; 
} 

void PTE::setModified(bool modified) 
{ 
    this->modified = modified; 
} 

void PTE::setTime(unsigned long int time)
{
    this->time = time;
}

bool PTE::getReferenced() 
{ 
    return referenced; 
} 

bool PTE::getModified() 
{ 
    return modified; 
} 

int PTE::getPhysicalFrameID() 
{ 
    return physicalFrameID; 
} 

int PTE::getVirtualFrameID() 
{ 
    return virtualFrameID; 
} 

unsigned long int PTE::getTime()
{
    return time;
}

InvertedPageTable::InvertedPageTable(int capacity, int frameSize, PageReplacementAlgo replacement, unsigned int tau)
: tau(tau)
{   
    this->capacity = capacity;
    this->frameSize = frameSize; 
    this->replacement = replacement;
    sysclock = 0; 
}

InvertedPageTable::~InvertedPageTable() 
{
    for (PTE *page : pageList)
        delete page;
}

std::list<PTE *>::iterator InvertedPageTable::findEntry(int virtualFrameID) 
{
    std::list<PTE *>::iterator it = pageList.begin();

    while (it != pageList.end() && (*it)->getVirtualFrameID() != virtualFrameID)
        ++it;
    return it;        
}

long InvertedPageTable::map(long virtualAddr, enum AccessMode mode)
{
    int virtualFrameID = virtualAddr / frameSize; 

    /* WSClock page replament algorithm requires to clear the reference bit for each frame in each clock tick */
    if (replacement == InvertedPageTable::PageReplacementAlgo::WSClock) {
        clearReferenceBit();
        /* Advance the system clock */
        ++sysclock;
    }

    if (pageTable.find(virtualFrameID) == pageTable.end())
        return -1;   

    std::list<PTE *>::iterator it = findEntry(virtualFrameID);
    PTE *entry = *it;

    /* Update the accessing information for the frame */
    entry->setReferenced(true);
    entry->setTime(sysclock);
    if (mode == AccessMode::ACC_WRITE)
        entry->setModified(true);

    /* Update the page list according to least recently used page */
    pageList.erase(it);
    pageList.push_back(entry);

    /* PA = PFRAMEID * FSIZE + VA % FSIZE */
    return entry->getPhysicalFrameID() * frameSize + virtualAddr % frameSize;
}

int InvertedPageTable::getSize() 
{ 
    return pageList.size(); 
}

bool InvertedPageTable::isFull() 
{ 
    return (int) pageList.size() < capacity ? false : true; 
}

void InvertedPageTable::printTable()
{
    std::cout << "\nPAGE TABLE\n";
    int i = 0;
    for (PTE *entry : pageList) {
        std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
        std::cout << i
            << "\t VF: " << entry->getVirtualFrameID() << " -> PF: " <<entry->getPhysicalFrameID() 
            << "\t M: " << entry->getModified() << "\t R: " << entry->getReferenced() << std::endl;
        ++i;
    }
    std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
}

bool InvertedPageTable::addPage(PTE *entry)
{
    std::unordered_map<int, int>::iterator it = pageTable.find(entry->getVirtualFrameID());
    if (it != pageTable.end()) 
        return false;   

    pageTable[entry->getVirtualFrameID()] = entry->getPhysicalFrameID();
    pageList.push_back(entry);
    return true;
}

std::list<PTE *> InvertedPageTable::getPageList() 
{
    return pageList;
}

PTE *InvertedPageTable::removePage(int virtualFrameID) 
{
    std::list<PTE *>::iterator it = findEntry(virtualFrameID);
    if (it == pageList.end()) {
        std::cout << "end of the list for page" << virtualFrameID << "\n";
        return nullptr;   
    }
    pageList.erase(it);
    pageTable.erase(virtualFrameID);
    return *it;
}

PTE *InvertedPageTable::evictPage(std::vector<PTE *> &modifiedPages)
{
    switch (replacement) {
        case SC:
            return evictPage_SC();
        case WSClock:
            return evictPage_WSClock(modifiedPages);
        case LRU:
        default:
            return evictPage_LRU();
    }
}

PTE *InvertedPageTable::evictPage_SC() 
{
    while (true) {
        PTE *oldestPage = pageList.front();
        /* Check the reference bit of the page */
        if (oldestPage->getReferenced()) {
            /* Page has been referenced, reset the reference bit */
            oldestPage->setReferenced(false);
            /* Move the page to the end of the list */
            pageList.pop_front();
            pageList.push_back(oldestPage);  
        } 
        else {
            /* Page can be evicted */
            return removePage(oldestPage->getVirtualFrameID());
        }
    }
}

PTE *InvertedPageTable::evictPage_LRU()
{
    PTE *leastRecentPage = pageList.front();
    return removePage(leastRecentPage->getVirtualFrameID());
}

PTE *InvertedPageTable::evictPage_WSClock(std::vector<PTE *> &modifiedPages) 
{
    while (true) {
        PTE *currentPage = pageList.front();

        if (currentPage->getModified() || currentPage->getReferenced()) {
            /* Set the current clock time and set the R and M bit to 0 */
            if (currentPage->getModified()) {
                /* Clear the modified bit, and indicate that this page should write back to the disk */
                currentPage->setModified(false); 
                modifiedPages.push_back(currentPage);
            }

            currentPage->setReferenced(false);

            currentPage->setTime(sysclock);

            /* Move the page to the end of the list (circular linked list for clock algorithm) */
            pageList.pop_front();
            pageList.push_back(currentPage);  
        }
        else if (sysclock - currentPage->getTime() > tau) {
            /* check if the page's age to see if it's older than the tau */
            /* Page can be evicted */
            return removePage(currentPage->getVirtualFrameID());
        }
        /* Witch each condition check, the clok and arrow (front) are advanced */
        ++sysclock;
    }
}

void InvertedPageTable::clearReferenceBit()
{
    for (PTE *page : pageList)
        page->setReferenced(false);
}

MemStats::MemStats()
{
    numRead = 0;
    numWrite = 0;
    numPageMiss = 0;
    numPageReplacement = 0;
    numDiskPageWrite = 0;
    numDiskPageRead = 0;
    workingSetFunction = 0; 
}

MemStats::~MemStats()
{
}

void MemStats::print()
{
    std::cout << "# of Memory and Disk Operations\n";
    std::cout << "Read            : " << numRead << std::endl;
    std::cout << "Write           : " << numWrite << std::endl;
    std::cout << "Page Miss       : " << numPageMiss << std::endl;
    std::cout << "Page Replacement: " << numPageReplacement << std::endl;
    std::cout << "Disk Page Read  : " << numDiskPageRead << std::endl;
    std::cout << "Disk Page Write : " << numDiskPageWrite << std::endl;
    // std::cout << "Working Set Function: " << workingSetFunction << std::endl; 
}

MemoryManagement::MemoryManagement(
    unsigned long int frameSize, 
    unsigned long int numPhysicalFrames, 
    unsigned long int numVirtualFrames, 
    InvertedPageTable::PageReplacementAlgo replacement,
    const char *diskFileName)
    : pageTable(numPhysicalFrames, frameSize, replacement, WSCLOCK_TAU)
{
    this->frameSize = frameSize;
    this->numPhysicalFrames = numPhysicalFrames;
    this->numVirtualFrames = numVirtualFrames;
    this->diskFileName = diskFileName;

    /* Create the physical memory */
    physicalMem = new int[frameSize * numPhysicalFrames];

    pthread_mutex_init(&mutex, NULL);

    /* Initialize the disk file */
    initDisk();
}

MemoryManagement::~MemoryManagement()
{
    /* Write back the modified pages to disk */
    for (PTE *page : pageTable.getPageList())
        if (page->getModified())
            writeDisk(page);
    delete[] physicalMem;
}

int MemoryManagement::read(long virtualAddr)
{
    pthread_mutex_lock(&mutex);

    long physicalAddr = access(virtualAddr, ACC_READ);
    int val = physicalMem[physicalAddr];
    pthread_mutex_unlock(&mutex);
    stats.numRead += 1;
    return val;
}

int MemoryManagement::write(long virtualAddr, int data)
{
    pthread_mutex_lock(&mutex);

    long physicalAddr = access(virtualAddr, ACC_WRITE);

        #ifdef DEBUG
        std::cerr << "Write VA: " << virtualAddr << " PA:" << physicalAddr << " data: " << data << std::endl; 
        #endif

    physicalMem[physicalAddr] = data;
    stats.numWrite += 1;

    pthread_mutex_unlock(&mutex);

    return data;
}

int MemoryManagement::getVirtualMemSize()
{
    return numVirtualFrames * frameSize;
}

int MemoryManagement::getPhysicalMemSize()
{
    return numPhysicalFrames * frameSize;
}

int MemoryManagement::getFrameSize()
{
    return frameSize;
}

unsigned long int MemoryManagement::access(long virtualAddr, AccessMode mode)
{
    long physicalAddr = pageTable.map(virtualAddr, mode);
    if (physicalAddr == -1) {
        stats.numPageMiss += 1;
        #ifdef DEBUG
        std::cerr << "Not in the page table: " << virtualAddr << std::endl;
        #endif
        PTE *page = nullptr;
        if (pageTable.isFull() == true) {
            stats.numPageReplacement += 1;
            #ifdef DEBUG
            std::cerr << "Page table is full. Evicting page..." << std::endl;
            #endif
            std::vector<PTE *> modifiedPages;
            /* Evict the page from the table */           
            page = pageTable.evictPage(modifiedPages);

            /* Write back the modified pages */
            for (PTE *page : modifiedPages) {
                writeDisk(page);
                stats.numDiskPageWrite += 1;
            }

            #ifdef DEBUG
            std::cerr << "Evicted page VFID: " << page->getVirtualFrameID() << " PFID: " << page->getPhysicalFrameID() << std::endl;
            #endif
            
            if (page->getModified()) {
                /* Write back the content of the evicted frame to disk */
                writeDisk(page);
                stats.numDiskPageWrite += 1;
            }
        }

        if (page == nullptr) {
            physicalAddr = memoryPosition(pageTable.getSize()) + getFrameOffset(virtualAddr);
            page = new PTE(getFrameID(virtualAddr), pageTable.getSize());
        }
        else {
            int physicalFrameID = page->getPhysicalFrameID();
            physicalAddr = memoryPosition(physicalFrameID) + getFrameOffset(virtualAddr);
            page->reset(getFrameID(virtualAddr), physicalFrameID);
        }
        /* Update the accessing information for the frame */
        if (mode == AccessMode::ACC_WRITE)
            page->setModified(true);

        /* Load the new page frame to memory */
        readDisk(page); 
        stats.numDiskPageRead += 1;

        /* Update the page table */
        pageTable.addPage(page);
        #ifdef DEBUG
        std::cerr << "add page VFID: " << page->getVirtualFrameID() << " PFID: " << page->getPhysicalFrameID() << " pageList size: " << pageTable.getSize() << std::endl;
        #endif
    }

    /* Print the page table on every 10000 memory accesses */
    if ((stats.numRead + stats.numWrite) % 10000 == 0)
        pageTable.printTable();

    return physicalAddr;
}

void MemoryManagement::printStats()
{
    stats.print();
}


void MemoryManagement::initDisk()
{
    std::fstream diskFile;
    int zero = 0;

    diskFile.open(diskFileName, std::fstream::out | std::fstream::trunc | std::fstream::binary);
    diskFile.seekp(0, std::ios::beg); 
    
    for (unsigned long int i = 0; i < numVirtualFrames; ++i)
        for (unsigned long int j = 0; j < frameSize; ++j)
            diskFile.write((char *) &zero, sizeof(int));
    diskFile.close();
}

void MemoryManagement::readDisk(PTE *entry)
{
    std::fstream diskFile;
    diskFile.open(diskFileName, std::fstream::in | std::fstream::binary);

    int *currFrame = physicalMem + memoryPosition(entry->getPhysicalFrameID());
    diskFile.seekg(diskPosition(entry->getVirtualFrameID()), std::ios::beg);
    for (unsigned long int i = 0; i < frameSize; ++i) {
        diskFile.read((char *) &currFrame[i], sizeof(int));
        #ifdef DEBUG
        std::cerr << "Reading from disk " << currFrame[i] << std::endl;
        #endif
    }
    diskFile.close();
}

void MemoryManagement::writeDisk(PTE *entry)
{
    std::fstream diskFile;
    diskFile.open(diskFileName, std::fstream::out | std::fstream::in | std::fstream::binary);

    #ifdef DEBUG
    std::cerr  << "Writing disk VFID: " << entry->getVirtualFrameID() << " " << "PFID: " << entry->getPhysicalFrameID() << std::endl;
    #endif
    int *currFrame = physicalMem + memoryPosition(entry->getPhysicalFrameID());
    diskFile.seekp(diskPosition(entry->getVirtualFrameID()), std::ios::beg);
    #ifdef DEBUG
    std::cerr << "Disk position: " << diskPosition(entry->getVirtualFrameID()) << std::endl;
    #endif
    for (unsigned long int i = 0; i < frameSize; ++i) {
        //TODO: The array could written as block
        diskFile.write((char *) &currFrame[i], sizeof(int)); 
        #ifdef DEBUG
        std::cerr << "Writing to disk: " << currFrame[i] << std::endl;
        #endif
        // diskFile << currFrame[i];
    }
    diskFile.close();
}

int MemoryManagement::getFrameID(unsigned long int addr)
{
    return addr / frameSize;
}

int MemoryManagement::getFrameOffset(unsigned long int addr)
{
    return addr % frameSize;
}

unsigned long int MemoryManagement::memoryPosition(int physicalFrameID) 
{
    return frameSize * physicalFrameID;
}

unsigned long int MemoryManagement::diskPosition(int virtualFrameID) 
{
    return sizeof(int) * frameSize * virtualFrameID;
}