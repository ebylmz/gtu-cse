#include <iostream>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <thread>
#include <vector>

#include "memory.h"
#include "simulation.h"

int strToInt(const char *str, int &status);

void usageErr(const char *pname);

bool handleArgs(int argc, char *argv[], int &bitsFrameOffset, int &bitsPhysicalFrames, int &bitsVirtualFrames, InvertedPageTable::PageReplacementAlgo &algo);

int main(int argc, char *argv[])
{
    int bitsFrameOffset, bitsVirtualFrames, bitsPhysicalFrames;
    InvertedPageTable::PageReplacementAlgo algo;
    if (handleArgs(argc, argv, bitsFrameOffset, bitsPhysicalFrames, bitsVirtualFrames, algo) == false) {
        usageErr(argv[0]);
        exit(1);
    }

    unsigned int frameSize = pow(2, bitsFrameOffset);
    unsigned int numVirtualFrames = pow(2, bitsVirtualFrames);
    unsigned int numPhysicalFrames = pow(2, bitsPhysicalFrames);
    unsigned int physicalMemSize = numPhysicalFrames * frameSize;
    unsigned int virtualMemSize = numVirtualFrames * frameSize;

    MemoryManagement mem(frameSize, numPhysicalFrames, numVirtualFrames, algo, argv[6]);

    /* Fill the virtual memory with random numbers */
    for (long addr = 0; addr < virtualMemSize; ++addr)
        mem.write(addr, rand() % 100);    

    /* Uncomment the below line to see simple matrix-vector and vector-transpose multiplications */
    // simpleMultiplication(mem);

    // simulate(mem, 0, SIM_MATRIX_ROW, SIM_MATRIX_COL);
    
    unsigned long int simulationSpaceSize = calcSimulationAdressSpace(SIM_MATRIX_ROW, SIM_MATRIX_COL);
    
    // Sequential simılation on the whole virtual memory
/*
    for (unsigned long next = 0; next < virtualMemSize; next += simulationSpaceSize) 
        simulate(mem, next, SIM_MATRIX_ROW, SIM_MATRIX_COL);
*/

    std::vector<std::thread> threads;
    // Parallel simılation on the whole virtual memory
    for (unsigned long next = 0; next < virtualMemSize; next += simulationSpaceSize) {
        threads.emplace_back(simulate, std::ref(mem), next, SIM_MATRIX_ROW, SIM_MATRIX_COL);
        if (threads.size() == 10) {
            for (auto &thread : threads)
                thread.join();
            threads.clear();
        }
    }

    for (auto &thread : threads)
        thread.join();


    std::cout << "#####################################" << std::endl;
    std::cout << "# of physical frames: " << numPhysicalFrames << std::endl;
    std::cout << "# of virtual  frames: " << numVirtualFrames << std::endl;
    std::cout << "Physical memory size: " << physicalMemSize << std::endl;
    std::cout << "Virtual  memory size: " << virtualMemSize << std::endl;
    std::cout << "#####################################\n\n";

    mem.printStats();
}

int strToInt(const char *str, int &status) 
{
    int num;
    char *endptr;

    num = 0;
    errno = 0;
    num = strtol(str, &endptr, 10);
    status = (*endptr != '\0' || errno != 0) ? -1 : 0;
    return num;
}

void usageErr(const char *pname) 
{
    std::cout << pname << " <bitsFrameOffset> <bitsPhysicalFrames> <bitsVirtualFrames> <replacement algo> <page table> <disk fname>\n";
    std:: cout << "<replacement algo>: LRU, SC, WSClock\n";
    std:: cout << "<page table>: inverted\n";
}

bool handleArgs(int argc, char *argv[], int &bitsFrameOffset, int &bitsPhysicalFrames, int &bitsVirtualFrames, InvertedPageTable::PageReplacementAlgo &algo)
{
    int status;

    if (argc != 7)
        return false;
    bitsFrameOffset = strToInt(argv[1], status);
    if (status == -1)
        return false;
    bitsPhysicalFrames = strToInt(argv[2], status);
    if (status == -1)
        return false;
    bitsVirtualFrames = strToInt(argv[3], status);
    if (status == -1)
        return false;

    if (strcmp(argv[4], "LRU") == 0)
        algo = InvertedPageTable::PageReplacementAlgo::LRU;
    else if (strcmp(argv[4], "SC") == 0)
        algo = InvertedPageTable::PageReplacementAlgo::SC;
    else if (strcmp(argv[4], "WSClock") == 0)
        algo = InvertedPageTable::PageReplacementAlgo::WSClock;
    else 
        return false;

    return true;
}