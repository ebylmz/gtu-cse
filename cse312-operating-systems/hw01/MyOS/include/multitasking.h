 
#ifndef __MYOS__MULTITASKING_H
#define __MYOS__MULTITASKING_H

#include <common/types.h>
#include <gdt.h>

namespace myos
{
    
    struct CPUState
    {
        common::uint32_t eax;
        common::uint32_t ebx;
        common::uint32_t ecx;
        common::uint32_t edx;

        common::uint32_t esi;
        common::uint32_t edi;
        common::uint32_t ebp;

        /*
        common::uint32_t gs;
        common::uint32_t fs;
        common::uint32_t es;
        common::uint32_t ds;
        */
        common::uint32_t error;

        common::uint32_t eip;
        common::uint32_t cs;
        common::uint32_t eflags;
        common::uint32_t esp;
        common::uint32_t ss;        
    } __attribute__((packed));
    
    typedef enum { Ready, Running, Blocked, Terminated } TaskState;
    
    typedef enum { Low, Normal, High } TaskPriority;

    class Task
    {
    friend class TaskManager;
    private:
        static const int STACK_SIZE = 4096; // 4 KiB 
        common::uint8_t stack[STACK_SIZE]; 
        TaskState state;
        common::uint32_t pid;
        common::uint32_t ppid;
        TaskPriority priority;
        CPUState *cpustate;
        bool waitparent;
    public:
        Task(GlobalDescriptorTable *gdt, void entrypoint(), common::uint32_t ppid, TaskPriority priority);
        Task();
        ~Task();
        void InitTask(GlobalDescriptorTable *gdt, void entrypoint(), common::uint32_t pid, 
            common::uint32_t ppid, TaskPriority priority);
    };
    
    class TaskManager
    {
    private:
        Task tasks[256];
        int numTasks;
        int numActiveTasks;
        int currentTask;
        int nextPid;
        GlobalDescriptorTable *gdt;
        void PrintCPUState(Task *task);
        void PrintCPUState(CPUState *cpustate);    
        int FindTask(common::uint32_t pid);
        void CopyTask(Task *src, Task *dest);
        void PrintTask(int tableIndex);
        void PrintTaskTable();
        void DispatchTask();
    public:
        TaskManager(GlobalDescriptorTable *gdt);
        ~TaskManager();
        bool AddTask(Task *task);
        common::uint32_t Fork(CPUState *cpustate);
        common::uint32_t Execve(void (*entrypoint)());
        void Waitpid(common::uint32_t pid);
        void Exit();
        CPUState* Schedule(CPUState *cpustate);
    };
}


#endif