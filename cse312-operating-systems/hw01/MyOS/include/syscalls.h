 
#ifndef __MYOS__SYSCALLS_H
#define __MYOS__SYSCALLS_H

#include <common/types.h>
#include <hardwarecommunication/interrupts.h>
#include <multitasking.h>

namespace myos
{
    
    class SyscallHandler : public hardwarecommunication::InterruptHandler
    {
    public:
        static void sys_exit();
        static void sys_waitpid(int pid);
        static void sys_fork();
        static void sys_execve(void (*entrypoint)());
        static void sys_write(char *str);
        static void sys_read(char *out);

        SyscallHandler(TaskManager *taskManager, hardwarecommunication::InterruptManager *interruptManager, myos::common::uint8_t InterruptNumber);
        ~SyscallHandler();
        
        virtual myos::common::uint32_t HandleInterrupt(myos::common::uint32_t esp);
    protected:
        TaskManager *taskManager;
    };
    

    class ProcessExecutionHandler : public hardwarecommunication::InterruptHandler
    {
    public:
        ProcessExecutionHandler(TaskManager *taskManager, hardwarecommunication::InterruptManager *interruptManager, myos::common::uint8_t InterruptNumber);
        ~ProcessExecutionHandler();
        
        virtual myos::common::uint32_t HandleInterrupt(myos::common::uint32_t esp);
    protected:
        TaskManager* taskManager;
    };


}


#endif