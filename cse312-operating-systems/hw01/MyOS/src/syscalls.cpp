
#include <syscalls.h>
#include <utility.h>
 
using namespace myos;
using namespace myos::common;
using namespace myos::hardwarecommunication;

void printf(char*);

SyscallHandler::SyscallHandler(TaskManager* taskManager, InterruptManager* interruptManager, uint8_t InterruptNumber)
: InterruptHandler(interruptManager, InterruptNumber  + interruptManager->HardwareInterruptOffset())
{
    this->taskManager = taskManager;
}

SyscallHandler::~SyscallHandler()
{
}

void SyscallHandler::sys_exit() {
    asm("int $0x80" : : "a" (1));
}

void SyscallHandler::sys_execve(void (*entrypoint)()) {
    asm("int $0x80" : : "a" (11), "b" (entrypoint));
}

void SyscallHandler::sys_waitpid(int pid) {
    asm("int $0x80" : : "a" (7), "b"(pid));
}

void SyscallHandler::sys_fork() {
    asm("int $0x80" : : "a" (2));
}

void SyscallHandler::sys_write(char *str) {
    asm("int $0x80" : : "a" (4), "b" (str));
}

void SyscallHandler::sys_read(char *out) {
    asm("int $0x80" : : "a" (3), "b" (out));
}

uint32_t SyscallHandler::HandleInterrupt(uint32_t esp)
{
    CPUState *cpu = (CPUState*)esp;

    switch(cpu->eax)
    {
        case 1: //  sys_exit
            taskManager->Exit();
            esp = (uint32_t) taskManager->Schedule(cpu);
            break;
        case 2: //  sys_fork
            taskManager->Fork(cpu);
            break;
        case 3: // sys_read
            scanf((char *)cpu->ebx);
            break;
        case 4: // sys_write
            printf((char*)cpu->ebx);
            break;
        case 7: // sys_waitpid
            taskManager->Waitpid(cpu->ebx);
            break;
        case 11: // sys_execve
            esp = taskManager->Execve((void (*)()) cpu->ebx);
            break;
        default:
            break;
    }
    
    return esp;
}

ProcessExecutionHandler::ProcessExecutionHandler(TaskManager* taskManager, hardwarecommunication::InterruptManager* interruptManager, myos::common::uint8_t InterruptNumber) 
: InterruptHandler(interruptManager, InterruptNumber)
// : InterruptHandler(interruptManager, InterruptNumber  + interruptManager->HardwareInterruptOffset())
{
    this->taskManager = taskManager;
}

ProcessExecutionHandler::~ProcessExecutionHandler() 
{
}
    
myos::common::uint32_t ProcessExecutionHandler::HandleInterrupt(myos::common::uint32_t esp) 
{
    taskManager->Exit();
    //!!!!!!!!
    // taskManager->Schedule(esp);
    return esp;
}