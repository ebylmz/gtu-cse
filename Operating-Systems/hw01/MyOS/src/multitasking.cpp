
#include <multitasking.h>
#include <utility.h>

using namespace myos;
using namespace myos::common;

Task::Task() {
    InitTask(0, 0, -1, -1, TaskPriority::Normal);
}

Task::Task(GlobalDescriptorTable *gdt, void entrypoint(), common::uint32_t ppid, TaskPriority priority)
{
    InitTask(gdt, entrypoint, pid, ppid, priority);
}

Task::~Task()
{
}

void Task::InitTask(GlobalDescriptorTable *gdt, void entrypoint(), common::uint32_t pid, common::uint32_t ppid, TaskPriority priority) {
    this->ppid = ppid;
    this->pid = pid;
    this->priority = priority;
    state = Ready;
    waitparent = false;

    cpustate = (CPUState*)(stack + STACK_SIZE - sizeof(CPUState));

    cpustate -> eax = 0;
    cpustate -> ebx = 0;
    cpustate -> ecx = 0;
    cpustate -> edx = 0;

    cpustate -> esi = 0;
    cpustate -> edi = 0;
    cpustate -> ebp = 0;
    
    cpustate -> eip = (uint32_t)entrypoint;
    cpustate -> cs = gdt->CodeSegmentSelector();
    cpustate -> eflags = 0x202;
}

TaskManager::TaskManager(GlobalDescriptorTable *gdt)
{
    this->gdt = gdt;
    numTasks = 0;
    numActiveTasks = 0;
    currentTask = -1;
    nextPid = 1;
}

TaskManager::~TaskManager()
{
}

void TaskManager::CopyTask(Task *src, Task *dest) {
    dest->state = src->state;
    dest->pid = src->pid;
    dest->ppid = src->ppid;
    dest->priority = src->priority;
    dest->waitparent = src->waitparent;

    for (int i = 0; i < Task::STACK_SIZE; ++i)
        dest->stack[i] = src->stack[i];

    // copy cpustate and stack
    *(dest->cpustate) = *(src->cpustate);
}

bool TaskManager::AddTask(Task* task)
{
    if(numTasks >= 256)
        return false;
    task->pid = nextPid;
    CopyTask(task, &tasks[numTasks]); 
    //TODO: increase nextPid, numTask and numActiveTask with a single function 
    ++nextPid, ++numTasks, ++numActiveTasks;
    return true;
}

void TaskManager::Waitpid(common::uint32_t pid) {
    printf("### syscall waitpid ###\n");
    Task *parent = &tasks[currentTask];

    // if the pid is -1, then waits any child to finish, otherwise waits the specific child
    if (pid > 0) {
        int childIndex = FindTask(pid);
        // make sure there is such a child process
        if (childIndex != -1 && tasks[childIndex].ppid == parent->pid) {
            // set parent status to blocked and 
            parent->state = Blocked;
            // when the child is terminated, parent will wakes up
            tasks[childIndex].waitparent = true;
        }
    }
    else {
        // set true to the waitparent for all the child
        for (int i = 0; i < numTasks; ++i) {
            if (tasks[i].ppid == parent->pid) {
                parent->state = Blocked;
                tasks[i].waitparent = true;
            }
        }
    }
}

void TaskManager::Exit() {
    printf("### syscall exit ###\n");
    
    Task *current = &tasks[currentTask];

    /* security guard for init process. it cannot be destroyed */
    if (current->pid == 1)
        return;

    // if it's parent waits its child to terminate
    if (current->waitparent) {
        int parentIndex = FindTask(current->ppid);
        if (parentIndex >= 0)
            tasks[parentIndex].state = Ready;
    }

    --numActiveTasks;
    current->state = TaskState::Terminated;
}

common::uint32_t TaskManager::Execve(void (*entrypoint)()) {
    printf("### syscall execve ###\n");
    Task *curr = &tasks[currentTask]; 
    curr->InitTask(gdt, entrypoint, curr->pid, curr->ppid, curr->priority);
    return (common::uint32_t) curr->cpustate;
}

common::uint32_t TaskManager::Fork(CPUState *cpustate) {
    printf("### syscall fork ###\n");

    if (numTasks >= 256)
        return -1;
    
    Task *parent = &tasks[currentTask];
    Task *child = &tasks[numTasks];
    
    child->InitTask(gdt, (void(*)()) parent->cpustate->eip, nextPid, parent->pid, parent->priority);

    // copy the current cpustate to the child
    *(child->cpustate) = *cpustate; 
    
    ++nextPid, ++numTasks, ++numActiveTasks;
    return child->pid;
}

int TaskManager::FindTask(common::uint32_t pid) 
{
    for (int i = 0; i < numTasks; ++i)
        if (tasks[i].pid == pid)
            return i;
    return -1;
}

void TaskManager::DispatchTask() 
{
    // if the task is not blocked, set its state to ready
    if (tasks[currentTask].state == Running)
        tasks[currentTask].state = Ready;

    do { 
        // apply Round Robin scheduling
        if (++currentTask >= numTasks)
            currentTask %= numTasks;
    } while (tasks[currentTask].state != Ready);
    
    tasks[currentTask].state = Running;
}

CPUState* TaskManager::Schedule(CPUState* cpustate)
{
    if (numTasks <= 0)
        return cpustate;
    
    if (currentTask >= 0)
        tasks[currentTask].cpustate = cpustate;

    DispatchTask(); 

/*
    if (numActiveTasks > 1)
        PrintTaskTable();
*/
    
    return tasks[currentTask].cpustate;
}


void TaskManager::PrintTask(int tableIndex) 
{
    Task *task = &tasks[tableIndex];

    printDigit(task->pid);  printf("     "); 
    printDigit(task->ppid); printf("      ");

    switch (task->priority) {
        case Low:       printf("Low    "); break;
        case Normal:    printf("Normal "); break;
        case High:      printf("High   "); break;
        default:        printf("Unknown"); break;
    }
    printf("    ");

    switch (task->state) {
        case Ready:         printf("Ready     "); break;
        case Running:       printf("Running   "); break;
        case Blocked:       printf("Blocked   "); break;
        case Terminated:    printf("Terminated"); break;
        default:            printf("Unknown   "); break;
    }
    printf("\n");
}

void printTableBorder(char * c) 
{
    const int COL_LENGTH = 80;
    for (int i = 0; i < COL_LENGTH; ++i) 
        printf(c);
    // when the coloumn lenght is exceeded, the cursor is set 
    // implicitly to the next line, so no need to add newline charachter
}

void TaskManager::PrintTaskTable() 
{
    printf("\n");
    printTableBorder("*");
    printf("PID   PPID   Priority   State\n");
    for (int i = 0; i < numTasks; ++i)
        PrintTask(i);
    printTableBorder("*");
}

void TaskManager::PrintCPUState(Task * task) 
{
    printf("PID: ");
    printDigit((uint32_t) task->pid);
    printf(", ");
    PrintCPUState(task->cpustate);
}

void TaskManager::PrintCPUState(CPUState * cpustate) 
{
    printf("eax: ");
    printDigit((uint32_t) cpustate->eax);
    printf(", ebx: ");
    printDigit((uint32_t) cpustate->ebx);
    printf(", ecx: ");
    printDigit((uint32_t) cpustate->ecx);
    printf(", edx: ");
    printDigit((uint32_t) cpustate->edx);
    printf(", esi: ");
    printDigit((uint32_t) cpustate->esi);
    printf(", edi: ");
    printDigit((uint32_t) cpustate->edi);
    printf(", ebp: ");
    printDigit((uint32_t) cpustate->ebp);
    printf(", error: ");
    printDigit((uint32_t) cpustate->error);
    printf(", eip: ");
    printDigit((uint32_t) cpustate->eip);
    printf(", cs: ");
    printDigit((uint32_t) cpustate->cs);
    printf(", eflags: ");
    printDigit((uint32_t) cpustate->eflags);
    printf(", esp: ");
    printDigit((uint32_t) cpustate->esp);
    printf(", ss: ");
    printDigit((uint32_t) cpustate->ss);
    printf("\n");
}