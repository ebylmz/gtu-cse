# Download
You need to install the GNU C++ Compiler, VirtualBox, binuilts, libc6-dev-i386 to be able to compile the code. To install these tools you can do:

    sudo apt-get install g++ binutils libc6-dev-i386
    sudo apt-get install VirtualBox grub-legacy xorriso

After installing these tools, you can run the following command to build the kernel:

    make

# Running
After making, the OS image is created as a single "mykernel.bin" binary file.

Open VirtualBox and create a new virtual machine named 'MyOS'

After creating virtual machine run the following command to emulate the OS:

    make

For the first time, select the iso file as mykernel.iso and then the OS will be loaded