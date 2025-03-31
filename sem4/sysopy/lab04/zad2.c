#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int global = 0;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Wrong amount of arguments\n");
        return 1;
    }
    // Print out program name
    printf("Program name: %s\n", argv[0]);

    int local = 0;

    pid_t pid = fork();
    // Error handling for child creation
    if (pid == -1) {
        printf("ERROR: Child process could not be created");
        return 2;
    }
    // Child process
    else if (pid == 0) {
        printf("child process\n");
        global++;
        local++;
        printf("child pid = %d, parent pid = %d\n", (int) getpid(), (int) getppid());
        printf("child's local = %d, child's global = %d\n", local, global);
        return execl("/bin/ls", "ls", argv[1], NULL);
    }
    // Parent process
    else {
        printf("parent process\n");
        printf("parent pid = %d, child pid = %d\n", (int) getpid(), (int) pid);
        int child_exit_code;
        // Wait for child process to finish and save the exit code to a variable
        wait(&child_exit_code);
        // Print out converted exit status
        printf("Child exit code: %d\n", WEXITSTATUS(child_exit_code));
        printf("Parent's local = %d, parent's global = %d\n", local, global);
        return WEXITSTATUS(child_exit_code);
    }
    return 0;
}