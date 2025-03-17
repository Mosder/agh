#include <stdio.h>
#include <dirent.h>
#define PATH_LIMIT 100

char* read_path_and_open_dir(char *message, DIR *dir) {
    char path[PATH_LIMIT];
    printf("%s\n", message);
    scanf("%s", path);
    DIR *dir = opendir(path);
    return dir;
}

int main(void) {
    char input_path[PATH_LIMIT], output_path[PATH_LIMIT];
    DIR *input_dir;
    while (!input_dir) {
        input_path = read_path_and_open_dir("Podaj ścieżkę do katalogu wejściowego:", input_dir);
    }
    printf("Podaj ścieżkę do katalogu wyjściowego:");
    scanf("%s", output_path);

    for (struct dirent *file = readdir(input_dir); file != NULL; file = readdir(input_dir)) {
        int f = open("file->d_name")
        printf("%s\n", file->d_name);
    }

    return 0;
}