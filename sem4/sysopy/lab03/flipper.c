#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>
#define PATH_LIMIT 100
#define LINE_LIMIT 1000

DIR* read_path_and_open_dir(char *message, char *dir_path) {
    DIR *dir = NULL;
    while (!dir) {
        printf("%s\n", message);
        scanf("%s", dir_path);
        dir = opendir(dir_path);
    }
    return dir;
}

int is_txt_file(struct dirent *file) {
    return strlen(file->d_name) >= 4 && strcmp(&file->d_name[strlen(file->d_name) - 4], ".txt") == 0;
}

void create_full_file_path(char *full_file_path, char *dir, char *file_name) {
    strcpy(full_file_path, dir);
    strcat(full_file_path, "/");
    strcat(full_file_path, file_name);
}

void reverse_file_lines(char *input_path, char *output_path) {
    FILE *input_file = fopen(input_path, "r");
    FILE *output_file = fopen(output_path, "w");
    char line[LINE_LIMIT];
    while (fgets(line, sizeof(line), input_file)) {
        int has_end_line = 0;
        if (strcmp(&line[strlen(line) - 1], "\n") == 0) has_end_line = 1;

        for (int i = strlen(line) - 1 - has_end_line; i >= 0; i--) {
            fwrite(&line[i], sizeof(char), 1, output_file);
        }
        if (has_end_line) fwrite("\n", sizeof(char), 1, output_file);
    }
    fclose(input_file);
    fclose(output_file);
}

int main(void) {
    char input_path[PATH_LIMIT], output_path[PATH_LIMIT];
    DIR *input_dir = read_path_and_open_dir("Podaj ścieżkę do katalogu wejściowego:", input_path);

    printf("Podaj ścieżkę do katalogu wyjściowego:\n");
    scanf("%s", output_path);
    mkdir(output_path, S_IRWXU | S_IRGRP | S_IROTH);
    DIR *output_dir = opendir(output_path);

    for (struct dirent *file = readdir(input_dir); file != NULL; file = readdir(input_dir)) {
        if (!is_txt_file(file)) continue;

        char input_file_path[PATH_LIMIT], output_file_path[PATH_LIMIT];
        create_full_file_path(input_file_path, input_path, file->d_name);
        create_full_file_path(output_file_path, output_path, file->d_name);

        reverse_file_lines(input_file_path, output_file_path);
    }

    closedir(input_dir);
    closedir(output_dir);
    return 0;
}