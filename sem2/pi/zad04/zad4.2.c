#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define MAX_ID_LEN 64
#define MAX_IDS 1024
#define NUM_OF_KEYWORDS 32

#define IN_CHAR 1
#define IN_STRING 2
#define IN_LINE_COMMENT 3
#define IN_BLOCK_COMMENT 4
#define IN_BLOCK_COMMENT_LONGER 5
#define AFTER_BLOCK_COMMENT_END 6
#define AFTER_BLOCK_COMMENT_END_LONGER 7

#define ASCII_0 (int)'0'
#define ASCII_9 (int)'9'
#define ASCII_UPPERCASE_A (int)'A'
#define ASCII_UPPERCASE_Z (int)'Z'
#define ASCII_LOWERCASE_A (int)'a'
#define ASCII_LOWERCASE_Z (int)'z'

#define TEST 0  // 1 - dla testowania,  0 - dla automatycznej oceny

char tab[MAX_IDS][MAX_ID_LEN];
// char debugMessage[10000];

char *keywords[] = {
	"auto", "break", "case", "char",
	"const", "continue", "default", "do",
	"double", "else", "enum", "extern",
	"float", "for", "goto", "if",
	"int", "long", "register", "return",
	"short", "signed", "sizeof", "static",
	"struct", "switch", "typedef", "union",
	"unsigned", "void", "volatile", "while"
};

int isLetter(char c) {
	if (c >= ASCII_UPPERCASE_A && c <= ASCII_UPPERCASE_Z) return 1;
	if (c >= ASCII_LOWERCASE_A && c <= ASCII_LOWERCASE_Z) return 1;
	if (c == '_') return 1;
	return 0;
}

int isDigit(char c) {
	if (c >= ASCII_0 && c <= ASCII_9) return 1;
	return 0;
}

int isKeyword(char word[]) {
	for (int i = 0; i < NUM_OF_KEYWORDS; i++)
		if (!strcmp(keywords[i], word)) return 1;
	return 0;
}

int isUnique(char word[], int n) {
	for (int i = 0; i < n; i++)
		if (!strcmp(tab[i], word)) return 0;
	return 1;
}

void handleInsideStringOrComment(int *flag, int c1, int c2) {
	if (*flag == IN_CHAR && c2 == '\'') *flag = 0;
	else if (*flag == IN_STRING && c2 == '"') *flag = 0;
	else if (*flag == IN_LINE_COMMENT && (c2 == '\n' || c2 == '\r')) *flag = 0;
	else if (*flag == IN_BLOCK_COMMENT_LONGER && c1 == '*' && c2 == '/') *flag = AFTER_BLOCK_COMMENT_END;
	else if (*flag == IN_BLOCK_COMMENT) *flag = IN_BLOCK_COMMENT_LONGER;
}

void checkForStartOfStringOrComment(int *flag, int c1, int c2) {
	if (c2 == '\'') *flag = IN_CHAR;
	else if (c2 == '"') *flag = IN_STRING;
	else if (c1 == '/' && *flag != AFTER_BLOCK_COMMENT_END && *flag != AFTER_BLOCK_COMMENT_END_LONGER) {
		if (c2 == '/') *flag = IN_LINE_COMMENT;
		else if (c2 == '*') *flag = IN_BLOCK_COMMENT;
	}
}

int find_idents(FILE *stream) {
	int flag = 0;
	int idLen = 0;
	int idsAmount = 0;
	char currentId[MAX_ID_LEN] = {0};

	int prev = getc(stream);
	if (prev == '\'') flag = IN_CHAR;
	else if (prev == '"') flag = IN_STRING;
	else if (isLetter(prev) || isDigit(prev)) currentId[idLen++] = prev;

	// if (prev == '\n') {
	// 	debugMessage[0] = '\\';
	// 	debugMessage[1] = 'n';
	// 	debugMessage[2] = '\n';
	// }
	// else if (prev == '\t') {
	// 	debugMessage[0] = '\\';
	// 	debugMessage[1] = 't';
	// 	debugMessage[2] = '\n';
	// }
	// else if (prev == '\r') {
	// 	debugMessage[0] = '\\';
	// 	debugMessage[1] = 'r';
	// 	debugMessage[2] = '\n';
	// }
	// else {
	// 	debugMessage[0] = prev;
	// 	debugMessage[1] = '\n';
	// }

	for (int nextChar = getc(stream); nextChar != EOF; nextChar = getc(stream)) {
		if (flag == AFTER_BLOCK_COMMENT_END) flag = AFTER_BLOCK_COMMENT_END_LONGER;
		else if (flag == AFTER_BLOCK_COMMENT_END_LONGER) flag = 0;
		if (flag && flag != AFTER_BLOCK_COMMENT_END && flag != AFTER_BLOCK_COMMENT_END_LONGER)
			handleInsideStringOrComment(&flag, prev, nextChar);
		else if (isLetter(nextChar) || isDigit(nextChar))
			currentId[idLen++] = nextChar;
		else {
			if (idLen) {
				if (!isKeyword(currentId) && isUnique(currentId, idsAmount) && isLetter(currentId[0])) {
					for (int i = 0; i < idLen; i++) tab[idsAmount][i] = currentId[i];
					idsAmount++;
				}
				for (int i = 0; i < idLen; i++) currentId[i] = 0;
				idLen = 0;
			}
			checkForStartOfStringOrComment(&flag, prev, nextChar);
		}
		prev = nextChar;

		// char dbg[] = "x  x x x\n";
		// if (nextChar == '\n') {
		// 	dbg[0] = '\\';
		// 	dbg[1] = 'n';
		// }
		// else if (nextChar == '\t') {
		// 	dbg[0] = '\\';
		// 	dbg[1] = 't';
		// }
		// else if (nextChar == '\r') {
		// 	dbg[0] = '\\';
		// 	dbg[1] = 'r';
		// }
		// else dbg[0] = nextChar;
		// dbg[3] = flag + ASCII_0;
		// dbg[5] = idLen + ASCII_0;
		// dbg[7] = idsAmount + ASCII_0;
		// strcat(debugMessage, dbg);
	}

	// for (int i = 0; i < idsAmount; i++) {
	// 	strcat(debugMessage, tab[i]);
	// 	strcat(debugMessage, "\n");
	// }
	return idsAmount;
}

int main(void) {
	char file_name[40];
	FILE *stream;

	if(TEST) stream = stdin;
	else {
		scanf("%s",file_name);
		stream = fopen(file_name,"r");
		if(stream == NULL) {
			printf("fopen failed\n");
			return -1;
		}
	}
	printf("%d\n", find_idents(stream));
	// printf("\n%s\n", debugMessage);
	return 0;
}
