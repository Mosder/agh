#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

#define NUM_OF_RANKS 13
#define NUM_OF_SUITS 4
#define NUM_OF_CARDS (NUM_OF_RANKS * NUM_OF_SUITS)

typedef struct deck Deck;
typedef struct gameState GameState;

struct deck {
	int cards[NUM_OF_CARDS];
	int top;
	int len;
};

struct gameState {
	int gameType; // 0 - standard, 1 - simple
	int	doneConflicts; // -1 means a conflict wasn't resolved, because A or B run out of cards
	int maxConflicts;
	Deck deckA;
	Deck deckB;
};

void startGame(GameState *state);
void deal(GameState *state);
int getRandInt(int start, int end);
void handleConflict(GameState *state);
void handleWar(GameState *state);
void topToBottom(GameState *state);
void steal(GameState *state, bool winner, int amount);
bool isOver(GameState state);
// void debugPrintState(GameState state);

int main(void) {
	GameState state;
	int seed;
	scanf("%d %d %d", &seed, &state.gameType, &state.maxConflicts);
	srand(seed);
	startGame(&state);
	return 0;
}

void startGame(GameState *state) {
	state->doneConflicts = 0;
	deal(state);
	// debugPrintState(*state);
	do {
		handleConflict(state);
	} while(!isOver(*state));
}

void deal(GameState *state) {
	for (int i = 0; i < NUM_OF_CARDS; i++) state->deckA.cards[i] = i;
	for (int i = 0; i < NUM_OF_CARDS; i++) {
		int k = getRandInt(i, NUM_OF_CARDS - 1);
		int temp = state->deckA.cards[i];
		state->deckA.cards[i] = state->deckA.cards[k];
		state->deckA.cards[k] = temp;
	}
	for (int i = 0; i < NUM_OF_CARDS / 2; i++) state->deckB.cards[i] = state->deckA.cards[i + NUM_OF_CARDS/2];
	state->deckA.len = NUM_OF_CARDS / 2;
	state->deckB.len = NUM_OF_CARDS / 2;
	state->deckA.top = 0;
	state->deckB.top = 0;
}

int getRandInt(int start, int end) {
	return rand() % (end-start+1) + start;
}

void handleConflict(GameState *state) {
	int cardA = floor(state->deckA.cards[state->deckA.top] / NUM_OF_SUITS);
	int cardB = floor(state->deckB.cards[state->deckB.top] / NUM_OF_SUITS);
	state->doneConflicts++;
	if (cardA != cardB) steal(state, cardA > cardB ? true : false, 1);
	else if (state->gameType == 0) handleWar(state);
	else topToBottom(state);
}

void handleWar(GameState *state) {
	for (int conflicts = 1; state->deckA.len > 2*conflicts && state->deckB.len > 2*conflicts; conflicts++) {
		state->doneConflicts++;
		int cardA = floor(state->deckA.cards[(state->deckA.top + 2*conflicts) % NUM_OF_CARDS] / NUM_OF_SUITS);
		int cardB = floor(state->deckB.cards[(state->deckB.top + 2*conflicts) % NUM_OF_CARDS] / NUM_OF_SUITS);
		if (cardA != cardB) {
			steal(state, cardA > cardB ? true : false, 2*conflicts + 1);
			return;
		}
		else if (state->doneConflicts == state->maxConflicts) return;
	}
	state->doneConflicts = -1;
}

void topToBottom(GameState *state) {
	state->deckA.cards[(state->deckA.top + state->deckA.len) % NUM_OF_CARDS] = state->deckA.cards[state->deckA.top];
	state->deckB.cards[(state->deckB.top + state->deckB.len) % NUM_OF_CARDS] = state->deckB.cards[state->deckB.top];
	state->deckA.top = (state->deckA.top + 1) % NUM_OF_CARDS;
	state->deckB.top = (state->deckB.top + 1) % NUM_OF_CARDS;
}

// winner == true when A won and false when B won the conflict
void steal(GameState *state, bool winner, int amount) {
	Deck *winnerDeck;
	Deck *loserDeck;
	if (winner) {
		winnerDeck = &state->deckA;
		loserDeck = &state->deckB;
	}
	else {
		winnerDeck = &state->deckB;
		loserDeck = &state->deckA;
	}
	for (int i = 0; i < amount; i++) {
		int beforeWinner = (winnerDeck->top + i) % NUM_OF_CARDS;
		int afterWinner = (beforeWinner + winnerDeck->len) % NUM_OF_CARDS;
		int beforeLoser = (loserDeck->top + i) % NUM_OF_CARDS;
		int afterLoser = (afterWinner + amount) % NUM_OF_CARDS;
		winnerDeck->cards[afterWinner] = winnerDeck->cards[beforeWinner];
		winnerDeck->cards[afterLoser] = loserDeck->cards[beforeLoser];
	}
	winnerDeck->len += amount;
	loserDeck->len -= amount;
	winnerDeck->top = (winnerDeck->top + amount) % NUM_OF_CARDS;
	loserDeck->top = (loserDeck->top + amount) % NUM_OF_CARDS;
}

bool isOver(GameState state) {
	if (state.deckA.len == 0) {
		printf("3\n");
		for (int i = 0; i < NUM_OF_CARDS; i++) {
			int ind = (state.deckB.top + i) % NUM_OF_CARDS;
			printf("%d ", state.deckB.cards[ind]);
		}
	}
	else if (state.deckB.len == 0) printf("2 %d", state.doneConflicts);
	else if (state.doneConflicts == -1) printf("1 %d %d", state.deckA.len, state.deckB.len);
	else if (state.doneConflicts == state.maxConflicts) printf("0 %d %d", state.deckA.len, state.deckB.len);
	else return false;
	printf("\n");
	// debugPrintState(*state);
	return true;
}

// void debugPrintState(GameState state) {
// 	printf("lenA: %d; lenB: %d; topA: %d; topB: %d; conflicts: %d\n",
// 		state.deckA.len, state.deckB.len, state.deckA.top, state.deckB.top, state.doneConflicts);
// 	printf("A: ");
// 	for (int i = 0; i < state.deckA.len; i++) printf("%d ", state.deckA.cards[(state.deckA.top +i) % NUM_OF_CARDS]);
// 	printf("\nB: ");
// 	for (int i = 0; i < state.deckB.len; i++) printf("%d ", state.deckB.cards[(state.deckA.top +i) % NUM_OF_CARDS]);
// 	printf("\n");
// }
