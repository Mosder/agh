struct Game {
    board: [i8; 9],
    turn: i8,
    xs: i8
}

impl Game {
    fn new_game_state(xs: i8) -> Game {
        let mut game_state = Game {
            [0; 9],
            1,
            xs
        };
        return game_state;
    }

    fn print_board() {
        for x in 0..3 {
            let mut row = [' '; 3];
            for y in 0..3 {
                if board[3*x + y] != 0 {
                    row[y] = if xs == board[3*x + y] {'X'} else {'O'};
                }
            }
            println!('{:?}', row);
        }
    }

    fn check_for_win(&self, x: i8, y: i8) -> i8 {
        if self.board[x] == self.board[x+3] == self.board[x+6] {
            return self.board[x]
        }
        if self.board[3*y] == self.board[3*y + 1] == self.board[3*y + 2] {
            return self.board[3*y]
        }
        if self.
    }

    fn play(&self) {

    }
}
