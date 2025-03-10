use std::io;

fn one_char_input(message: String) -> char {
    let mut user_input = String::new();
    println!("{}", message);
    let _ = io::stdin().read_line(&mut user_input);
    return user_input.chars().nth(0).unwrap();
}

struct Game {
    board: [i8; 9],
    turn: i8,
    xs: i8
}

impl Game {
    fn create() -> Game {
        let first_player = one_char_input(String::from("Kto zaczyna? (X/o)"));
        let game = Game {
            board: [0; 9],
            turn: 1,
            xs: if first_player == 'O' || first_player == 'o' {-1} else {1}
        };
        return game;
    }

    fn print_board(&self) {
        for x in 0..3 {
            let mut row = [' '; 3];
            for y in 0..3 {
                if self.board[3*x + y] != 0 {
                    row[y] = if self.xs == self.board[3*x + y] {'X'} else {'O'};
                }
            }
            println!("{:?}", row);
        }
    }

    fn check_for_win(&self, x: usize, y: usize) -> i8 {
        if self.board[x] == self.board[x+3] && self.board[x] == self.board[x+6] {
            return self.board[x];
        }
        if self.board[3*y] == self.board[3*y + 1] && self.board[3*y] == self.board[3*y + 2] {
            return self.board[3*y];
        }
        if self.board[0] == self.board[4] && self.board[0] == self.board[8] {
            return self.board[0];
        }
        if self.board[2] == self.board[4] && self.board[2] == self.board[6] {
            return self.board[2];
        }
        return 0;
    }

    fn play(mut self) {
        self.print_board();
        for _ in 0..9 {
            let mut tile: usize;
            let player_char = if self.turn == self.xs {'X'} else {'O'};
            loop {
                let tile_char = one_char_input(format!("Ruch {}:", player_char));
                if tile_char < '1' || tile_char > '9' {
                    println!("Nieprawidłowe pole");
                    self.print_board();
                    continue;
                }
                tile = tile_char as usize - 49;
                if self.board[tile] == 0 {
                    break;
                }
                println!("Pole zajęte!")
            }
            self.board[tile] = self.turn;
            self.print_board();
            if self.check_for_win(tile % 3, tile / 3) != 0 {
                println!("{} wygrał!", player_char);
                return;
            }
            self.turn *= -1
        }
        println!("Remis");
    }
}

fn main() {
    loop {
        let game = Game::create();
        game.play();
        let cmd = one_char_input(String::from("Jeszcze raz? (N/y)"));
        if cmd != 'Y' && cmd != 'y' {
            break;
        }
    }
}
