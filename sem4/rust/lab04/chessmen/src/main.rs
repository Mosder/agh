use PieceType::*;
use Color::*;

#[derive(Debug)]
enum Color {
    White, Black
}

#[derive(Debug)]
struct Position {
    x: u8,
    y: u8
}

struct Vector2D {
    x: i8,
    y: i8
}

impl Position {
    const LOWER_BOUND: u8 = 0;
    const UPPER_BOUND: u8 = 7;

    fn get_vector(&self, other: &Position) -> Vector2D {
        Vector2D {x: other.x as i8 - self.x as i8, y: other.y as i8 - self.y as i8}
    }
    
    fn in_bounds(&self) -> bool {
        self.x >= Self::LOWER_BOUND && self.x <= Self::UPPER_BOUND &&
        self.y >= Self::LOWER_BOUND && self.y <= Self::UPPER_BOUND
    }
}

#[derive(Debug)]
enum PieceType {
    Pawn, Knight, Bishop, Rook, Queen, King
}

#[derive(Debug)]
struct ChessPiece {
    r#type: PieceType,
    position: Position,
    color: Color
}

impl ChessPiece {
    fn r#move(&mut self, destination: Position) -> bool {
        if !destination.in_bounds() { return false };
        let vector = self.position.get_vector(&destination);
        let allowed = match self.r#type {
            Pawn => vector.x == 0 && match self.color {
                White => vector.y == 1,
                Black => vector.x == -1
            },
            Knight => i8::abs(vector.x * vector.y) == 2,
            Bishop => i8::abs(vector.x) == i8::abs(vector.y),
            Rook => vector.x * vector.y == 0,
            Queen => i8::abs(vector.x) == i8::abs(vector.y) || vector.x * vector.y == 0,
            King => i8::abs(vector.x) <= 1 && i8::abs(vector.y) <= 1
        };
        if allowed {
            self.position = destination;
        }
        allowed
    }
}

fn main() {
    println!("fortnite battlepass");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pawn_move() {
        let mut pawn = ChessPiece {
            r#type: Pawn,
            position: Position { x: 1, y: 2 },
            color: White
        };
        assert_eq!(pawn.r#move(Position { x: 1, y: 1 }), false);
        assert_eq!(pawn.r#move(Position { x: 1, y: 3 }), true);
        assert_eq!(pawn.r#move(Position { x: 1, y: 5 }), false);
    }
    
    #[test]
    fn test_knight_move() {
        let mut knight = ChessPiece {
            r#type: Knight,
            position: Position { x: 1, y: 2 },
            color: White
        };
        assert_eq!(knight.r#move(Position { x: 3, y: 3 }), true);
        assert_eq!(knight.r#move(Position { x: 5, y: 3 }), false);
        assert_eq!(knight.r#move(Position { x: 2, y: 1 }), true);
    }

    #[test]
    fn test_bishop_move() {
        let mut bishop = ChessPiece {
            r#type: Bishop,
            position: Position { x: 1, y: 2 },
            color: White
        };
        assert_eq!(bishop.r#move(Position { x: 3, y: 0 }), true);
        assert_eq!(bishop.r#move(Position { x: 2, y: 0 }), false);
        assert_eq!(bishop.r#move(Position { x: 3, y: 1 }), false);
    }
    
    #[test]
    fn test_rook_move() {
        let mut rook = ChessPiece {
            r#type: Rook,
            position: Position { x: 1, y: 2 },
            color: White
        };
        assert_eq!(rook.r#move(Position { x: 3, y: 2 }), true);
        assert_eq!(rook.r#move(Position { x: 3, y: 8 }), false);
        assert_eq!(rook.r#move(Position { x: 3, y: 7 }), true);
    }
    
    #[test]
    fn test_queen_move() {
        let mut queen = ChessPiece {
            r#type: Queen,
            position: Position { x: 1, y: 2 },
            color: White
        };
        assert_eq!(queen.r#move(Position { x: 3, y: 4 }), true);
        assert_eq!(queen.r#move(Position { x: 7, y: 4 }), true);
        assert_eq!(queen.r#move(Position { x: 5, y: 3 }), false);
    }
    
    #[test]
    fn test_king_move() {
        let mut king = ChessPiece {
            r#type: King,
            position: Position { x: 1, y: 2 },
            color: Black
        };
        assert_eq!(king.r#move(Position { x: 2, y: 3 }), true);
        assert_eq!(king.r#move(Position { x: 2, y: 2 }), true);
        assert_eq!(king.r#move(Position { x: 4, y: 3 }), false);
    }
    
}
