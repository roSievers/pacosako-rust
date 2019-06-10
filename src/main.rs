
use colored::*;

use std::fmt;

use std::fmt::Debug;
use std::fmt::Display;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PieceType {
    Pawn,
    Rock,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PlayerColor {
    White,
    Black,
}

impl PlayerColor {
    fn other(self) -> Self {
        use PlayerColor::*;
        match self {
            White => Black,
            Black => White,
        }
    }
    fn paint_string(self, input: &str) -> colored::ColoredString {
        use PlayerColor::*;
        match self {
            White => input.red(),
            Black => input.blue(),
        }
    }
}

impl PieceType {
    fn to_char(self) -> &'static str {
        use PieceType::*;

        match self {
            Pawn => "P",
            Rock => "R",
            Knight => "N",
            Bishop => "B",
            Queen => "Q",
            King => "K",
        }
    }
}

/// In a DenseBoard we reserve memory for all positions.
#[derive(Clone, Debug)]
struct DenseBoard {
    white: Vec<Option<PieceType>>,
    black: Vec<Option<PieceType>>,
    dance: Vec<bool>,
    current_player: PlayerColor,
    // The lifted piece has no player color as it must belong to the current player.
    lifted_piece: Option<(BoardPosition, PieceType)>,
}

/// Represents zero to two lifted pieces
/// The owner of the pieces must be tracked externally, usually this will be the current player.
enum Hand {
    Empty,
    Single {
        piece: PieceType,
        position: BoardPosition,
    },
    Pair {
        piece: PieceType,
        partner: PieceType,
        position: BoardPosition,
    },
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct BoardPosition(u8);

impl BoardPosition {
    fn x(self) -> u8 {
        self.0 % 8
    }
    fn y(self) -> u8 {
        self.0 / 8
    }
    fn new(x: u8, y: u8) -> Self {
        Self(x + 8 * y)
    }
    fn new_checked(x: i8, y: i8) -> Option<Self> {
        if x >= 0 && y >= 0 && x < 7 && y < 8 {
            Some(Self::new(x as u8, y as u8))
        } else {
            None
        }
    }
    fn add(self, other: (i8, i8)) -> Option<Self> {
        Self::new_checked(self.x() as i8 + other.0, self.y() as i8 + other.1)
    }
}

/// The debug output for a position is a string like d4 that is easily human readable.
impl Debug for BoardPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            ["a", "b", "c", "d", "e", "f", "g", "h"][self.x() as usize],
            self.y() + 1
        )
    }
}

/// The display output for a position is a string like d4 that is easily human readable.
/// The Display implementation just wraps the Debug implementation.
impl Display for BoardPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A PacoAction is an action that can be applied to a PacoBoard to modify it.
/// An action is an atomar part of a move, like picking up a piece or placing it down.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum PacoAction {
    /// Lifting a piece starts a move.
    Lift(BoardPosition),
    /// Placing the piece picked up earlier either ends a move or continues it in case of a chain.
    Place(BoardPosition),
}

/// The PacoBoard trait encapsulates arbitrary Board implementations.
trait PacoBoard {
    /// Check if a PacoAction is legal and execute it. Otherwise return an error.
    fn execute(&mut self, action: PacoAction) -> Result<&mut Self, ()>;
    /// List all actions that can be executed in the current state. Note that actions which leave
    /// the board in a deadend state (like lifting up a pawn that is blocked) should be included
    /// in the list as well.
    fn actions(&self) -> Vec<PacoAction>;
}

impl DenseBoard {
    fn new() -> Self {
        use PieceType::*;
        let mut result: Self = DenseBoard {
            white: Vec::with_capacity(64),
            black: Vec::with_capacity(64),
            dance: vec![false; 64],
            current_player: PlayerColor::White,
            lifted_piece: None,
        };

        // Board structure
        let back_row = vec![Rock, Knight, Bishop, Queen, King, Bishop, Knight, Rock];
        let front_row = vec![Pawn; 8];

        result
            .white
            .extend(back_row.iter().map(|&a| Option::Some(a)));
        result
            .white
            .extend(front_row.iter().map(|&a| Option::Some(a)));
        result.white.append(&mut vec![None; 64 - 16]);

        assert!(
            result.white.len() == 64,
            "Amount of white pieces is incorrect."
        );

        result.black.append(&mut vec![None; 64 - 16]);
        result
            .black
            .extend(front_row.iter().map(|&a| Option::Some(a)));
        result
            .black
            .extend(back_row.iter().map(|&a| Option::Some(a)));

        assert!(
            result.black.len() == 64,
            "Amount of black pieces is incorrect."
        );

        result
    }

    /// Lifts the piece of the current player in the given position of the board.
    /// Only one piece may be lifted at a time.
    fn lift(&mut self, position: BoardPosition) -> Result<&mut Self, ()> {
        use PlayerColor::*;
        if self.lifted_piece != None {
            return Err(());
        }
        let piece = match self.current_player {
            White => self.white.get_mut(position.0 as usize),
            Black => self.black.get_mut(position.0 as usize),
        }
        .unwrap();

        if let Some(piece_type) = piece {
            self.lifted_piece = Some((position, *piece_type));
            *piece = None;
            Ok(self)
        } else {
            Err(())
        }
    }

    /// Places the piece that is currently lifted back on the board.
    /// Returns an error if no piece is currently being lifted.
    fn place(&mut self, position: BoardPosition) -> Result<&mut Self, ()> {
        use PlayerColor::*;
        if let Some(lifted) = self.lifted_piece {
            let piece = match self.current_player {
                White => self.white.get_mut(position.0 as usize),
                Black => self.black.get_mut(position.0 as usize),
            }
            .unwrap();
            if piece.is_some() {
                Err(())
            } else {
                *piece = Some(lifted.1);
                self.lifted_piece = None;
                self.other_player();
                Ok(self)
            }
        } else {
            Err(())
        }
    }

    fn other_player(&mut self) -> &mut Self {
        self.current_player = self.current_player.other();
        self
    }

    /// The Dense Board representaition containing only pieces of the current player.
    fn active_pieces(&self) -> &Vec<Option<PieceType>> {
        match self.current_player {
            PlayerColor::White => &self.white,
            PlayerColor::Black => &self.black,
        }
    }

    /// The Dense Board representaition containing only pieces of the opponent player.
    fn opponent_pieces(&self) -> &Vec<Option<PieceType>> {
        match self.current_player {
            PlayerColor::White => &self.black,
            PlayerColor::Black => &self.white,
        }
    }

    /// All positions where the active player has a piece.
    fn active_positions<'a>(&'a self) -> impl Iterator<Item = BoardPosition> + 'a {
        // The filter map takes (usize, Optional<PieceType>) and returns Optional<BoardPosition>
        // where we just place the index in a Some whenever a piece is found.
        self.active_pieces()
            .iter()
            .enumerate()
            .filter_map(|p| p.1.map(|_| BoardPosition(p.0 as u8)))
    }

    /// All place target for a piece of given type at a given position.
    /// This is intended to recieve its own lifted piece as input but does not require it.
    fn place_targets(&self, position: BoardPosition, piece_type: PieceType) -> Vec<BoardPosition> {
        use PieceType::*;
        match piece_type {
            Pawn => self.place_targets_pawn(position),
            Rock => self.place_targets_rock(position),
            Knight => self.place_targets_knight(position),
            Bishop => self.place_targets_bishop(position),
            Queen => self.place_targets_queen(position),
            King => self.place_targets_king(position),
        }
    }

    /// Calculates all possible placement targets for a pawn at the given position.
    fn place_targets_pawn(&self, position: BoardPosition) -> Vec<BoardPosition> {
        vec![]
    }
    /// Calculates all possible placement targets for a rock at the given position.
    fn place_targets_rock(&self, position: BoardPosition) -> Vec<BoardPosition> {
        vec![]
    }
    /// Calculates all possible placement targets for a knight at the given position.
    fn place_targets_knight(&self, position: BoardPosition) -> Vec<BoardPosition> {
        let offsets = vec![
            (1, 2),
            (2, 1),
            (2, -1),
            (1, -2),
            (-1, -2),
            (-2, -1),
            (-2, 1),
            (-1, 2),
        ];
        let targets_on_board = offsets.iter().filter_map(|d| position.add(*d));
        targets_on_board.filter(|p| self.can_place_at(*p)).collect()
    }
    /// Calculates all possible placement targets for a bishop at the given position.
    fn place_targets_bishop(&self, position: BoardPosition) -> Vec<BoardPosition> {
        vec![]
    }
    /// Calculates all possible placement targets for a queen at the given position.
    fn place_targets_queen(&self, position: BoardPosition) -> Vec<BoardPosition> {
        vec![]
    }
    /// Calculates all possible placement targets for a king at the given position.
    fn place_targets_king(&self, position: BoardPosition) -> Vec<BoardPosition> {
        vec![]
    }
    /// Decide whether the current player may place a lifted piece at the indicated position.
    ///
    /// This is only forbidden when the target position holds a piece of the own color
    /// without a dance partner.
    fn can_place_at(&self, target: BoardPosition) -> bool {
        let opponent_present = self
            .opponent_pieces()
            .get(target.0 as usize)
            .unwrap()
            .is_some();
        if opponent_present {
            true
        } else {
            let self_present = self
                .active_pieces()
                .get(target.0 as usize)
                .unwrap()
                .is_some();
            !self_present
        }
    }
}

impl PacoBoard for DenseBoard {
    fn execute(&mut self, action: PacoAction) -> Result<&mut Self, ()> {
        use PacoAction::*;
        match action {
            Lift(position) => self.lift(position),
            Place(position) => self.place(position),
        }
    }
    fn actions(&self) -> Vec<PacoAction> {
        use PacoAction::*;
        if let Some((position, piece_type)) = self.lifted_piece {
            // the player currently lifts a piece, we calculate all possible positions where
            // it can be placed down. This takes opponents pieces in considerations but won't
            // discard chaining into a blocked pawn (or simmilar).
            self.place_targets(position, piece_type)
                .iter()
                .map(|p| Place(*p))
                .collect()
        } else {
            // If no piece is lifted up, then we just return lifting actions of all pieces of the
            // current player.
            self.active_positions().map(Lift).collect()
        }
    }
}


impl Display for DenseBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PlayerColor::*;
        writeln!(
            f,
            "╔═══════════════════════════╗"
        )?;
        let mut trailing_bracket = false;
        let highlighted_position = self.lifted_piece.map(|l| (l.0).0 as usize);
        for y in (0..8).rev() {
            write!(f, "║ {}", y + 1)?;
            for x in 0..8 {
                let coord = BoardPosition::new(x, y).0 as usize;
                let w = self.white.get(coord).unwrap();
                let b = self.black.get(coord).unwrap();

                if trailing_bracket {
                    write!(f, ")")?;
                    trailing_bracket = false;
                } else if Some(coord) == highlighted_position {
                    write!(f, "(")?;
                    trailing_bracket = true;
                } else {
                    write!(f, " ")?;
                }

                match w {
                    Some(piece) => {
                        write!(f, "{}", White.paint_string(piece.to_char()))?;
                    }
                    None => {
                        write!(f, "{}", " ".underline())?;
                    }
                };

                match b {
                    Some(piece) => {
                        write!(f, "{}", Black.paint_string(piece.to_char()))?;
                    }
                    None => {
                        write!(f, "{}", " ".underline())?;
                    }
                };
            }
            writeln!(f, " ║")?;
        }

        let lifted = self.lifted_piece.map_or("*", |p| p.1.to_char());

        writeln!(
            f,
            "║ {} A  B  C  D  E  F  G  H  ║",
            self.current_player.paint_string(lifted)
        )?;
        write!(
            f,
            "╚═══════════════════════════╝"
        )?;
        Ok(())
    }

}

fn main() -> Result<(), ()> {
    use PacoAction::*;
    println!("Initial Board layout: ");
    let mut board = DenseBoard::new();
    println!("{}", board);
    board.execute(Lift(BoardPosition(11)))?;
    println!("{}", board);
    board.execute(Place(BoardPosition(27)))?;
    println!("{}", board);
    // Show possible moves of the black knight on b8.
    board.execute(Lift(BoardPosition::new(1, 7)))?;
    println!("{}", board);
    println!("{:?}", board.actions());
    Ok(())
}

