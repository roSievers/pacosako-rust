
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

#[derive(Clone, Debug)]
enum PacoError {
    // You can not "Lift" when the hand is full.
    LiftFullHand,
    // You can not "Lift" from an empty position.
    LiftEmptyPosition,
    // You can not "Place" when the hand is empty.
    PlaceEmptyHand,
    // You can not "Place" a pair when the target is occupied.
    PlacePairFullPosition,
}

impl PlayerColor {
    #[allow(dead_code)]
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
    lifted_piece: Hand,
}

/// Represents zero to two lifted pieces
/// The owner of the pieces must be tracked externally, usually this will be the current player.
#[derive(Clone, Debug, PartialEq, Eq)]
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

impl Hand {
    fn position(&self) -> Option<BoardPosition> {
        use Hand::*;
        match self {
            Empty => None,
            Single { position, .. } => Some(*position),
            Pair { position, .. } => Some(*position),
        }
    }
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
    fn execute(&mut self, action: PacoAction) -> Result<&mut Self, PacoError>;
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
            lifted_piece: Hand::Empty,
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
    fn lift(&mut self, position: BoardPosition) -> Result<&mut Self, PacoError> {
        if self.lifted_piece != Hand::Empty {
            return Err(PacoError::LiftFullHand);
        }
        // We unwrap the pieces once to remove the outer Some() from the .get_mut(..) call.
        // We still recieve an optional where None represents an empty square.
        let piece = *self.active_pieces().get(position.0 as usize).unwrap();
        let partner = *self.opponent_pieces().get(position.0 as usize).unwrap();

        if let Some(piece_type) = piece {
            if let Some(partner_type) = partner {
                self.lifted_piece = Hand::Pair {
                    piece: piece_type,
                    partner: partner_type,
                    position,
                };
                *self
                    .opponent_pieces_mut()
                    .get_mut(position.0 as usize)
                    .unwrap() = None;
            } else {
                self.lifted_piece = Hand::Single {
                    piece: piece_type,
                    position,
                };
            }
            *self
                .active_pieces_mut()
                .get_mut(position.0 as usize)
                .unwrap() = None;
            Ok(self)
        } else {
            Err(PacoError::LiftEmptyPosition)
        }
    }

    /// Places the piece that is currently lifted back on the board.
    /// Returns an error if no piece is currently being lifted.
    fn place(&mut self, target: BoardPosition) -> Result<&mut Self, PacoError> {
        match self.lifted_piece {
            Hand::Empty => Err(PacoError::PlaceEmptyHand),
            Hand::Single { piece, .. } => {
                // Read piece currently on the board at the target position and place the
                // held piece there.
                let board_piece = *self.active_pieces().get(target.0 as usize).unwrap();
                *self.active_pieces_mut().get_mut(target.0 as usize).unwrap() = Some(piece);
                if let Some(new_hand_piece) = board_piece {
                    self.lifted_piece = Hand::Single {
                        piece: new_hand_piece,
                        position: target,
                    };
                } else {
                    self.lifted_piece = Hand::Empty;
                    self.current_player = self.current_player.other();
                }
                Ok(self)
            }
            Hand::Pair { piece, partner, .. } => {
                let board_piece = self.active_pieces().get(target.0 as usize).unwrap();
                let board_partner = self.opponent_pieces().get(target.0 as usize).unwrap();

                if board_piece.is_some() || board_partner.is_some() {
                    Err(PacoError::PlacePairFullPosition)
                } else {
                    *self.active_pieces_mut().get_mut(target.0 as usize).unwrap() = Some(piece);
                    *self
                        .opponent_pieces_mut()
                        .get_mut(target.0 as usize)
                        .unwrap() = Some(partner);
                    self.lifted_piece = Hand::Empty;
                    self.current_player = self.current_player.other();
                    Ok(self)
                }
            }
        }
    }

    /// The Dense Board representation containing only pieces of the current player.
    fn active_pieces(&self) -> &Vec<Option<PieceType>> {
        match self.current_player {
            PlayerColor::White => &self.white,
            PlayerColor::Black => &self.black,
        }
    }

    /// The Dense Board representation containing only pieces of the opponent player.
    fn opponent_pieces(&self) -> &Vec<Option<PieceType>> {
        match self.current_player {
            PlayerColor::White => &self.black,
            PlayerColor::Black => &self.white,
        }
    }

    /// The Dense Board representation containing only pieces of the current player.
    fn active_pieces_mut(&mut self) -> &mut Vec<Option<PieceType>> {
        match self.current_player {
            PlayerColor::White => &mut self.white,
            PlayerColor::Black => &mut self.black,
        }
    }

    /// The Dense Board representation containing only pieces of the opponent player.
    fn opponent_pieces_mut(&mut self) -> &mut Vec<Option<PieceType>> {
        match self.current_player {
            PlayerColor::White => &mut self.black,
            PlayerColor::Black => &mut self.white,
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
    /// This is intended to recieve its own lifted piece as input but will work if the
    /// input piece is different.
    fn place_targets(
        &self,
        position: BoardPosition,
        piece_type: PieceType,
        is_pair: bool,
    ) -> Vec<BoardPosition> {
        use PieceType::*;
        match piece_type {
            Pawn => self.place_targets_pawn(position, is_pair),
            Rock => self.place_targets_rock(position, is_pair),
            Knight => self.place_targets_knight(position, is_pair),
            Bishop => self.place_targets_bishop(position, is_pair),
            Queen => self.place_targets_queen(position, is_pair),
            King => self.place_targets_king(position),
        }
    }

    /// Calculates all possible placement targets for a pawn at the given position.
    fn place_targets_pawn(&self, position: BoardPosition, is_pair: bool) -> Vec<BoardPosition> {
        use PlayerColor::White;
        let mut possible_moves = Vec::new();

        let forward = if self.current_player == White { 1 } else { -1 };

        // Striking left & right, this is only possible if there is a target
        // and in particular this is never possible for a pair.
        if !is_pair {
            let strike_directions = [(-1, forward), (1, forward)];
            let targets_on_board = strike_directions.iter().filter_map(|d| position.add(*d));

            // TODO: The filter function can be more performant by directly checking
            // "opponent_present" instead.
            targets_on_board
                .filter(|p| self.can_place_single_at(*p) && !self.is_empty(*p))
                .for_each(|p| possible_moves.push(p));
        }

        // Moving forward, this is similar to a king
        if let Some(step) = position.add((0, forward)) {
            if self.is_empty(step) {
                possible_moves.push(step);
                // If we are on the base row, check if we can move another step.
                let base_row = if self.current_player == White { 1 } else { 6 };
                if position.y() == base_row {
                    if let Some(step_2) = step.add((0, forward)) {
                        if self.is_empty(step_2) {
                            possible_moves.push(step_2);
                        }
                    }
                }
            }
        }

        // TODO: En passant, see https://en.wikipedia.org/wiki/En_passant

        possible_moves
    }
    /// Calculates all possible placement targets for a rock at the given position.
    fn place_targets_rock(&self, position: BoardPosition, is_pair: bool) -> Vec<BoardPosition> {
        let directions = vec![(1, 0), (0, 1), (-1, 0), (0, -1)];
        directions
            .iter()
            .flat_map(|d| self.slide_targets(position, *d, is_pair))
            .collect()
    }
    /// Calculates all possible placement targets for a knight at the given position.
    fn place_targets_knight(&self, position: BoardPosition, is_pair: bool) -> Vec<BoardPosition> {
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
        if is_pair {
            targets_on_board.filter(|p| self.is_empty(*p)).collect()
        } else {
            targets_on_board
                .filter(|p| self.can_place_single_at(*p))
                .collect()
        }
    }
    /// Calculates all possible placement targets for a bishop at the given position.
    fn place_targets_bishop(&self, position: BoardPosition, is_pair: bool) -> Vec<BoardPosition> {
        let directions = vec![(1, 1), (-1, 1), (1, -1), (-1, -1)];
        directions
            .iter()
            .flat_map(|d| self.slide_targets(position, *d, is_pair))
            .collect()
    }
    /// Calculates all possible placement targets for a queen at the given position.
    fn place_targets_queen(&self, position: BoardPosition, is_pair: bool) -> Vec<BoardPosition> {
        let directions = vec![
            (1, 2),
            (2, 1),
            (2, -1),
            (1, -2),
            (-1, -2),
            (-2, -1),
            (-2, 1),
            (-1, 2),
        ];
        directions
            .iter()
            .flat_map(|d| self.slide_targets(position, *d, is_pair))
            .collect()
    }
    /// Calculates all possible placement targets for a king at the given position.
    fn place_targets_king(&self, position: BoardPosition) -> Vec<BoardPosition> {
        let offsets = vec![
            (0, 1),
            (1, 1),
            (1, 0),
            (1, -1),
            (0, -1),
            (-1, -1),
            (-1, 0),
            (-1, 1),
        ];
        let targets_on_board = offsets.iter().filter_map(|d| position.add(*d));
        // Placing the king works like placing a pair, as he can only be placed on empty squares.
        targets_on_board.filter(|p| self.is_empty(*p)).collect()

        // TODO: Casteling. This depends on a working Ŝako solver, as the king must not be
        // in Ŝako in order to castle.
    }
    /// Decide whether the current player may place a single lifted piece at the indicated position.
    ///
    /// This is only forbidden when the target position holds a piece of the own color
    /// without a dance partner.
    fn can_place_single_at(&self, target: BoardPosition) -> bool {
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
    /// Decide whethe a pair may be placed at the indicated position.
    ///
    /// This is only allowed if the position is completely empty.
    fn is_empty(&self, target: BoardPosition) -> bool {
        self.white.get(target.0 as usize).unwrap().is_none()
            && self.black.get(target.0 as usize).unwrap().is_none()
    }
    /// Calculates all targets by sliding step by step in a given direction and stopping at the
    /// first obstacle or at the end of the board.
    fn slide_targets(
        &self,
        start: BoardPosition,
        (dx, dy): (i8, i8),
        is_pair: bool,
    ) -> Vec<BoardPosition> {
        let mut possible_moves = Vec::new();
        let mut slide = start.add((dx, dy));

        // This while loop leaves if we drop off the board or if we hit a target.
        // The is_pair parameter determines, if the first thing we hit is a valid target.
        while let Some(target) = slide {
            if self.is_empty(target) {
                possible_moves.push(target);
                slide = target.add((dx, dy));
            } else if !is_pair && self.can_place_single_at(target) {
                possible_moves.push(target);
                slide = None;
            } else {
                slide = None;
            }
        }
        possible_moves
    }
}

impl PacoBoard for DenseBoard {
    fn execute(&mut self, action: PacoAction) -> Result<&mut Self, PacoError> {
        use PacoAction::*;
        match action {
            Lift(position) => self.lift(position),
            Place(position) => self.place(position),
        }
    }
    fn actions(&self) -> Vec<PacoAction> {
        use PacoAction::*;
        match self.lifted_piece {
            Hand::Empty => {
                // If no piece is lifted up, then we just return lifting actions of all pieces of
                // the current player.
                self.active_positions().map(Lift).collect()
            }
            Hand::Single { piece, position } => {
                // the player currently lifts a piece, we calculate all possible positions where
                // it can be placed down. This takes opponents pieces in considerations but won't
                // discard chaining into a blocked pawn (or simmilar).
                self.place_targets(position, piece, false)
                    .iter()
                    .map(|p| Place(*p))
                    .collect()
            }
            Hand::Pair {
                piece, position, ..
            } => self
                .place_targets(position, piece, true)
                .iter()
                .map(|p| Place(*p))
                .collect(),
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
        let highlighted_position = self.lifted_piece.position().map(|p| p.0 as usize);
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

        match self.lifted_piece {
            Hand::Empty => writeln!(
                f,
                "║ {} A  B  C  D  E  F  G  H  ║",
                self.current_player.paint_string("*")
            )?,
            Hand::Single { piece, .. } => {
                writeln!(
                    f,
                    "║ {} A  B  C  D  E  F  G  H  ║",
                    self.current_player.paint_string(piece.to_char())
                )?;
            }
            Hand::Pair { piece, partner, .. } => {
                let (w, b) = match self.current_player {
                    PlayerColor::White => (piece, partner),
                    PlayerColor::Black => (partner, piece),
                };
                writeln!(
                    f,
                    "║{}{} A  B  C  D  E  F  G  H  ║",
                    White.paint_string(w.to_char()),
                    Black.paint_string(b.to_char())
                )?;
            }
        }
        write!(
            f,
            "╚═══════════════════════════╝"
        )?;
        Ok(())
    }

}

fn main() -> Result<(), PacoError> {
    use PacoAction::*;
    println!("Initial Board layout: ");
    let mut board = DenseBoard::new();
    println!("{}", board);
    board.execute(Lift(BoardPosition::new(3, 1)))?;
    println!("{}", board);
    board.execute(Place(BoardPosition::new(3, 3)))?;
    println!("{}", board);
    // Show possible moves of the black knight on b8.
    board.execute(Lift(BoardPosition::new(1, 7)))?;
    println!("{}", board);

    // This may not be legal, but we want to try moving pairs.
    board.execute(Place(BoardPosition::new(3, 3)))?;
    println!("{}", board);

    board.execute(Lift(BoardPosition::new(3, 3)))?;
    println!("{}", board);
    board.execute(Place(BoardPosition::new(3, 4)))?;
    println!("{}", board);

    // println!("{:?}", board.actions());
    Ok(())
}

