
use colored::*;
use std::fmt;
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
struct PacoBoard {
    white: Vec<Option<PieceType>>,
    black: Vec<Option<PieceType>>,
    dance: Vec<bool>,
    current_player: PlayerColor,
    // The lifted piece has no player color as it must belong to the current player.
    lifted_piece: Option<(BoardPosition, PieceType)>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct BoardPosition(u8);

impl PacoBoard {
    fn new() -> Self {
        use PieceType::*;
        let mut result: Self = PacoBoard {
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
}


impl Display for PacoBoard {
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
                let coord = x + 8 * y;
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
    println!("Initial Board layout: ");
    let mut board = PacoBoard::new();
    println!("{}", board);
    board.lift(BoardPosition(10))?;
    println!("{}", board);
    board.place(BoardPosition(26))?;
    board.other_player();
    println!("{}", board);
    Ok(())
}

