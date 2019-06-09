
use colored::*;
use std::fmt;
use std::fmt::Display;

#[derive(Clone, Copy, Debug)]
enum PieceType {
    Pawn,
    Rock,
    Knight,
    Bishop,
    Queen,
    King,
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
}

impl PacoBoard {
    fn new() -> Self {
        use PieceType::*;
        let mut result: Self = PacoBoard {
            white: Vec::with_capacity(64),
            black: Vec::with_capacity(64),
            dance: vec![false; 64],
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
}


impl Display for PacoBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "╔═══════════════════════════╗"
        )?;
        for y in (0..8).rev() {
            write!(f, "║ {}", y + 1)?;
            for x in 0..8 {
                let coord = x + 8 * y;
                let w = self.white.get(coord).unwrap();
                let b = self.black.get(coord).unwrap();

                match w {
                    Some(piece) => {
                        write!(f, " {}", piece.to_char().red())?;
                    }
                    None => {
                        write!(f, " {}", " ".underline())?;
                    }
                };

                match b {
                    Some(piece) => {
                        write!(f, "{}", piece.to_char().blue())?;
                    }
                    None => {
                        write!(f, "{}", " ".underline())?;
                    }
                };
            }
            writeln!(f, " ║")?;
        }
        writeln!(f, "║   A  B  C  D  E  F  G  H  ║")?;
        writeln!(
            f,
            "╚═══════════════════════════╝"
        )?;
        Ok(())
    }

}

fn main() {
    println!("Initial Board layout: ");
    let board = PacoBoard::new();
    println!("{}", board);
}

