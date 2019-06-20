
use std::fmt;

use std::fmt::Debug;
use std::fmt::Display;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PieceType {
    Pawn,
    Rock,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PlayerColor {
    White,
    Black,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct BoardPosition(pub u8);

impl BoardPosition {
    pub fn x(self) -> u8 {
        self.0 % 8
    }
    pub fn y(self) -> u8 {
        self.0 / 8
    }
    pub fn new(x: u8, y: u8) -> Self {
        Self(x + 8 * y)
    }
    pub fn new_checked(x: i8, y: i8) -> Option<Self> {
        if x >= 0 && y >= 0 && x < 8 && y < 8 {
            Some(Self::new(x as u8, y as u8))
        } else {
            None
        }
    }
    pub fn add(self, other: (i8, i8)) -> Option<Self> {
        Self::new_checked(self.x() as i8 + other.0, self.y() as i8 + other.1)
    }

    /// Indicates whether the given position in on the pawn row of the `player` parameter.
    pub fn in_pawn_row(self, player: PlayerColor) -> bool {
        use PlayerColor::*;
        match player {
            White => self.y() == 1,
            Black => self.y() == 6,
        }
    }

    /// If the position is on a players home row, return that player.
    pub fn home_row(self) -> Option<PlayerColor> {
        use PlayerColor::*;
        if self.y() == 0 {
            Some(White)
        } else if self.y() == 7 {
            Some(Black)
        } else {
            None
        }
    }

    /// Returns the position where a pawn would be after moving forward by one step.
    /// This depends on the color of the active `player`.
    /// Returns `None` if the pawn is already on the home row of the other player.
    pub fn advance_pawn(self, player: PlayerColor) -> Option<Self> {
        use PlayerColor::*;
        match player {
            White => Self::new_checked(self.x() as i8, self.y() as i8 + 1),
            Black => Self::new_checked(self.x() as i8, self.y() as i8 + 1),
        }
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