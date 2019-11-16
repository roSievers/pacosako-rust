/// This example shows you how to randomly generate board positions to
/// find interesting positions.
use pacosako::{DenseBoard, PacoError};
use std::cmp::{max, min};

// use rand::distributions::{Distribution, Standard};
use rand::{thread_rng, Rng};

fn main() -> Result<(), PacoError> {
    // Randomly generate DenseBoards and try to find one with long chains.

    let mut rng = thread_rng();
    let mut best_length = 0;
    let mut counter: usize = 0;

    loop {
        counter += 1;
        let board: DenseBoard = rng.gen();
        let sequences = pacosako::find_sako_sequences(&((&board).into()))?;
        let max_white: usize = sequences
            .white
            .iter()
            .map(|chain| chain.len())
            .max()
            .unwrap_or(0);
        let max_black: usize = sequences
            .white
            .iter()
            .map(|chain| chain.len())
            .max()
            .unwrap_or(0);
        let min_white: usize = sequences
            .white
            .iter()
            .map(|chain| chain.len())
            .min()
            .unwrap_or(0);
        let min_black: usize = sequences
            .white
            .iter()
            .map(|chain| chain.len())
            .min()
            .unwrap_or(0);
        let max_chain_length: usize = max(max_white, max_black);
        let min_chain_length: usize = max(min_white, min_black);

        if min_chain_length > best_length {
            best_length = min_chain_length;
        } else {
            continue;
        }

        println!("\n\n");
        println!("Randomly generated board (n = {}):", counter);
        println!("{}", board);
        println!("{:?}", sequences);
        println!("Best Max length: {}", max_chain_length);
        println!("Best Min length: {}", min_chain_length);
    }
}
