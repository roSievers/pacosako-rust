
use pacosako::{PacoError, DenseBoard, EditorBoard, analyse_sako, PacoBoard};

fn main() -> Result<(), PacoError> {
    let schema = "8 .. .. .. .. .K .B .. .R
7 .P .. .. .. .P .. .. .P
6 .. .. .P .. QB .R .P ..
5 .. .P .. .. BP .. N. ..
4 .. P. .. PQ .. .. BN PN
3 .. .. N. .. PP .. .. ..
2 P. .. P. .. .. P. .. P.
1 R. .. .. .. .. R. K. ..
* A  B  C  D  E  F  G  H";

    let parsed = pacosako::parser::matrix(schema);

    if let Ok((_, matrix)) = parsed {
        let mut board = DenseBoard::from_squares(matrix.0);

        // Print the board as json using serde.
        let pieces : EditorBoard = (&board).into();

        // Serialize it to a JSON string.
        let j = serde_json::to_string(&pieces).unwrap();
        println!("{}", j);

        board.current_player = board.current_player().other();
        analyse_sako(board)?;
    }

    Ok(())
}