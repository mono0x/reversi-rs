use std::io;
use std::time::Duration;

use serde_json::{json, Map, Value};

use crate::{bitboard::BitBoard, pos, Player};

const ENDPOINT: &str = "https://api.typesafe.ai/v1/systemone";

pub(crate) struct JevPlayer {
    agent: ureq::Agent,
    api_key: String,
    model: String,
}

impl JevPlayer {
    pub(crate) fn from_env() -> io::Result<Self> {
        let api_key = std::env::var("TYPESAFE_API_KEY")
            .ok()
            .filter(|key| !key.trim().is_empty())
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "TYPESAFE_API_KEY is required for jev",
                )
            })?;
        let model = match std::env::var("TYPESAFE_MODEL") {
            Ok(model) if !model.trim().is_empty() => model,
            Err(std::env::VarError::NotPresent) => "jev-latest".to_owned(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "TYPESAFE_MODEL must be nonempty UTF-8",
                ))
            }
        };
        Ok(Self {
            agent: ureq::Agent::config_builder()
                .timeout_global(Some(Duration::from_secs(30)))
                .build()
                .into(),
            api_key,
            model,
        })
    }
}

impl Player for JevPlayer {
    fn next_move(&self, board: &BitBoard) -> io::Result<u64> {
        let moves = board.legal_moves_vec();
        match moves.as_slice() {
            [] => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "jev has no legal moves; pass in the game loop",
                ))
            }
            [only] => return Ok(*only),
            _ => {}
        }
        let mut response = self
            .agent
            .post(ENDPOINT)
            .header("Authorization", format!("Bearer {}", self.api_key))
            .send_json(request(board, &self.model))
            .map_err(|error| io::Error::other(format!("Jev request failed: {error}")))?;
        let body: Value = response.body_mut().read_json().map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid Jev response: {error}"),
            )
        })?;
        selected_move(&body, &moves)
    }
}

fn coordinate(square: u64) -> String {
    let index = square.leading_zeros();
    format!("{}{}", (b'a' + (index % 8) as u8) as char, index / 8 + 1)
}

fn move_description(board: &BitBoard, square: u64) -> String {
    let next = board.do_move(square);
    // do_move swaps the sides; keep M as the chooser in every option.
    format!(
        "Place your disc at {}. Board after this move:\n{}",
        coordinate(square),
        board_diagram(next.bits.1, next.bits.0),
    )
}

fn board_diagram(own: u64, opponent: u64) -> String {
    let mut diagram = String::from("  abcdefgh\n");
    for y in 0..8 {
        diagram.push(char::from(b'1' + y as u8));
        diagram.push(' ');
        for x in 0..8 {
            let square = pos(x, y);
            diagram.push(if own & square != 0 {
                'M'
            } else if opponent & square != 0 {
                'O'
            } else {
                '.'
            });
        }
        diagram.push('\n');
    }
    diagram
}

fn request(board: &BitBoard, model: &str) -> Value {
    let criteria: Map<String, Value> = board
        .legal_moves_vec()
        .into_iter()
        .map(|square| {
            let name = coordinate(square);
            let description = json!(move_description(board, square));
            (name, description)
        })
        .collect();
    json!({
        "model": model,
        "state": {
            "game": "Reversi (Othello), standard 8x8 rules",
            "board": board_diagram(board.bits.0, board.bits.1),
            "legend": "In every board, M = your discs, O = opponent discs, . = empty. Columns a-h run left to right; rows 1-8 run top to bottom. M is to move on the board in state. Each option shows the board after M plays that move; O moves next unless O must pass or the game has ended.",
            "rules": "A move brackets and flips opponent discs in any of eight directions. A player with no legal move passes. The game ends when neither player can move; the player with more discs wins. All supplied choices are legal moves."
        },
        "questions": {
            "move": {
                "type": "choice",
                "instructions": "Which legal move gives you the best chance of winning this Reversi game? Choose a move for M using the current board in state and the board after each move shown in its option.",
                "criteria": criteria
            }
        }
    })
}

fn selected_move(response: &Value, moves: &[u64]) -> io::Result<u64> {
    let answer = &response["answers"]["move"];
    if answer["type"] == "choice" {
        if let Some(choice) = answer["choice"].as_str() {
            if let Some(&square) = moves.iter().find(|&&square| coordinate(square) == choice) {
                return Ok(square);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::InvalidData,
        "Jev response must contain a choice from the legal moves",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn forced_move_and_pass_need_no_api_request() {
        let player = JevPlayer {
            agent: ureq::Agent::new_with_defaults(),
            api_key: String::new(),
            model: String::new(),
        };
        let board = BitBoard {
            bits: (pos(0, 0), pos(1, 0)),
        };
        assert_eq!(player.next_move(&board).unwrap(), pos(2, 0));
        let passed = BitBoard {
            bits: (board.bits.1, board.bits.0),
        };
        assert_eq!(
            player.next_move(&passed).unwrap_err().kind(),
            io::ErrorKind::InvalidInput
        );
    }

    #[test]
    fn initial_board_and_legal_choices() {
        let payload = request(&BitBoard::new(), "test-model");
        assert_eq!(payload["model"], "test-model");
        assert_eq!(payload["state"]["board"], "  abcdefgh\n1 ........\n2 ........\n3 ........\n4 ...OM...\n5 ...MO...\n6 ........\n7 ........\n8 ........\n");
        let choices = payload["questions"]["move"]["criteria"]
            .as_object()
            .unwrap();
        assert_eq!(
            choices.keys().map(String::as_str).collect::<Vec<_>>(),
            ["c4", "d3", "e6", "f5"]
        );
        assert_eq!(choices["d3"], "Place your disc at d3. Board after this move:\n  abcdefgh\n1 ........\n2 ........\n3 ...M....\n4 ...MM...\n5 ...MO...\n6 ........\n7 ........\n8 ........\n");
    }

    #[test]
    fn board_is_from_the_next_players_perspective() {
        let board = BitBoard::new().do_move(pos(3, 2));
        let payload = request(&board, "test-model");
        assert_eq!(payload["state"]["board"], "  abcdefgh\n1 ........\n2 ........\n3 ...O....\n4 ...OO...\n5 ...OM...\n6 ........\n7 ........\n8 ........\n");
        assert_eq!(payload["questions"]["move"]["criteria"]["c3"], "Place your disc at c3. Board after this move:\n  abcdefgh\n1 ........\n2 ........\n3 ..MO....\n4 ...MO...\n5 ...OM...\n6 ........\n7 ........\n8 ........\n");
    }

    #[test]
    fn coordinates_cover_the_entire_board() {
        for y in 0..8 {
            for x in 0..8 {
                assert_eq!(
                    coordinate(pos(x, y)),
                    format!("{}{}", (b'a' + x as u8) as char, y + 1)
                );
            }
        }
    }

    #[test]
    fn accepts_legal_choices_and_rejects_invalid_answers() {
        let moves = BitBoard::new().legal_moves_vec();
        for &square in &moves {
            let response =
                json!({"answers": {"move": {"type": "choice", "choice": coordinate(square)}}});
            assert_eq!(selected_move(&response, &moves).unwrap(), square);
        }
        for answer in [
            json!(null),
            json!({"type": "score", "choice": "d3"}),
            json!({"type": "choice"}),
            json!({"type": "choice", "choice": 19}),
            json!({"type": "choice", "choice": "a1"}),
            json!({"type": "choice", "choice": "pass"}),
        ] {
            let response = json!({"answers": {"move": answer}});
            assert_eq!(
                selected_move(&response, &moves).unwrap_err().kind(),
                io::ErrorKind::InvalidData
            );
        }
    }
}
