use regex::Regex;
use std::fmt;
use std::io;
use std::io::Write;

mod bitboard;
mod search;

use bitboard::BitBoard;
use search::AIPlayer;

pub fn pos(x: u32, y: u32) -> u64 {
    0x8000_0000_0000_0000 >> ((x as u64) + (y as u64) * 8)
}

trait Player {
    fn next_move(&self, board: &BitBoard) -> io::Result<u64>;
}

#[allow(dead_code)]
struct HumanPlayer {}

impl Player for HumanPlayer {
    fn next_move(&self, board: &BitBoard) -> io::Result<u64> {
        let re = Regex::new(r"^[a-h][1-8]$").unwrap();
        let legal_moves = board.legal_moves();
        loop {
            print!("> ");
            io::stdout().flush()?;
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer)?;
            let input = buffer.trim();
            if !re.is_match(&input) {
                println!("invalid input: {}", input);
                continue;
            }
            let mut chars = input.chars();
            let c = chars.next().unwrap();
            let r = chars.next().unwrap();
            let pos = pos(
                (c as i32 - 'a' as i32) as u32,
                (r as i32 - '1' as i32) as u32,
            );
            if pos & legal_moves == 0 {
                println!("illegal move: {}", input);
                continue;
            }
            return Ok(pos);
        }
    }
}

struct RandomPlayer {}

impl Player for RandomPlayer {
    fn next_move(&self, board: &BitBoard) -> io::Result<u64> {
        let moves = board.legal_moves_vec();
        Ok(moves[rand::random::<usize>() % moves.len()])
    }
}

impl Player for AIPlayer {
    fn next_move(&self, board: &BitBoard) -> io::Result<u64> {
        Ok(self.search(board))
    }
}

struct GameContext {
    board: BitBoard,
    turns: usize,
}

impl GameContext {
    fn new() -> Self {
        Self {
            board: BitBoard::new(),
            turns: 0,
        }
    }
}

impl fmt::Display for GameContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "  abcdefgh")?;
        let mut pos = 0x8000_0000_0000_0000;
        let (black, white);
        if self.turns % 2 == 0 {
            (black, white) = self.board.bits;
        } else {
            (white, black) = self.board.bits;
        }
        for i in 0..8 {
            write!(f, "{} ", i + 1)?;
            for _ in 0..8 {
                if black & pos != 0 {
                    write!(f, "O")?;
                } else if white & pos != 0 {
                    write!(f, "X")?;
                } else {
                    write!(f, ".")?;
                }
                pos >>= 1;
            }
            writeln!(f)?;
        }
        writeln!(
            f,
            "black: {}, white: {}",
            black.count_ones(),
            white.count_ones(),
        )?;
        Ok(())
    }
}

struct Game {
    players: [Box<dyn Player>; 2],
}

impl Game {
    fn new(black: Box<dyn Player>, white: Box<dyn Player>) -> Self {
        Game {
            players: [black, white],
        }
    }

    fn play(&mut self) -> io::Result<()> {
        let mut ctx = GameContext::new();
        loop {
            println!("{}", ctx);
            if ctx.board.game_over() {
                break;
            }
            let legal_moves = ctx.board.legal_moves();
            if legal_moves == 0 {
                ctx.board = ctx.board.do_pass();
                ctx.turns += 1;
                continue;
            }
            let pos = self.players[ctx.turns % 2].next_move(&ctx.board)?;
            ctx.board = ctx.board.do_move(pos);
            ctx.turns += 1;
        }
        Ok(())
    }
}

fn main() -> io::Result<()> {
    let mut game = Game::new(Box::new(AIPlayer {}), Box::new(RandomPlayer {}));
    game.play()?;
    Ok(())
}
