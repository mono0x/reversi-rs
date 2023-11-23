use regex::Regex;
use std::fmt;
use std::io;
use std::io::Write;

macro_rules! adjacents {
    ($start:expr, $mask: expr, $shift: ident, $n:expr) => {{
        // find adjacent of start bits from mask
        let mut result = $mask & $shift($start, $n);
        // expand adjacents of start bits
        result |= $mask & $shift(result, $n);
        result |= $mask & $shift(result, $n);
        result |= $mask & $shift(result, $n);
        result |= $mask & $shift(result, $n);
        result |= $mask & $shift(result, $n);
        result
    }};
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct BitBoard {
    // (current (black), opponent (white))
    bits: (u64, u64), // MSB <- a1, b1, ... h1, a2, b2, ... , h8 -> LSB
}

impl BitBoard {
    pub fn new() -> Self {
        //   abcdefgh
        // 1 00000000 => 00
        // 2 00000000 => 00
        // 3 00000000 => 00
        // 4 00001000 => 08
        // 5 00010000 => 10
        // 6 00000000 => 00
        // 7 00000000 => 00
        // 8 00000000 => 00
        let black = 0x0000_0008_1000_0000;
        //   abcdefgh
        // 1 00000000 => 00
        // 2 00000000 => 00
        // 3 00000000 => 00
        // 4 00010000 => 10
        // 5 00001000 => 08
        // 6 00000000 => 00
        // 7 00000000 => 00
        // 8 00000000 => 00
        let white = 0x0000_0010_0800_0000;

        BitBoard {
            bits: (black, white),
        }
    }

    pub fn legal_moves(&self) -> u64 {
        #[inline]
        const fn flip_bits_dir(black: u64, white: u64, shift: u32, mask: u64) -> u64 {
            let mask = white & mask; // apply mask to prevent overflow by edge of board
            let l = adjacents!(black, mask, shift_l, shift);
            let r = adjacents!(black, mask, shift_r, shift);
            shift_l(l, shift) | shift_r(r, shift)
        }

        let blank = !(self.bits.0 | self.bits.1);

        let mut result = 0;
        for (shift, mask) in Self::SHIFT_AND_MASKS {
            result |= flip_bits_dir(self.bits.0, self.bits.1, shift, mask);
        }
        result & blank
    }

    pub fn legal_moves_vec(&self) -> Vec<u64> {
        let mut moves = Vec::new();
        let m = self.legal_moves();
        for i in 0..64 {
            let pos = 0x8000_0000_0000_0000 >> i;
            if m & pos != 0 {
                moves.push(pos);
            }
        }
        moves
    }

    pub fn do_move(&self, pos: u64) -> Self {
        assert_eq!(pos.count_ones(), 1);
        assert_eq!(self.legal_moves() & pos, pos);

        #[inline]
        const fn flip_bits_dir(black: u64, white: u64, pos: u64, shift: u32, mask: u64) -> u64 {
            let mask = white & mask; // apply mask to prevent overflow by edge of board

            // find adjacents starting from pos
            let l1 = adjacents!(pos, mask, shift_l, shift);
            let r2 = adjacents!(pos, mask, shift_r, shift);

            // find adjacents starting from opposide side across the opponents
            let l2 = adjacents!(black, mask, shift_l, shift);
            let r1 = adjacents!(black, mask, shift_r, shift);

            // common adjacents starting from both sides can be flippable
            (l1 & r1) | (r2 & l2)
        }

        let mut flip = 0;
        for (shift, mask) in Self::SHIFT_AND_MASKS {
            flip |= flip_bits_dir(self.bits.0, self.bits.1, pos, shift, mask);
        }
        let new_black = self.bits.0 ^ flip ^ pos;
        let new_white = self.bits.1 ^ flip;

        Self {
            // reverse black and white
            bits: (new_white, new_black),
        }
    }

    pub fn do_pass(&self) -> Self {
        assert_eq!(self.legal_moves(), 0);

        Self {
            // reverse black and white
            bits: (self.bits.1, self.bits.0),
        }
    }

    pub fn game_over(&self) -> bool {
        if self.legal_moves() != 0 {
            return false;
        }
        self.clone().do_pass().legal_moves() == 0
    }

    //   abcdefgh
    // 1 01111110 => 7e
    // 2 01111110 => 7e
    // 3 01111110 => 7e
    // 4 01111110 => 7e
    // 5 01111110 => 7e
    // 6 01111110 => 7e
    // 7 01111110 => 7e
    // 8 01111110 => 7e
    const HORIZONTAL_MASK: u64 = 0x7e7e7e7e7e7e7e7e;

    //   abcdefgh
    // 1 00000000 => 00
    // 2 11111111 => ff
    // 3 11111111 => ff
    // 4 11111111 => ff
    // 5 11111111 => ff
    // 6 11111111 => ff
    // 7 11111111 => ff
    // 8 00000000 => 00
    const VERTICAL_MASK: u64 = 0x00ffffffffffff00;

    //   abcdefgh
    // 1 00000000 => 00
    // 2 01111110 => 7e
    // 3 01111110 => 7e
    // 4 01111110 => 7e
    // 5 01111110 => 7e
    // 6 01111110 => 7e
    // 7 01111110 => 7e
    // 8 00000000 => 00
    const DIAGONAL_MASK: u64 = 0x007e7e7e7e7e7e00;

    const SHIFT_AND_MASKS: [(u32, u64); 4] = [
        // horizontal
        (1, Self::HORIZONTAL_MASK),
        // vertical
        (8, Self::VERTICAL_MASK),
        // digonal
        (7, Self::DIAGONAL_MASK),
        (9, Self::DIAGONAL_MASK),
    ];
}

#[inline]
const fn shift_l(lhs: u64, rhs: u32) -> u64 {
    lhs << rhs
}

#[inline]
const fn shift_r(lhs: u64, rhs: u32) -> u64 {
    lhs >> rhs
}

pub fn pos(x: u32, y: u32) -> u64 {
    0x8000_0000_0000_0000 >> ((x as u64) + (y as u64) * 8)
}

#[test]
fn test_board_legal_moves() {
    let board = BitBoard::new();
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00010000 => 10
    // 4 00100000 => 20
    // 5 00000100 => 04
    // 6 00001000 => 08
    // 7 00000000 => 00
    // 8 00000000 => 00
    assert_eq!(board.legal_moves(), 0x0000_1020_0408_0000);
    assert_eq!(
        board.legal_moves_vec(),
        vec![
            0x0000_1000_0000_0000,
            0x0000_0020_0000_0000,
            0x0000_0000_0400_0000,
            0x0000_0000_0008_0000,
        ]
    )
}

#[test]
fn test_board_do_move() {
    let mut board = BitBoard::new();
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000100 => 04
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 00000000 => 00
    board = board.do_move(0x0000_0000_0400_0000); // f5
    assert_eq!(
        board,
        BitBoard {
            bits: (
                //   abcdefgh
                // 1 00000000 => 00
                // 2 00000000 => 00
                // 3 00000000 => 00
                // 4 00010000 => 10
                // 5 00000000 => 00
                // 6 00000000 => 00
                // 7 00000000 => 00
                // 8 00000000 => 00
                0x0000_0010_0000_0000,
                //   abcdefgh
                // 1 00000000 => 00
                // 2 00000000 => 00
                // 3 00000000 => 00
                // 4 00001000 => 08
                // 5 00011100 => 1c
                // 6 00000000 => 00
                // 7 00000000 => 00
                // 8 00000000 => 00
                0x0000_0008_1c00_0000,
            ),
        }
    );
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000100 => 04
    // 7 00000000 => 00
    // 8 00000000 => 00
    board = board.do_move(0x0000_0000_0004_0000); // f6
    assert_eq!(
        board,
        BitBoard {
            bits: (
                //   abcdefgh
                // 1 00000000 => 00
                // 2 00000000 => 00
                // 3 00000000 => 00
                // 4 00001000 => 08
                // 5 00010100 => 14
                // 6 00000000 => 00
                // 7 00000000 => 00
                // 8 00000000 => 00
                0x0000_0008_1400_0000,
                //   abcdefgh
                // 1 00000000 => 00
                // 2 00000000 => 00
                // 3 00000000 => 00
                // 4 00010000 => 10
                // 5 00001000 => 08
                // 6 00000100 => 04
                // 7 00000000 => 00
                // 8 00000000 => 00
                0x0000_0010_0804_0000,
            ),
        }
    );
}

trait Player {
    fn next_move(&mut self, board: &BitBoard) -> io::Result<u64>;
}

struct HumanPlayer {}

impl Player for HumanPlayer {
    fn next_move(&mut self, board: &BitBoard) -> io::Result<u64> {
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
    fn next_move(&mut self, board: &BitBoard) -> io::Result<u64> {
        let moves = board.legal_moves_vec();
        Ok(moves[rand::random::<usize>() % moves.len()])
    }
}

#[rustfmt::skip]
const WEIGHT: [i32; 64] = [
    120, -20,  20,   5,   5,  20, -20, 120,
    -20, -40,  -5,  -5,  -5,  -5, -40, -20,
     20,  -5,  15,   3,   3,  15,  -5,  20,
      5,  -5,   3,   3,   3,   3,  -5,   5,
      5,  -5,   3,   3,   3,   3,  -5,   5,
     20,  -5,  15,   3,   3,  15,  -5,  20,
    -20, -40,  -5,  -5,  -5,  -5, -40, -20,
    120, -20,  20,   5,   5,  20, -20, 120,
];

const STABLE_WEIGHT: i32 = 120;

const STABLE_PATTERNS: [u64; 8] = [
    //   abcdefgh
    // 1 11100000 => e0
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 00000000 => 00
    0xe000_0000_0000_0000,
    //   abcdefgh
    // 1 00000111 => 07
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 00000000 => 00
    0x0700_0000_0000_0000,
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 11100000 => e0
    0x0000_0000_0000_e000,
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 00000111 => 07
    0x0000_0000_0000_0007,
    //   abcdefgh
    // 1 10000000 => 80
    // 2 10000000 => 80
    // 3 10000000 => 80
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 00000000 => 00
    0x8080_8000_0000_0000,
    //   abcdefgh
    // 1 00000001 => 01
    // 2 00000001 => 01
    // 3 00000001 => 01
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000000 => 00
    // 7 00000000 => 00
    // 8 00000000 => 00
    0x0101_0100_0000_0000,
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 10000000 => 80
    // 7 10000000 => 80
    // 8 10000000 => 80
    0x0000_0000_0080_8080,
    //   abcdefgh
    // 1 00000000 => 00
    // 2 00000000 => 00
    // 3 00000000 => 00
    // 4 00000000 => 00
    // 5 00000000 => 00
    // 6 00000001 => 01
    // 7 00000001 => 01
    // 8 00000001 => 01
    0x0000_0000_0001_0101,
];

struct AIPlayer {}

impl AIPlayer {
    fn search(&self, board: &BitBoard) -> u64 {
        let (depth, endgame) = self.parameters(board);
        let moves = board.legal_moves_vec();
        let mut alpha = -i32::MAX;
        let beta = i32::MAX;
        let mut best_pos = 0;
        for &pos in &moves {
            let score = -self.negamax(&board.do_move(pos), depth, endgame, -beta, -alpha);
            if score > alpha {
                alpha = score;
                best_pos = pos;
            }
        }
        best_pos
    }

    fn negamax(&self, board: &BitBoard, depth: u32, endgame: bool, alpha: i32, beta: i32) -> i32 {
        if depth == 0 || board.game_over() {
            return self.evaluate(board, endgame);
        }
        let moves = board.legal_moves_vec();
        if moves.is_empty() {
            return -self.negamax(&board.do_pass(), depth - 1, endgame, alpha, beta);
        }
        let mut alpha = alpha;
        let mut best = -i32::MAX;

        let ordered;
        if depth >= 2 {
            let mut items = Vec::new();
            for &pos in &moves {
                let score = -self.evaluate(&board.do_move(pos), endgame);
                items.push((score, pos));
            }
            items.sort_by(|(a, _), (b, _)| b.cmp(a));
            ordered = items.iter().map(|(_, pos)| *pos).collect::<Vec<_>>();
        } else {
            ordered = moves;
        }

        for pos in ordered {
            let score = -self.negamax(&board.do_move(pos), depth - 1, endgame, -beta, -alpha);
            if score >= beta {
                return score;
            }
            alpha = std::cmp::max(alpha, score);
            best = std::cmp::max(best, score);
        }
        best
    }

    fn parameters(&self, board: &BitBoard) -> (u32, bool) {
        if (board.bits.0 | board.bits.1).count_zeros() <= 14 {
            return (u32::MAX, true);
        } else {
            return (9, false);
        }
    }

    fn evaluate(&self, board: &BitBoard, endgame: bool) -> i32 {
        let black = board.bits.0.count_ones() as i32;
        let white = board.bits.1.count_ones() as i32;
        if endgame {
            return black - white;
        }
        if black == 0 {
            return -i32::MAX;
        }
        if white == 0 {
            return i32::MAX;
        }
        let mut pos = 0x8000_0000_0000_0000;
        let mut score = 0;
        for i in 0..64 {
            if board.bits.0 & pos != 0 {
                score += WEIGHT[i];
            } else if board.bits.1 & pos != 0 {
                score -= WEIGHT[i];
            }
            pos >>= 1;
        }
        for pattern in STABLE_PATTERNS {
            if board.bits.0 & pattern == pattern {
                score += STABLE_WEIGHT;
            } else if board.bits.1 & pattern == pattern {
                score -= STABLE_WEIGHT;
            }
        }
        score
    }
}

impl Player for AIPlayer {
    fn next_move(&mut self, board: &BitBoard) -> io::Result<u64> {
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
