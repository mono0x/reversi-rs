use crate::bitboard::BitBoard;

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

pub(crate) struct AIPlayer {}

impl AIPlayer {
    pub(crate) fn search(&self, board: &BitBoard) -> u64 {
        let (depth, endgame) = self.parameters(board);
        self.search_with_depth(board, depth, endgame).0
    }

    // Depth counts plies after the root move, matching the game player's search.
    pub(crate) fn search_with_depth(
        &self,
        board: &BitBoard,
        depth: u32,
        endgame: bool,
    ) -> (u64, i32) {
        let moves = board.legal_moves_vec();
        let mut alpha = -i32::MAX;
        let beta = i32::MAX;
        let mut best_pos = 0;
        for &pos in &moves {
            let score = -self.negamax(&board.do_move(pos), false, depth, endgame, -beta, -alpha);
            if score > alpha {
                alpha = score;
                best_pos = pos;
            }
        }
        (best_pos, alpha)
    }

    fn negamax(
        &self,
        board: &BitBoard,
        passed: bool,
        depth: u32,
        endgame: bool,
        alpha: i32,
        beta: i32,
    ) -> i32 {
        if depth == 0 {
            return self.evaluate(board, endgame);
        }
        let moves = board.legal_moves_vec();
        if moves.is_empty() {
            if passed {
                return self.evaluate(board, endgame);
            }
            return -self.negamax(&board.do_pass(), true, depth - 1, endgame, alpha, beta);
        }
        let mut alpha = alpha;
        let mut best = -i32::MAX;

        let ordered;
        if depth >= 2 {
            let mut items = Vec::new();
            for &pos in &moves {
                let score = -self.evaluate(&board.do_move(pos), false);
                items.push((score, pos));
            }
            items.sort_by(|(a, _), (b, _)| b.cmp(a));
            ordered = items.iter().map(|(_, pos)| *pos).collect::<Vec<_>>();
        } else {
            ordered = moves;
        }

        for pos in ordered {
            let score = -self.negamax(
                &board.do_move(pos),
                false,
                depth - 1,
                endgame,
                -beta,
                -alpha,
            );
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
