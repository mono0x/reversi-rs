use crate::bitboard::BitBoard;
use rand::seq::SliceRandom;

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

const WEIGHT_MASKS: [(i32, u64); 8] = {
    let mut groups = [
        (-40, 0),
        (-20, 0),
        (-5, 0),
        (3, 0),
        (5, 0),
        (15, 0),
        (20, 0),
        (120, 0),
    ];
    let mut group = 0;
    while group < groups.len() {
        let mut index = 0;
        while index < WEIGHT.len() {
            if WEIGHT[index] == groups[group].0 {
                groups[group].1 |= 0x8000_0000_0000_0000 >> index;
            }
            index += 1;
        }
        group += 1;
    }
    groups
};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SearchMode {
    Midgame,
    Wdl,
    Exact,
}

pub(crate) struct AIPlayer {}

impl AIPlayer {
    pub(crate) fn search(&self, board: &BitBoard) -> u64 {
        let (depth, mode) = self.parameters(board);
        self.search_with_depth(board, depth, mode).0
    }

    // Depth counts plies after the root move, matching the game player's search.
    pub(crate) fn search_with_depth(
        &self,
        board: &BitBoard,
        depth: u32,
        mode: SearchMode,
    ) -> (u64, i32) {
        let endgame = mode != SearchMode::Midgame;
        // A narrow window proves the outcome without resolving the final margin.
        let (mut alpha, beta) = if mode == SearchMode::Wdl {
            (-1, 1)
        } else {
            (-i32::MAX, i32::MAX)
        };
        let outcome = |score: i32| {
            if mode == SearchMode::Wdl {
                score.signum()
            } else {
                score
            }
        };
        let mut moves = board.legal_moves_vec();
        if moves.is_empty() {
            return (
                0,
                outcome(self.negamax(board, false, depth.saturating_add(1), endgame, alpha, beta)),
            );
        }
        moves.shuffle(&mut rand::thread_rng());
        let mut best = -i32::MAX;
        let mut best_pos = moves[0];
        for pos in moves {
            let score = -self.negamax(&board.do_move(pos), false, depth, endgame, -beta, -alpha);
            if score > best {
                best = score;
                best_pos = pos;
            }
            alpha = alpha.max(score);
            if alpha >= beta {
                break;
            }
        }
        (best_pos, outcome(best))
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
        let mut moves = board.legal_moves();
        if moves == 0 {
            if passed {
                return self.evaluate(board, endgame);
            }
            return -self.negamax(&board.do_pass(), true, depth - 1, endgame, -beta, -alpha);
        }
        let mut alpha = alpha;
        let mut best = -i32::MAX;

        if depth == 1 || (endgame && (board.bits.0 | board.bits.1).count_zeros() == 1) {
            while moves != 0 {
                let score = -self.evaluate(&board.do_move(take_move(&mut moves)), endgame);
                if score >= beta {
                    return score;
                }
                best = best.max(score);
            }
            return best;
        }

        if moves.is_power_of_two() {
            return -self.negamax(
                &board.do_move(moves),
                false,
                depth - 1,
                endgame,
                -beta,
                -alpha,
            );
        }

        let mut children = [(0, *board); 64];
        let mut count = 0;
        while moves != 0 {
            let child = board.do_move(take_move(&mut moves));
            let priority = if endgame {
                -(child.legal_moves().count_ones() as i32)
            } else {
                -self.evaluate(&child, false)
            };
            children[count] = (priority, child);
            count += 1;
        }
        let children = &mut children[..count];
        children.sort_unstable_by(|a, b| b.0.cmp(&a.0));

        for &(_, child) in children.iter() {
            let score = -self.negamax(&child, false, depth - 1, endgame, -beta, -alpha);
            if score >= beta {
                return score;
            }
            alpha = std::cmp::max(alpha, score);
            best = std::cmp::max(best, score);
        }
        best
    }

    fn parameters(&self, board: &BitBoard) -> (u32, SearchMode) {
        match (board.bits.0 | board.bits.1).count_zeros() {
            0..=16 => (u32::MAX, SearchMode::Exact),
            17..=18 => (u32::MAX, SearchMode::Wdl),
            _ => (9, SearchMode::Midgame),
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
        let mut score = 0;
        for (weight, mask) in WEIGHT_MASKS {
            score += weight
                * ((board.bits.0 & mask).count_ones() as i32
                    - (board.bits.1 & mask).count_ones() as i32);
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

fn take_move(moves: &mut u64) -> u64 {
    let pos = 0x8000_0000_0000_0000 >> moves.leading_zeros();
    *moves ^= pos;
    pos
}

#[cfg(test)]
mod tests {
    #[test]
    fn evaluation_matches_square_weights() {
        use super::{AIPlayer, STABLE_PATTERNS, STABLE_WEIGHT, WEIGHT};
        use crate::bitboard::BitBoard;
        use rand::{rngs::StdRng, Rng, SeedableRng};

        fn reference(board: BitBoard, endgame: bool) -> i32 {
            if endgame {
                return board.bits.0.count_ones() as i32 - board.bits.1.count_ones() as i32;
            }
            if board.bits.0 == 0 {
                return -i32::MAX;
            }
            if board.bits.1 == 0 {
                return i32::MAX;
            }
            let mut score = 0;
            for (index, weight) in WEIGHT.iter().enumerate() {
                let pos = 0x8000_0000_0000_0000 >> index;
                if board.bits.0 & pos != 0 {
                    score += weight;
                } else if board.bits.1 & pos != 0 {
                    score -= weight;
                }
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
        let mut random = StdRng::seed_from_u64(0);
        for _ in 0..10_000 {
            let current = random.gen::<u64>();
            let board = BitBoard {
                bits: (current, random.gen::<u64>() & !current),
            };
            for endgame in [false, true] {
                assert_eq!(
                    AIPlayer {}.evaluate(&board, endgame),
                    reference(board, endgame)
                );
            }
        }
        for bits in [(0, 0), (0, u64::MAX), (u64::MAX, 0)] {
            for endgame in [false, true] {
                let board = BitBoard { bits };
                assert_eq!(
                    AIPlayer {}.evaluate(&board, endgame),
                    reference(board, endgame)
                );
            }
        }
    }

    #[test]
    fn chooses_a_legal_move_when_every_move_loses() {
        use super::{AIPlayer, SearchMode};
        use crate::bitboard::BitBoard;

        let board = BitBoard {
            bits: (1 << 37, (1 << 39) | (1 << 38) | (1 << 36)),
        };
        assert_eq!(
            AIPlayer {}.search_with_depth(&board, 1, SearchMode::Midgame),
            (1 << 35, -i32::MAX)
        );
    }

    #[test]
    fn search_matches_minimax() {
        use super::{AIPlayer, SearchMode};
        use crate::bitboard::BitBoard;
        use rand::{rngs::StdRng, Rng, SeedableRng};

        fn minimax(player: &AIPlayer, board: BitBoard, depth: u32, endgame: bool) -> i32 {
            if depth == 0 || board.game_over() {
                return player.evaluate(&board, endgame);
            }
            let moves = board.legal_moves_vec();
            if moves.is_empty() {
                return -minimax(player, board.do_pass(), depth - 1, endgame);
            }
            moves
                .into_iter()
                .map(|pos| -minimax(player, board.do_move(pos), depth - 1, endgame))
                .max()
                .unwrap()
        }

        let player = AIPlayer {};
        let mut random = StdRng::seed_from_u64(42);
        let mut passes = 0;
        let mut outcomes = [false; 3];
        let mut endgame_passes = 0;
        for _ in 0..16 {
            let mut board = BitBoard::new();
            let mut turn = 0;
            loop {
                let moves = board.legal_moves_vec();
                let endgame = (board.bits.0 | board.bits.1).count_zeros() <= 5;
                let depth = if endgame { 12 } else { 2 };
                if turn % 8 == 0 || endgame || moves.is_empty() {
                    let expected = minimax(&player, board, depth + 1, endgame);
                    let mode = if endgame {
                        SearchMode::Exact
                    } else {
                        SearchMode::Midgame
                    };
                    let (pos, score) = player.search_with_depth(&board, depth, mode);
                    if endgame {
                        outcomes[(expected.signum() + 1) as usize] = true;
                        if moves.is_empty() && !board.game_over() {
                            endgame_passes += 1;
                        }
                        let (pos, score) =
                            player.search_with_depth(&board, u32::MAX, SearchMode::Wdl);
                        assert_eq!(score, expected.signum(), "board={board:?}");
                        if moves.is_empty() {
                            assert_eq!(pos, 0);
                        } else {
                            assert!(moves.contains(&pos));
                            assert_eq!(
                                (-minimax(&player, board.do_move(pos), depth, true)).signum(),
                                expected.signum()
                            );
                        }
                    }
                    assert_eq!(score, expected, "board={board:?}, depth={depth}");
                    if moves.is_empty() {
                        assert_eq!(pos, 0);
                    } else {
                        assert!(moves.contains(&pos), "board={board:?}, pos={pos:#x}");
                        assert_eq!(
                            -minimax(&player, board.do_move(pos), depth, endgame),
                            expected
                        );
                    }
                }
                if board.game_over() {
                    break;
                }
                if moves.is_empty() {
                    passes += 1;
                    let exact = minimax(&player, board, 4, false);
                    if exact.abs() < i32::MAX - 2 {
                        for (alpha, beta) in [
                            (exact - 1, exact + 1),
                            (exact - 2, exact - 1),
                            (exact + 1, exact + 2),
                        ] {
                            let result = player.negamax(&board, false, 4, false, alpha, beta);
                            if exact <= alpha {
                                assert!(result <= alpha);
                            } else if exact >= beta {
                                assert!(result >= beta);
                            } else {
                                assert_eq!(result, exact);
                            }
                        }
                    }
                    board = board.do_pass();
                } else {
                    board = board.do_move(moves[random.gen_range(0..moves.len())]);
                }
                turn += 1;
            }
        }
        assert!(passes > 0);
        assert!(endgame_passes > 0);
        assert!(outcomes.into_iter().all(|seen| seen));
    }
}
