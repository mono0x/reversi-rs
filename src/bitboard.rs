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
pub(crate) struct BitBoard {
    // (current (black), opponent (white))
    pub(crate) bits: (u64, u64), // MSB <- a1, b1, ... h1, a2, b2, ... , h8 -> LSB
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
        let mut mask = self.legal_moves();
        let mut moves = Vec::with_capacity(mask.count_ones() as usize);
        while mask != 0 {
            // Preserve the existing a1-to-h8 move order.
            let pos = 0x8000_0000_0000_0000 >> mask.leading_zeros();
            moves.push(pos);
            mask ^= pos;
        }
        moves
    }

    pub fn do_move(&self, pos: u64) -> Self {
        assert_eq!(pos.count_ones(), 1);
        assert_eq!((self.bits.0 | self.bits.1) & pos, 0);

        #[inline]
        const fn flip_bits_dir(black: u64, white: u64, pos: u64, shift: u32, mask: u64) -> u64 {
            let mask = white & mask; // apply mask to prevent overflow by edge of board

            let left = adjacents!(pos, mask, shift_l, shift);
            let right = adjacents!(pos, mask, shift_r, shift);
            // A run flips only when its far end is bounded by our own disc.
            let left = if shift_l(left, shift) & black != 0 {
                left
            } else {
                0
            };
            let right = if shift_r(right, shift) & black != 0 {
                right
            } else {
                0
            };
            left | right
        }

        let mut flip = 0;
        for (shift, mask) in Self::SHIFT_AND_MASKS {
            flip |= flip_bits_dir(self.bits.0, self.bits.1, pos, shift, mask);
        }
        assert_ne!(flip, 0);
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
        Self {
            bits: (self.bits.1, self.bits.0),
        }
        .legal_moves()
            == 0
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

    // Vertical overflow is discarded by the shift itself; only file wrap needs masking.
    const SHIFT_AND_MASKS: [(u32, u64); 4] = [
        // horizontal
        (1, Self::HORIZONTAL_MASK),
        // vertical
        (8, u64::MAX),
        // digonal
        (7, Self::HORIZONTAL_MASK),
        (9, Self::HORIZONTAL_MASK),
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

#[cfg(test)]
mod tests {

    #[test]
    fn operations_match_reference() {
        use super::BitBoard;
        use rand::{rngs::StdRng, Rng, SeedableRng};

        fn reference_flips(board: BitBoard, index: u32) -> u64 {
            let pos = 1u64 << index;
            if (board.bits.0 | board.bits.1) & pos != 0 {
                return 0;
            }
            let mut flips = 0;
            for (dx, dy) in [
                (-1, -1),
                (0, -1),
                (1, -1),
                (-1, 0),
                (1, 0),
                (-1, 1),
                (0, 1),
                (1, 1),
            ] {
                let (mut x, mut y) = ((index % 8) as i32 + dx, (index / 8) as i32 + dy);
                let mut line = 0;
                while (0..8).contains(&x) && (0..8).contains(&y) {
                    let bit = 1u64 << (y * 8 + x);
                    if board.bits.1 & bit != 0 {
                        line |= bit;
                    } else {
                        if board.bits.0 & bit != 0 {
                            flips |= line;
                        }
                        break;
                    }
                    x += dx;
                    y += dy;
                }
            }
            flips
        }

        fn check_board(board: BitBoard) {
            let mut expected = Vec::new();
            for index in (0..64).rev() {
                let flips = reference_flips(board, index);
                if flips == 0 {
                    continue;
                }
                let pos = 1u64 << index;
                expected.push(pos);
                assert_eq!(
                    board.do_move(pos).bits,
                    (board.bits.1 ^ flips, board.bits.0 | flips | pos)
                );
            }
            assert_eq!(
                board.legal_moves(),
                expected.iter().fold(0, |mask, pos| mask | pos)
            );
            assert_eq!(board.legal_moves_vec(), expected);
            let opponent = BitBoard {
                bits: (board.bits.1, board.bits.0),
            };
            let opponent_can_move = (0..64).any(|index| reference_flips(opponent, index) != 0);
            assert_eq!(board.game_over(), expected.is_empty() && !opponent_can_move);
            if expected.is_empty() {
                assert_eq!(board.do_pass(), opponent);
            }
        }

        // Exercise every straight six-disc capture, including board edges.
        for start in 0..64 {
            for (dx, dy) in [
                (-1, -1),
                (0, -1),
                (1, -1),
                (-1, 0),
                (1, 0),
                (-1, 1),
                (0, 1),
                (1, 1),
            ] {
                let (x, y) = (start % 8, start / 8);
                if !(0..8).contains(&(x + 7 * dx)) || !(0..8).contains(&(y + 7 * dy)) {
                    continue;
                }
                let opponent = (1..7).fold(0, |mask, step| {
                    mask | (1u64 << ((y + step * dy) * 8 + x + step * dx))
                });
                check_board(BitBoard {
                    bits: (1u64 << start, opponent),
                });
            }
        }
        let mut random = StdRng::seed_from_u64(0);
        for _ in 0..2_000 {
            let current = random.gen::<u64>();
            let opponent = random.gen::<u64>() & !current;
            check_board(BitBoard {
                bits: (current, opponent),
            });
        }
        for _ in 0..64 {
            let mut board = BitBoard::new();
            loop {
                check_board(board);
                if board.game_over() {
                    break;
                }
                let moves = board.legal_moves_vec();
                board = if moves.is_empty() {
                    board.do_pass()
                } else {
                    board.do_move(moves[random.gen_range(0..moves.len())])
                };
            }
        }
        for bits in [(0, 0), (u64::MAX, 0), (0, u64::MAX)] {
            check_board(BitBoard { bits });
        }
    }

    #[test]
    fn invalid_moves_are_rejected() {
        use super::BitBoard;
        let board = BitBoard::new();
        for pos in [
            0,
            u64::MAX,
            1,
            board.bits.0 & board.bits.0.wrapping_neg(),
            board.bits.1 & board.bits.1.wrapping_neg(),
        ] {
            assert!(std::panic::catch_unwind(|| board.do_move(pos)).is_err());
        }
        assert!(std::panic::catch_unwind(|| board.do_pass()).is_err());
    }
}
