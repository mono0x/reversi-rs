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
