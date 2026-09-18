use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

#[path = "../src/bitboard.rs"]
mod bitboard;
#[path = "../src/search.rs"]
mod search;

use bitboard::BitBoard;
use search::AIPlayer;

fn positions() -> Vec<(&'static str, BitBoard, u32, bool)> {
    let mut board = BitBoard::new();
    let mut positions = Vec::new();
    let mut played = 0;
    while !board.game_over() {
        let moves = board.legal_moves();
        if moves == 0 {
            board = board.do_pass();
            continue;
        }
        match played {
            0 => positions.push(("opening", board, 5, false)),
            20 => positions.push(("middlegame", board, 5, false)),
            36 => positions.push(("late_middlegame", board, 5, false)),
            50 => positions.push(("endgame_10", board, u32::MAX, true)),
            52 => positions.push(("endgame_8", board, u32::MAX, true)),
            _ => {}
        }
        board = board.do_move(1u64 << moves.trailing_zeros());
        played += 1;
    }
    assert_eq!(positions.len(), 5);
    positions
}

fn bench_search(c: &mut Criterion) {
    let player = AIPlayer {};
    let positions = positions();
    let mut group = c.benchmark_group("search");
    for (name, board, depth, endgame) in &positions {
        group.bench_function(*name, |b| {
            b.iter(|| {
                player.search_with_depth(black_box(board), black_box(*depth), black_box(*endgame))
            })
        });
    }
    // Exercise the actual game player's depth selection as well.
    group.bench_function("opening_default", |b| {
        b.iter(|| player.search(black_box(&positions[0].1)))
    });
    group.finish();
}

criterion_group!(benches, bench_search);
criterion_main!(benches);
