use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

#[path = "../src/bitboard.rs"]
mod bitboard;
#[path = "../src/search.rs"]
mod search;

use bitboard::BitBoard;
use search::{AIPlayer, SearchMode};

fn positions() -> Vec<(&'static str, BitBoard, u32, SearchMode)> {
    let mut board = BitBoard::new();
    let mut positions = Vec::new();
    let mut played = 0;
    while !board.game_over() {
        let moves = board.legal_moves_vec();
        if moves.is_empty() {
            board = board.do_pass();
            continue;
        }
        match played {
            0 => positions.push(("opening", board, 5, SearchMode::Midgame)),
            20 => positions.push(("middlegame", board, 5, SearchMode::Midgame)),
            36 => positions.push(("late_middlegame", board, 5, SearchMode::Midgame)),
            42 => positions.push(("wdl_18", board, u32::MAX, SearchMode::Wdl)),
            44 => {
                positions.push(("wdl_16", board, u32::MAX, SearchMode::Wdl));
                positions.push(("endgame_16", board, u32::MAX, SearchMode::Exact));
            }
            46 => positions.push(("endgame_14", board, u32::MAX, SearchMode::Exact)),
            50 => positions.push(("endgame_10", board, u32::MAX, SearchMode::Exact)),
            52 => positions.push(("endgame_8", board, u32::MAX, SearchMode::Exact)),
            _ => {}
        }
        board = board.do_move(*moves.last().unwrap());
        played += 1;
    }
    assert_eq!(positions.len(), 9);
    positions
}

fn bench_search(c: &mut Criterion) {
    let player = AIPlayer {};
    let positions = positions();
    let mut group = c.benchmark_group("search");
    for (name, board, depth, mode) in &positions {
        group.bench_function(*name, |b| {
            b.iter(|| {
                player.search_with_depth(black_box(board), black_box(*depth), black_box(*mode))
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
