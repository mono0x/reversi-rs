use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::hint::black_box;

#[path = "../src/bitboard.rs"]
mod bitboard;

use bitboard::BitBoard;

fn positions() -> (Vec<(&'static str, BitBoard)>, BitBoard, BitBoard) {
    let mut board = BitBoard::new();
    let mut positions = Vec::new();
    let mut pass = None;
    let mut moves_played = 0;
    loop {
        let moves = board.legal_moves();
        if moves == 0 {
            if board.game_over() {
                break;
            }
            pass.get_or_insert(board);
            board = board.do_pass();
            continue;
        }
        match moves_played {
            0 => positions.push(("opening", board)),
            24 => positions.push(("middlegame", board)),
            50 => positions.push(("endgame", board)),
            _ => {}
        }
        // Always choose the least significant legal move for reproducible positions.
        board = board.do_move(1u64 << moves.trailing_zeros());
        moves_played += 1;
    }
    assert_eq!(positions.len(), 3);
    (
        positions,
        pass.expect("the fixed game must include a pass"),
        board,
    )
}

fn bench_bitboard(c: &mut Criterion) {
    let (positions, pass, finished) = positions();
    let mut group = c.benchmark_group("bitboard");
    group.bench_function("new", |b| b.iter(|| black_box(BitBoard::new())));

    for (name, board) in &positions {
        group.bench_with_input(BenchmarkId::new("legal_moves", name), board, |b, board| {
            b.iter(|| black_box(board).legal_moves())
        });
        group.bench_with_input(
            BenchmarkId::new("legal_moves_vec", name),
            board,
            |b, board| b.iter(|| black_box(board).legal_moves_vec()),
        );
        let pos = 1u64 << board.legal_moves().trailing_zeros();
        group.bench_with_input(BenchmarkId::new("do_move", name), board, |b, board| {
            b.iter(|| black_box(board).do_move(black_box(pos)))
        });
    }

    group.bench_function("do_pass", |b| b.iter(|| black_box(&pass).do_pass()));
    for (name, board) in positions
        .iter()
        .chain([("pass", pass), ("finished", finished)].iter())
    {
        group.bench_with_input(BenchmarkId::new("game_over", name), board, |b, board| {
            b.iter(|| black_box(board).game_over())
        });
    }
    group.finish();
}

criterion_group!(benches, bench_bitboard);
criterion_main!(benches);
