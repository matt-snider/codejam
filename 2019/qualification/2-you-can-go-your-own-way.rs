//! GCJ 2019, Qualification: Problem 2 - You Can Go Your Own Way
//!
//! By Matt Snider, 2019-04-06

use std::io;
use std::io::BufRead;


fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(|l| l.unwrap());
    let t: i32 = lines.next().unwrap().parse().unwrap();

    for i in 1..(t + 1) {
        let n: i32 = lines.next().unwrap().parse().unwrap();
        let p = lines.next().unwrap();
        let y = solve(n, p);
        println!("Case #{}: {}", i, y);
    }
}


/// Given a maze of dimensions `n` by `n` and Lydia's path `p`, find another
/// path to from the start (northwest corner) to the finish (southwest corner).
///
/// Approach: we can simply invert Lydia's path, by switching S -> E and E -> S.
///
/// * Since the maze is equal height and width, there is no risk here of leaving
/// the maze, because the count of all S's will equal the count of all E's.
/// * A naive solution would be to move N-times in the S direction, and then N-times
/// in the E direction. The problem with this is, it does not take it into account
/// the restriction that we don't ever follow Lydia's path.
/// * By inverting the path we won't ever follow it.
fn solve(_n: i32, p: String) -> String {
    p.chars().map(|c| match c {
        'E' => 'S',
        'S' => 'E',
        _   => unreachable!(),
    }).collect()
}

