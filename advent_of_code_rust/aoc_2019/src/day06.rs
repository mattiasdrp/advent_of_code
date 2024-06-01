use aoc_utils::file_utils::read_lines;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::Path,
};

fn bfs(root: &str, exit: Option<&str>, map: HashMap<String, Vec<String>>) -> usize {
    let mut queue: VecDeque<(&str, usize)> = VecDeque::new();
    queue.push_back((root, 0));
    let mut visited = HashSet::new();
    visited.insert(root.to_string());
    let mut total = 0;
    while let Some((node, depth)) = queue.pop_front() {
        visited.insert(node.to_string());
        if exit == Some(node) {
            return depth - 2;
        };
        total += depth;
        if let Some(vec) = map.get(node) {
            vec.iter().for_each(|node| {
                if !visited.contains(node) {
                    queue.push_back((node, depth + 1))
                }
            })
        }
    }
    total
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let mut map: HashMap<String, Vec<String>> = HashMap::new();
    read_lines(filename)
        .unwrap()
        .map(|line| line.unwrap())
        .for_each(|line| {
            if let [from, to] = line.trim().split(')').collect::<Vec<_>>()[..] {
                match map.get_mut(from) {
                    Some(ref mut vec) => vec.push(to.to_string()),
                    None => {
                        let _ = map.insert(from.to_string(), vec![to.to_string()]);
                    }
                };
                match map.get_mut(to) {
                    Some(ref mut vec) => vec.push(from.to_string()),
                    None => {
                        let _ = map.insert(to.to_string(), vec![from.to_string()]);
                    }
                };
            } else {
                panic!("bad representation of an orbit");
            }
        });
    (if part == 1 {
        bfs("COM", None, map)
    } else {
        bfs("SAN", Some("YOU"), map)
    })
    .try_into()
    .unwrap()
}
