use std::{collections::HashMap, fs, path::Path};

fn resolve1(layers: HashMap<usize, &[u8]>) -> isize {
    let (layer, _) = layers.iter().fold(
        (0, usize::MAX),
        |(min_layer, min_zeros), (layer, content)| {
            let zeros = content.iter().filter(|c| **c == b'0').count();
            if zeros < min_zeros {
                (*layer, zeros)
            } else {
                (min_layer, min_zeros)
            }
        },
    );
    let content = layers.get(&layer).unwrap();
    let ones = content.iter().filter(|c| **c == b'1').count();
    let twos = content.iter().filter(|c| **c == b'2').count();
    (ones * twos) as isize
}

fn resolve2(layers: HashMap<usize, &[u8]>, width: usize, height: usize) -> isize {
    let max_layer = layers.len();
    for i in 0..(width * height) {
        if i % width == 0 {
            println!()
        }
        for layer in 1..=max_layer {
            let c = layers.get(&layer).unwrap()[i];
            if c != b'2' {
                print!("{}", if c == b'0' { "  " } else { "██" });
                break;
            };
        }
    }
    0
}

pub fn resolve<P>(part: u8, filename: P, example: bool) -> isize
where
    P: AsRef<Path> + std::fmt::Debug,
{
    let (width, height) = if example {
        if part == 1 {
            (3, 2)
        } else {
            (2, 2)
        }
    } else {
        (25, 6)
    };
    let elements = width * height;
    let string = fs::read_to_string(&filename).unwrap();
    let mut layers: HashMap<_, _> = HashMap::new();
    let mut layer = 1;
    loop {
        if layer * elements >= string.len() {
            break;
        };
        layers.insert(
            layer,
            string[(layer - 1) * elements..layer * elements].as_bytes(),
        );
        layer += 1;
    }
    if part == 1 {
        resolve1(layers)
    } else {
        resolve2(layers, width, height)
    }
}
