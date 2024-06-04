pub struct VecPermutation<T> {
    vec: Vec<T>,
    i: usize,
    length: usize,
    cpt: Vec<usize>,
    first: bool,
}

impl<T> From<Vec<T>> for VecPermutation<T> {
    fn from(item: Vec<T>) -> Self {
        let length = item.len();
        VecPermutation {
            vec: item,
            i: 0,
            length,
            cpt: vec![0; length],
            first: true,
        }
    }
}

impl<T> Iterator for VecPermutation<T>
where
    T: Copy,
{
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.first {
                self.first = false;
                return Some(self.vec.clone());
            } else if self.i < self.length {
                if self.cpt[self.i] < self.i {
                    if self.i % 2 == 0 {
                        self.vec.swap(0, self.i)
                    } else {
                        self.vec.swap(self.cpt[self.i], self.i)
                    };
                    self.cpt[self.i] += 1;
                    self.i = 0;
                    return Some(self.vec.clone());
                } else {
                    self.cpt[self.i] = 0;
                    self.i += 1;
                }
            } else {
                return None;
            }
        }
    }
}

pub fn combinations<T>(vec: &[T]) -> Vec<(T, T)>
where
    T: Clone,
{
    let mut res = Vec::new();
    for (i, el1) in vec.iter().enumerate() {
        for el2 in vec[i + 1..].iter() {
            res.push((el1.clone(), el2.clone()))
        }
    }
    res
}
