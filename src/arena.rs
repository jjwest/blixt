use std::cmp;
use std::mem;
use std::ops::{Index, IndexMut};

#[derive(Copy, Clone, Debug)]
pub struct Id {
    arena: u32,
    id: u32,
}

pub struct Arena<T> {
    current_block: usize,
    blocks: Vec<Vec<T>>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Arena::with_block_size(1024 * 1024)
    }
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Arena::default()
    }

    pub fn with_block_size(size: usize) -> Self {
        let type_size = cmp::max(1, mem::size_of::<T>());
        Arena {
            current_block: 0,
            blocks: vec![Vec::with_capacity(size / type_size)],
        }
    }

    pub fn alloc(&mut self, elem: T) -> Id {
        if self.blocks.is_empty() {
            self.blocks[0].push(elem);
            return Id { arena: 0, id: 0 };
        }

        if self.blocks.len() == self.blocks.capacity() {
            self.blocks.push(Vec::with_capacity(
                self.blocks[self.current_block].capacity(),
            ));
            self.current_block += 1;
        }

        let block = &mut self.blocks[self.current_block];
        let id = Id {
            arena: self.current_block as u32,
            id: block.len() as u32,
        };

        block.push(elem);

        id
    }
}

impl<T> Index<Id> for Arena<T> {
    type Output = T;
    fn index(&self, idx: Id) -> &T {
        &self.blocks[idx.arena as usize][idx.id as usize]
    }
}

impl<T> IndexMut<Id> for Arena<T> {
    fn index_mut(&mut self, idx: Id) -> &mut T {
        &mut self.blocks[idx.arena as usize][idx.id as usize]
    }
}
