module Teste2023 where


data B_tree a = Nil | Block { leftmost::B_tree a, block::[(a, B_tree)] }

t = Block {
    leftmost = Block { leftmost = Nil, block = [(1, Nil), (2, Nil), (5, Nil), (6, Nil)] },
    block = [
        (7, Block { leftmost = Nil, block = [(9, Nil), (12, Nil)] }),
        (16, Block { leftmost = Nil, block = [(18, Nil), (21, Nil)] })
    ]
}