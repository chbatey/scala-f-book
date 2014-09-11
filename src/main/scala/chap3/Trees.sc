import chap3.{Branch, Tree, Leaf}

val tree1 = Leaf(1)
val tree2 = Leaf(2)
val tree3 = Branch(tree1, tree2)
val tree4 = Branch(Branch(tree3, tree2), tree1)

Tree.size(tree1)
Tree.size(tree2)
Tree.size(tree3)
Tree.size(tree4)
Tree.size2(tree1)
Tree.size2(tree2)
Tree.size2(tree3)
Tree.size2(tree4)
Tree.max(tree1)
Tree.max(tree2)
Tree.max(tree3)
Tree.max(tree4)
Tree.max2(tree1)
Tree.max2(tree2)
Tree.max2(tree3)
Tree.max2(tree4)
Tree.maxDepth(tree1)
Tree.maxDepth(tree2)
Tree.maxDepth(tree3)
Tree.maxDepth(tree4)
val plusOne : (Int => Int) = a => a + 1

Tree.map(tree1)(plusOne)
Tree.map(tree2)(plusOne)
Tree.map(tree3)(plusOne)
Tree.map(tree4)(plusOne)



Tree.fold(tree1, 0)(_+_)
Tree.fold(tree2, 0)(_+_)
Tree.fold(tree3, 0)(_+_)
Tree.fold(tree4, 0)(_+_)

