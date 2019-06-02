structure Ast :> AST =
struct
  structure Node =
  struct
    datatype 'a Node = Node of {
      value: 'a ref,
      child: 'a Node list ref,
      parent: 'a Node ref
    }
    type 'a t = 'a Node
    fun node value child =
      Node {value = value, child = child}
    fun child (Node {child = child, ...}) = child
    fun empty v = Node {value = v, child = nil}
    fun addChild (Node {value = pvalue, child = pchild}) newNode =
      Node {value = pvalue, child = pchild @ newNode}
  end

  datatype 'a Tree = Tree of {root: 'a Node.t}
  type 'a t = 'a Tree
  fun tree r = Tree {root = r}
  fun root (Tree {root = r}) = r
  (* fun tranverse (Tree {root = r}) = *)
end
