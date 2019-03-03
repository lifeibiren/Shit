    
structure AST = struct

    datatype 'a tree = Lf
                    | Br of 'a * 'a tree * 'a tree
    
    fun size Lf = 0
      | size (Br(v, t1, t2)) = 1 + size t1 + size t2
    
    
    fun depth Lf = 0
      | depth (Br(v,t1,t2)) = 1 + Int.max(depth t1, depth t2)
    
    fun preorder t = 
        let 
            fun preord (Lf, vs) = vs
              | preord (Br(v, t1, t2), vs) = v :: preord(t1, preord(t2, vs))
        in
            preord (t, [])
        end
    
    fun inorder t = 
        let
            fun inord (Lf, vs) = vs
              | inord (Br(v, t1, t2), vs) = inord (t1, v::inord(t2, vs))
        in
            inord (t, [])
        end
        
    fun postorder t = 
        let
            fun postord (Lf, vs) = vs
              | postord (Br(v, t1, t2), vs) = postord (t1, postord(t2, v :: vs))
        in
            postord (t, [])
        end
end
