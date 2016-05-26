
module Block exposing (Block,inside,separated,collided,hit_wall)

type alias Block =
    { top : Float
    , left : Float
    , right : Float
    , bot : Float
    }

inside block (x,y) =
    block.left <= x && x <= block.right
        && block.bot <= y && y <= block.top

separated block1 block2 =
    block1.bot > block2.top || block1.top < block2.bot
        || block1.right < block2.left || block1.left > block2.right

collided block = not << separated block

hit_wall blocks r (x,y) =
    let
        block = { top = y+r
                , left = x-r
                , right = x+r
                , bot = y-r
                }
    in
        List.any (collided block) blocks
    
