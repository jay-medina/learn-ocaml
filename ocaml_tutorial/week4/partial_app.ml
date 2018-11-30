(*


Every triangle has a circumscribed circle, that is a circle that goes through the three points of a given triangle. Trigonometry tells us that the radius of this circle is s2⋅cos(a2)⋅2⋅cos(b2)⋅2⋅cos(c2) where a, b and c are the angles of the triangle, and s is its circumference.

  1. Define a function ccr: float -> float -> float -> float -> float that takes as arguments a, b, c and s, and returns the radius of circumscribed circle as described above.
    
  2. Update ccr so that it does as much work as possible when partially applied to each argument, and minimizes the total number of operations (multiplications, divisions and calls to cos).

*)

let cos_h p = 2.0 *. (cos (p /. 2.0))
                     
let ccr a = 
  let a_part = cos_h a in 
  fun b -> 
    let b_part = cos_h b *. a_part in 
    fun c -> 
      let c_part = cos_h c *. b_part in 
      fun s ->
        s /. (c_part)

