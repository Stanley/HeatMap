import com.seisw.util.geom.PolySimple

class Circle(x1: Double, y1: Double, z1: Int, r1: Double) {
  
  val z = if(z1 < 1) 1 else if(z1 > 100) 100 else z1
  val x = x1
  val y = y1
  val r = r1
  
  // Converts circle to a polygon
  def toPoly: PolySimple = {
    
    val count = 50 
    val result = new PolySimple    
    val step = 2 * Math.Pi / count
    
    for(i <- 1 to count){      
      val y1 = y + r * Math.cos(step*i)
      val x1 = x + r * Math.sin(step*i)      
      result.add(x1, y1)
    }    
    result
  }
}
