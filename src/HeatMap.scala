import scala.io.Source
import scala.xml.NodeBuffer
import com.seisw.util.geom.{Poly, PolyDefault}

class HeatMap(source: Source) {    
  
  // Convert numbers from source to List (of List of 3 params: x, y, z)
  val data: List[Point] = List.fromString(source.mkString, '\n').map{line =>
    val args:List[Float] = List.fromString(line, ' ').map(_.toFloat)
    new Point(args(0), args(1), args(2).toInt)
  }
  
  // Find min and max value of latitude and longitude
  val lats: List[Float] = data.map( row => row.lat)
  val w: Float = lats.reduceLeft(Math.min(_,_))
  val e: Float = lats.reduceLeft(Math.max(_,_))

  val lngs: List[Float] = data.map( row => row.lng)
  val s: Float = lngs.reduceLeft(Math.min(_,_))
  val n: Float = lngs.reduceLeft(Math.max(_,_))
    
  // Object which represents given Earth sector as rectangle
  val geo = new Geo(n, e, s, w, data)
                    
  // Zamienia liczbę z przedziału 0-100 na odpowiadający mu kolor w zefiniowanym gradiencie
  def color(percent: Int): String = {		
  
    val str = "%02X%02X%02X"
    val decr: Double = if(percent % 33 == 0) 1 else percent % 33 / 33.0
    val incr: Double = 1 - decr
    
	percent match {
	  case 0 =>
        str.format(255, 0, 0)
      case x if 1 to 33 contains x =>
        str.format(255, (255*decr).toInt, 0)
      case x if 34 to 66 contains x =>				
        str.format((255*incr).toInt, (255).toInt, (255*decr).toInt)
      case x if 67 to 99 contains x =>
        str.format(0, (255*incr).toInt, (255).toInt)
      case _ =>
        str.format(0, 0, 255)			  
      }    
  }
    
  // Generates 100 gradiens 
  def gradients: NodeBuffer = {
    val result = new NodeBuffer
    for(i <- 1 to 100){
      val hex: String = color(i)
      val gradient = <radialGradient id={"g_" + i}>
          <stop offset="50%" style={"stop-opacity:0.7; stop-color: #" + hex } />
          <stop offset="100%" style={"stop-opacity:0.1; stop-color: #" + hex } />
        </radialGradient>
      result &+ gradient
    }
    result
  }  
    
  def circles: NodeBuffer = {   
    
    val result = new NodeBuffer
    
    // Sort the list so more important nodes will be on top of the image
    geo.circles.sort(_.z > _.z).foreach{ circle =>        
      val c = <circle
          style={ "fill: url(#g_" + circle.z + ")"}      
          cx={ circle.x.toString } 
          cy={ circle.y.toString }  
          r={ circle.r.toString }>
        </circle>         
       
      result &+ c                                                                
    }       
    result
  }  
  
  def polygon(limit: Int): NodeBuffer = {
    
    val set = geo.circles.filter(circle => (circle.z <= limit))
    
    val result = new NodeBuffer
    var poly: Poly = new PolyDefault
    
    set.foreach(circle => poly = poly.union(circle.toPoly))
    
    Console.println("size: " + poly.getArea + " km2")
    
    var points = "M" + poly.getX(0) + " " + poly.getY(0)
    
    for(i <- 1 until poly.getNumPoints){
      points = points + " L" + poly.getX(i) + " " + poly.getY(i)
    }
    
    val polyline = <path
      d={ points + " Z" }
      style="fill:white; fill-opacity:0; stroke:white; stroke-width:0.02">
    </path>
        
    result &+ polyline        
    result
  }  
}