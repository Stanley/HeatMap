import java.awt.Color
import scala.io.Source
import scala.xml.XML._
import scala.xml.NodeBuffer
import scala.xml.dtd.{DocType, PublicID}

object HeatMap {
  
  // Image width in pixels
  val width: Int = 1000
  // Avg. speed
  val speed: Int = 5
  // Time limit in hours
  val limit: Float = 0.25f   
  // Distance in kilometers
  val dist: Float = speed * limit    
  
  // Zamienia liczbę z przedziału 0-100 na odpowiadający mu kolor w zefiniowanym gardiencie
  def color(percent: Int): Color = {		
  
    val decr: Double = if(percent % 33 == 0) 1 else percent % 33 / 33.0
    val incr: Double = 1 - decr
    
	percent match {
      case x if 0 to 33 contains x =>
        new Color(0, (255*decr).toInt, (255).toInt)
      case x if 34 to 66 contains x =>				
        new Color((255*decr).toInt, (255).toInt, (255*incr).toInt)
      case x if 67 to 99 contains x =>
        new Color(255, (255*incr).toInt, 0)
      case 100 =>
        new Color(255, 0, 0)
      case _ =>
        new Color(0, 0, 255)			  
      }    
  }
  
  def main(args: Array[String]) {
				
    // Read file with initial data 
    val source = Source.fromFile("../data/AWF.input")
  
    // Convert numbers from file to List (of List of 3 params: x, y, z)
    val array: List[List[Float]] = List.fromString(source.mkString, '\n').map(List.fromString(_, ' ').map(_.toFloat))
        
    // Wspólna wielkość dla wszystkich elips
//    val map: Element = document.createElement("map")    
//    map.setAttribute("rx", (geo.longitude_to_pixels(dist)/2).toString)
//    map.setAttribute("ry", (geo.latitude_to_pixels(dist)/2).toString)
//    root.appendChild(map)
                                                  
                                                    		
    // Declare the doctype of XML document
    val doctype = DocType("svg", PublicID("-//W3C//DTD SVG 1.0//EN", "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd"), Nil)
    
    // Create root node
    val root = <svg>{ list(array) }</svg>      		
      
    saveFull("filename", root, "UTF-8", true, doctype)
              
  }
  
  // Sort the list so more important nodes will be on top of the image
  def list(data: List[List[Float]]): NodeBuffer = {
    
    // Find min and max value of latitude and longitude
    val lats: List[Float] = data.map( row => row(0))
    val w: Float = lats.reduceLeft(Math.min(_,_))
    val e: Float = lats.reduceLeft(Math.max(_,_))

    val lngs: List[Float] = data.map( row => row(1))
    val s: Float = lngs.reduceLeft(Math.min(_,_))
    val n: Float = lngs.reduceLeft(Math.max(_,_))

    val factor: Float = this.width / (e-w)
    val geo = new Geo(w,s)
    
    val result = new NodeBuffer
    data.sort(_(2) > _(2)).foreach{ row =>
      result &+ (<elipse
                   cx={ ((row(0)-w)*factor).toString } 
                   cy={ ((row(1)-s)*factor).toString }                   
                 />)
      
//      paint(svgGenerator, (row(0)-w)*factor, (row(1)-s)*factor, (100 - row(2)).toInt, geo)
      
//      ellipse.setAttribute("rx", (geo.longitude_to_pixels(dist)/2).toString)
//      ellipse.setAttribute("ry", (geo.latitude_to_pixels(dist)/2).toString)                                     
    }       
    result
  }
}

class Geo(w: Float, s: Float) {
    
  val longitude_factor: Float = length(w, s, w, s+1).toFloat
  val latitude_factor:  Float = length(w, s, w+1, s).toFloat
  
  // Returns length in kilometers from and to given points
  // Takes two points
  def length(p1_lng: Double, p1_lat: Double, p2_lng: Double, p2_lat: Double): Double = {
    val r = 6371
    val to_rad = 3.142 / 180

    val d_lat = (p2_lat - p1_lat) * to_rad
    val d_lng = (p2_lng - p1_lng) * to_rad

    val a = Math.sin(d_lat / 2) * Math.sin(d_lat / 2) +
    Math.cos(p1_lat * to_rad) * Math.cos(p2_lat * to_rad) *
    Math.sin(d_lng / 2) * Math.sin(d_lng / 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    r * c
  }
  
  // Returns number of pixels which are required to draw horizontal line
  // Takes line lenght in kilometers
  def longitude_to_pixels(lenght: Float): Int = {
    Math.round(lenght * longitude_factor)
  }
  
  // Returns number of pixels which are required to draw vertical line
  // Takes line lenght in kilometers
  def latitude_to_pixels(lenght: Float): Int = {
    Math.round(lenght * latitude_factor)
  }
}