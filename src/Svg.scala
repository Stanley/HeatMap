import scala.io.Source
import scala.xml.dtd.{DocType, PublicID}
import scala.xml.XML._

object Svg {
  def main(args: Array[String]) {
				
    // Read file with initial data 
    val map = new HeatMap(Source.fromFile("../data/AWF.input"))                                                  
                                                    		
    // Declare the doctype of XML document
    val doctype = DocType("svg", PublicID("-//W3C//DTD SVG 1.0//EN", "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd"), Nil)
    
    // Create root node
    val root =       
      <svg xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="http://www.w3.org/2000/svg" width={ map.geo.image_width + "px" } height={ map.geo.image_height + "px" } viewBox={ map.geo.viewbox }>
        <defs>
          { map.gradients }
        </defs>
        <g opacity=".5">
          { map.circles }
          <g id="outline" opacity=".8">
           { map.polygon(30) }
          </g>
        </g>   
        
      </svg>                  
      
    print("Zapisuję mapę do pliku: svg/out.svg \n")
    saveFull("svg/out.svg", root, "UTF-8", true, doctype)
    print("Gotowe\n")
              
  }
}
