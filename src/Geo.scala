class Geo(n:Double, e:Double, s:Double, w:Double, points: List[Point]) {      

  // Avg. speed in km/h
  val speed: Int = 4
  // Time limit in hours
  val limit: Float = 0.25f   
  // Distance in kilometers
  val dist: Float = speed * limit
  
  // Sector width in km                                         
  val width: Double = length((n+s)/2, w, (n+s)/2, e) + 2*dist 
  // Sector height in km                                         
  val height: Double = length(n, w, s, w) + 2*dist    
  
  // Image width in pixels
  val image_width: Int = 1000
  // Image height in pixels
  val image_height: Int = (width / height * image_width).toInt
  
  val w_factor = (e - w) / width
  val h_factor = (n - s) / height
  println((e + w_factor*dist/2) +","+ (n ) +"), new GLatLng("+ (w + w_factor*dist/2) +","+ (s))
  // pixels in km      
  val factor: Double = image_width / width
  
  var circles: List[Circle] = points.map{ pkt => 
    new Circle( distance_to(pkt.lng, e) + dist, distance_to(s, pkt.lat) + dist, pkt.mag, dist) 
  }
  
  // Distance to given point in km
  def distance_to(lng:Double, lat:Double): Double = {
    length(s, e, lng, lat)
  }  
  
  // Returns: "x, y, width, height"
  def viewbox: String = {
    0 + ", " + 0 + ", " + height + ", " + width
  }
                                         
  // Returns length in kilometers from and to given points
  // Takes two points
  private def length(p1_lng: Double, p1_lat: Double, p2_lng: Double, p2_lat: Double): Double = {
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
}