## Great Circle Distance (function from the dispeRse R package (written by Graeme T. Lloyd)
GreatCircleDistanceFromLongLat <- function(long1, lat1, long2, lat2, EarthRad = 6367.4447, Warn = TRUE) {
  
  # If latitude and longitude describe different points:
  if(all.equal(long1, long2) != TRUE || all.equal(lat1, lat2) != TRUE) {
    
    # Convert point 1 longitude to radians:
    long1 <- long1 * (pi / 180)
    # Convert point 1 latitude to radians:
    lat1 <- lat1 * (pi / 180)
    # Convert point 2 longitude to radians:
    long2 <- long2 * (pi / 180)
    # Convert point 2 latitude to radians:
    lat2 <- lat2 * (pi / 180)
    
    # Get great circle distance using cosine:
    great_circle_distance <- acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(abs(long1 - long2))) * EarthRad
    
    # If latitude and longitude describe the same point (within floating point error):
  } else {
    
    # Warn user that they may have accidentally submitted identical points:
    if(Warn) print("WARNING: points are identical within floating point error.")
    
    # Set distance to zero:
    great_circle_distance <- 0
    
  }
  
  # Return great circle distance:
  return(great_circle_distance)
  
}