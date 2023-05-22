f_compute_slider = function(v, y) {
  slider_result = rep(NA, length(v))
  
  for (slider in (1):length(slider_result)) {
    #sliding_index = (slider - y + 1):slider
    sliding_index = (slider - y / 2):(slider + y /
                                                     2)
    sliding_index <-
      sliding_index[sliding_index > 0 &
                      sliding_index <= length(slider_result)]
    
    slider_result[slider] <-
      mean(v[sliding_index], na.rm = TRUE)
  }
  
  return(slider_result)
}
