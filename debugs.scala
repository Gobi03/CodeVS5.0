/*  debug tools  */
object debugs{
  // output chizu(Array[Char])
  def outputChizu(map: Array[Array[Char]]): Unit = {
    for(h <- 0 until 17){
      for(w <- 0 until 14)
        print(map(h)(w))
      println()
    }
  }
}
