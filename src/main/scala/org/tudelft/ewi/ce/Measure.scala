object Measure {
  def time[R](name: String, block: => R,
           printName: Boolean = false,
           printUnits: Boolean = false,
           newLine: Boolean = false,
           space:String=","): R =
  {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    if (result.isInstanceOf[Byte]) print(" ") // prevent lazyness
  val t1 = System.nanoTime()
    if (printName)
      print(name)
    print(t1 - t0)
    if (printUnits)
      print(" ns")
    print(space)
    if (newLine)
      print("\n")
    result
  }

  /**
    * Measure time of some expression
    * @param block The expression
    * @tparam R
    * @return A tuple containing (the time in ns, the result of the expression)
    */
  def time[R](block: => R) : (Long , R) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (t1 - t0, result)
  }
}
