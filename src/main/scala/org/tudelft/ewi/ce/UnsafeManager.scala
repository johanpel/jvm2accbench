import sun.misc.Unsafe

object UnsafeManager {
  /**
    * Create a new instance of [[sun.misc.Unsafe]]
    * @return The instance
    */
  def getUnsafeInstance: sun.misc.Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe")
    f.setAccessible(true)
    val unsafe = f.get(null).asInstanceOf[Unsafe]
    unsafe
  }
}