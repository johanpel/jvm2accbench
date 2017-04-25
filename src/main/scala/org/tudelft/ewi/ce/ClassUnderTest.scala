import java.nio.IntBuffer

import sun.misc.Unsafe

/**
  * This abstract class will be implemented by the generated top-level object class
 */
abstract class ClassUnderTest {
  /**
    * Serialize the object data to a ByteBuffer
    * @param bb The bytebuffer to serialize to
    */
  def writeByteBufferManual(bb : IntBuffer) : Unit

  /**
    * Serialize the object data using the [[sun.misc.Unsafe]] library.
    * @param address The address of the memory region to serialize to
    * @param UnsafeInstance An [[Unsafe]] instance
    */
  def writeUnsafeManual(address : Long, UnsafeInstance : Unsafe) : Unit
}

/**
  * Instantiator class, will be implemented by generated code
  */
abstract class Instantiator {
  val root : Any
}