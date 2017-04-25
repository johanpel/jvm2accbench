import scala.util.Random

/**
  * Keeps track of all references.
  */
object ObjectRef {
  var current: Int = 0

  def reset(): Unit = current = 0

  def next(): Int = {
    current = current + 1
    current
  }
}

object ObjectGenerator {

  /**
    * Creates a leaf object
    * @param clazz Which class to use
    * @param arraySize The size of the arrays of the leaf object
    * @param level At which level of the tree
    * @return A tuple containing the leaf object and its size in bytes
    */
  def createAndFillLeafObject(clazz : gClass, arraySize : Int, level : Int = 0): (gObject, Int) = {
    var bytes = 0
    val obj = new gObject(clazz,level)
    for (p <- 0 until clazz.primitives) {
      obj.primitives(p) = new gPrimitive(p)
      bytes += 4
    }
    for (a <- 0 until clazz.arrays) {
      obj.arrays(a) = new gPrimitiveArray(arraySize, level)
      bytes += 4 * arraySize
    }
    (obj, bytes)
  }

  /**
    * Create an object with a willow style layout
    * @param classes The classes at each level
    * @param leafPrimitives The number of primitives in the leaves
    * @param leafArraySize The size of the array in the leaves
    * @param level The level at which this object is inserted in the tree
    * @return A tuple containing the willow object and its size in bytes
    */
  def createWillow(classes : IndexedSeq[gClass], leafPrimitives : Int, leafArraySize : Int, level : Int = 0) : (gObject, Int) = {
    var bytes = 0

    val levelClass = classes(level)
    if (level < classes.length-1) {
      val obj = new gObject(levelClass, level)
      for (r <- obj.references.indices) {
        val next = createWillow(classes, leafPrimitives, leafArraySize, level + 1)
        obj.references(r) = next._1
        bytes += next._2
      }
      (obj, bytes)
    }
    else {
      createAndFillLeafObject(levelClass, leafArraySize, level)
    }
  }
}