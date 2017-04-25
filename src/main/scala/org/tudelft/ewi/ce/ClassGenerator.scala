import scala.collection.mutable.ListBuffer

object ClassGenerator {

  var classes = new ListBuffer[gClass]

  // Array type:
  ClassGenerator(0,0,0,0,0)

  var ref = classes.length

  /**
    * Create a new class in the class list
    * @param primitives Number of primitives
    * @param arrays Number of arrays of primitives
    * @param references Number of references
    * @param reffays Number of arrays of references
    * @param level Level in the class tree
    * @return The new class object
    */
  def apply(primitives : Int, arrays : Int, references : Int, reffays : Int, level : Int = 0) : gClass = {
    val clazz = new gClass(ref, primitives, arrays, references, reffays, level)
    ref += 1
    classes += clazz
    clazz
  }

  /**
    * Get a class by its reference
    * @param ref The reference
    * @return The class
    */
  def get(ref: Int) : gClass = {
    classes(ref)
  }
}
