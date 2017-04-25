import scala.collection.mutable.ListBuffer
import scala.util.Random

object TreeTabs {
  def apply(amount: Int): String = {
    var str = ""
    (0 until amount).foreach(i => str += "─ ")
    str += "─> "
    str
  }
}

object Tabs {
  def apply(amount: Int): String = {
    var str = ""
    (0 until amount).foreach(i => str += "  ")
    str
  }
}

/**
  * Generates Scala code from an object tree
  */
object ScalaCodeGenerator {

  val definedClasses : ListBuffer[Int] = new ListBuffer[Int]()

  var prim = "Int"
  var primSize = 4

  /**
    * Change the type of the primiitve (unused)
    * @param name The name of the primitive type in Scala
    * @param size The size of the primitive type in native bytes
    */
  def setPrim(name : String, size : Int) : Unit = {
    prim = name
    primSize = size
  }

  /**
    * Generate the code for the classes of the objects in the tree
    * @return The code
    */
  def generateClassCode : String = {

    var s: String = ""
    s += "import sun.misc.Unsafe\n"
    s += "import java.nio.ByteBuffer\n"
    s += "import java.nio.IntBuffer\n"
    s += "import scala.util.Random\n"
    s += "\n"

    for (c <- ClassGenerator.classes) {
      if (c.typeRef != 0) {
        s += s"class C${c.typeRef}("
        for (p <- 0 until c.primitives) {
          s += "val p" + p + s" : $prim, "

        }
        for (r <- 0 until c.references) {
          s += s"val r$r : C${c.refTypes(r)}, "
        }
        for (a <- 0 until c.arrays) {
          s += s"val a$a : Array[$prim], "
        }
        for (ra <- 0 until c.reffays) {
          s += s"val ra$ra : Array[C${c.reffayType}], "
        }
        // Remove last ,_
        if (c.params > 0)
          s = s.dropRight(2)
        s += ") "

        if (c.level == 0) {
          s += s"extends ClassUnderTest {"
          s += "\n" + generateWriteByteBufferManual("", "", ClassGenerator.get(1))
          s += "\n" + generateWriteUnsafeManual("","", ClassGenerator.get(1))
          s += "}"
        }

        s += "\n\n"
      }
    }
    s
  }

  /**
    * Recursively generate the [[Instantiator]] class code
    * @param str The string to append the code to
    * @param obj The object for which to generate the code
    * @return The code
    */
  def generateInstantiation(str: String, obj : gObject) : String = {
    var s : String = str
    val clazz = obj.clazz
    if (obj.level == 0) {

      s += "class GeneratedInstantiator extends Instantiator {\n"
      s += s"  Random.setSeed(0L)\n"
      s += s"  override val root = new C${clazz.typeRef}(\n"
    }
    else {
      s += Tabs(obj.level+1)
      s += s"new C${clazz.typeRef}(\n"
    }

    for (p <- 0 until clazz.primitives) {
      s += Tabs(obj.level+3)
      s += s"Random.next$prim,\n"
      //s += s"125,\n"
      //s += s"0.125f,\n"
    }
    for (a <- 0 until clazz.arrays) {
      s += Tabs(obj.level+3)
      s += s"Array.fill(${obj.arrays(a).size}){Random.next$prim},\n"
    }
    for (r <- 0 until clazz.references) {
      s = generateInstantiation(s, obj.references(r))
    }
    // TODO: reference arrays

    // Remove ,_
    s = s.dropRight(2)

    if (obj.level == 0) {
      s += "\n"
      s += Tabs(obj.level)
      s += "  )\n}\n"
    }
    else {
      s += "\n"
      s += Tabs(obj.level+1)
      s += "),\n"
    }
    s
  }

  /**
    * Recursively generate the code to serialize using ByteBuffers
    * @param str The string to append the code to
    * @param su The superclass of the current class this method call is working on
    * @param clazz The current class this method call is working on
    * @return The code
    */
  def generateWriteByteBufferManual(str: String, su: String, clazz: gClass): String = {
    var s: String = str

    if (clazz.level == 0) {
      s += s"  def writeByteBufferManual(bb : ${prim}Buffer) : Unit = {\n    "
    }

    // Primitives
    for (p <- 0 until clazz.primitives) {
      s += s"    bb.put(${su}p$p)\n"
    }

    // Arrays
    for (a <- 0 until clazz.arrays) {
      s += s"    bb.put(${su}a$a)\n"
    }

    // References
    for (r <- 0 until clazz.references) {
      s = generateWriteByteBufferManual(s, s"${su}r$r.", ClassGenerator.get(clazz.refTypes(r)))
    }

    if (clazz.level == 0) {
      s += "  }\n"
    }
    s
  }

  /**
    * Recursively generate the code to serialize using the [[sun.misc.Unsafe]] library
    * @param str The string to append the code to
    * @param su The superclass of the current class this method call is working on
    * @param clazz The current class this method call is working on
    * @return The code
    */
  def generateWriteUnsafeManual(str: String, su: String, clazz: gClass): String = {
    var s: String = str

    if (clazz.level == 0) {
      s += "  def writeUnsafeManual(address : Long, UnsafeInstance : Unsafe) : Unit = {\n"
      s += "    var addr = address\n"
      s += s"    val offset = UnsafeInstance.arrayBaseOffset(classOf[Array[$prim]])\n"
    }

    // Primitives
    for (p <- 0 until clazz.primitives) {
      s += s"    UnsafeInstance.put$prim(addr, ${su}p$p)\n"
      s += s"    addr += $primSize\n"
    }

    // Arrays
    for (a <- 0 until clazz.arrays) {
      //s += s"    offset = UnsafeInstance.arrayBaseOffset(${su}a$a.getClass)\n"
      s += s"    UnsafeInstance.copyMemory(${su}a$a, offset, null, addr, $primSize * ${su}a$a.length)\n"
      s += s"    addr += $primSize * ${su}a$a.length\n"
    }

    // References
    for (r <- 0 until clazz.references) {
      s = generateWriteUnsafeManual(s, s"${su}r$r.", ClassGenerator.get(clazz.refTypes(r)))
    }

    if (clazz.level == 0) {
      s += "  }\n"
    }
    s
  }
}