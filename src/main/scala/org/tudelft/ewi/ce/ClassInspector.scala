import java.lang.reflect.Modifier

import scala.reflect.ClassTag

object ClassInspector {

  def refOrPrimSize[T : ClassTag](t : Class[_]) : Int = {
    t.getTypeName match {
      case "boolean"  => 1
      case "short"    => 2
      case "char"     => 2
      case "int"      => 4
      case "float"    => 4
      case "long"     => 8
      case "double"   => 8
      case _ => -1
    }
  }

  def copyScheme[T : ClassTag](clazz : Class[T], level : Int = 0) : Unit = {
    if (level == 0) println("--- Copy scheme for " + clazz.getName)
    val UNSAFE = UnsafeManager.getUnsafeInstance
    // Fully qualified class
    if (!clazz.isPrimitive && !clazz.isArray) {
      val fields = clazz.getDeclaredFields
      val offsets = fields.map(f => UNSAFE.objectFieldOffset(f))
      val types = fields.map(f => f.getType)
      val prims = types.map(t => t.isPrimitive)
      val arrays = types.map(t => t.isArray)
      val sizes = types.map(t => refOrPrimSize(t))
      val skip = fields.map(f => Modifier.isTransient(f.getModifiers))
      // Primitives

      (0 until fields.length).foreach(i => {
        if (!skip(i) && prims(i) && !arrays(i)) {
          (0 until level).foreach(i => print("\t"))
          println("Copy (" + sizes(i) + ") from [" + offsets(i) + "]\tprimitive: " + types(i).getName + " " + fields(i).getName)
        }
      })
      // Objects
      (0 until fields.length).foreach(i => {
        if (!skip(i) && !prims(i) && !arrays(i)) {
          (0 until level).foreach(i => print("\t"))
          println("Object copy    : " + fields(i).getName)
          copyScheme(fields(i).getType, level + 1)
        }
      })
      // Arrays
      (0 until fields.length).foreach(i => {
        if (!skip(i) && arrays(i)) {
          (0 until level).foreach(i => print("\t"))
          println("Copy array     : ")
          copyScheme(fields(i).getType, level + 1)
        }
      })
    }
    else if (clazz.isArray) {
      (0 until level).foreach(i => print("\t"))
      println("Read arraySize")
      val comp = clazz.getComponentType
      if (!comp.isPrimitive) {
        (0 until level).foreach(i => print("\t"))
        println("Copy (arraySize) non-primitive elements:" + comp.getName)
        copyScheme(comp)
      }
      else {
        val off = UNSAFE.arrayBaseOffset(clazz)
        (0 until level).foreach(i => print("\t"))
        println("Copy (arraySize x " + refOrPrimSize(comp) + ") from [" + off + "] primitive " + comp.getName + "s")
      }
    }
    else {
      (0 until level).foreach(i => print("\t"))
      println("PRIMITIVE")
    }
  }
}
