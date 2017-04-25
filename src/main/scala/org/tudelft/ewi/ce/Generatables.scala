/**
  * A reference
  * @param ref The reference number
  * @param level At which level of the tree
  * @param typeRef What type
  */
class gRef(val ref: Int, val level: Int = 0, val typeRef : Int)

/**
  * An object
  * @param clazz Class of the object
  * @param level At which level of the tree
  */
class gObject(val clazz: gClass, level: Int = 0) extends gRef(ObjectRef.next(), level, clazz.typeRef) {
  var primitives: Array[gPrimitive] = Array.ofDim[gPrimitive](clazz.primitives)
  var references: Array[gObject] = Array.ofDim[gObject](clazz.references)
  var arrays: Array[gPrimitiveArray] = Array.ofDim[gPrimitiveArray](clazz.arrays)
  var reffays: Array[gReferenceArray] = Array.ofDim[gReferenceArray](clazz.reffays)

  override def toString: String = {
    var str: String = ""
    str += s"Object $ref, Class ${clazz.typeRef}, Level: $level\n"
    for (p <- primitives) {
      str += TreeTabs(level)
      str += p
    }
    for (r <- references) {
      str += TreeTabs(level)
      if (r != null)
        str += r
      else
        str += s"r —> (null)\n"
    }
    for (a <- arrays) {
      str += TreeTabs(level)
      if (a != null)
        str += a
      else
        str += s"a —> (null)\n"
    }
    for (ra <- reffays) {
      str += TreeTabs(level)
      if (ra != null)
        str += ra
      else
        str += s"ra —> (null)\n"
    }
    str
  }
}

/**
  * A primitive
  * @param fieldNum Field number
  */
class gPrimitive(val fieldNum: Int) {
  override def toString: String = {
    s"P\n"
  }
}

/**
  * An array of primitives
  * @param size The number of primitives
  * @param level At which level of the tree
  */
class gPrimitiveArray(val size: Int, level: Int = 0) extends gRef(ObjectRef.next(), level, 0) {
  override def toString: String = {
    s"A[$size]\n"
  }
}

/**
  * An array of references
  * @param ref The reference number
  * @param size How many references the array itself has
  * @param level At which level of the tree
  */
class gReferenceArray(ref: Int, size: Int, level: Int = 0) extends gRef(ObjectRef.next(), level, 0) {
  var references: Array[gRef] = Array.ofDim[gRef](size)

  override def toString: String = {
    var str = ""
    for (r <- references) {
      str += TreeTabs(level)
      if (r != null)
        str += r
      else
        str += s"r —> (null)\n"
    }
    str
  }
}

/**
  * A class
  * @param typeRef Type of each of the references
  * @param primitives Number of primitives
  * @param arrays Number of arrays of primitives
  * @param references Number of references
  * @param reffays Number of of arrays of references
  * @param level At which level of the object tree does this class reside
  */
class gClass(val typeRef: Int, val primitives: Int, val arrays: Int, val references: Int, val reffays: Int, val level: Int = 0) {

  def params : Int = primitives + arrays + references + reffays

  var refTypes : Array[Int] = Array.ofDim[Int](references)
  var reffayType : Int = 0

  override def toString: String = {
    s"Class Type Ref.: $typeRef, Primitives: $primitives Arrays:$arrays References:$references Ref. Arrays:$reffays Level:$level\n"
  }
}
