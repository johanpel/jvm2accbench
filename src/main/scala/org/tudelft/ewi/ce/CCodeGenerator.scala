object CCodeGenerator {

  var prim = "int"
  var primSig = "I"
  var primSize = 4

  var addPrims = false
  var prefetch = false
  var mergeBB = false
  var avx = false
  var power8 = false

  /**
    * Set the type of the primitives
    * @param name Name of the primitive in C. Example: "int" or "float"
    * @param sig JVM signature of the primitive. Example "I" or "F"
    * @param size Size of the primitive in C bytes. Example "4" for an int or "8" for a long
    */
  def setPrim(name: String, sig: String, size: Int): Unit = {
    prim = name
    primSig = sig
    primSize = size
  }

  /**
    * Generate C header
    * @param str string to append code to
    * @return String with code appended
    */
  def generateHeader(str: String): String = {
    var s = str
    s += "#include <jni.h>\n"
    //s += "#include <assert.h>\n"
    s += "#include <stdio.h>\n"
    s += "#include <stdlib.h>\n"
    s += "#include <omp.h>\n"
    s += "#include <string.h>\n"
    if (avx) {
      s += "#include <immintrin.h>\n"
    }
    s += "\n"
    s += "JavaVM * global_vm = NULL;\n"
    s += "\n"
    s += "jint JNI_OnLoad(JavaVM *vm, void *reserved) {\n"
    s += "  global_vm = vm;\n"
    s += "  return JNI_VERSION_1_2;\n"
    s += "}\n"
    s
  }

  /**
    * Generate C code to access a single object from a normal ByteBuffer
    * @param str string to append to
    * @return String with code appended
    */
  def generateAccessObjectByteBuffer(str: String): String = {
    var s = str
    s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessObjectByteArray(JNIEnv * jni, jobject me, jobject primArray) {\n"
    s += s"  jsize len = (*jni)->GetArrayLength(jni, primArray) / $primSize;\n"
    s += s"  $prim * values = ($prim*)(*jni)->Get${prim.capitalize}ArrayElements(jni, primArray, NULL);\n"
    s += s"  $prim result = ($prim)0;\n"
    s += s"\n"
    if (addPrims) {
      s += s"  for (int i=0; i<len; i++) {\n"
      s += s"    result += values[i];\n"
      s += s"  }\n"
    }/* else if (prefetch) {
      s += s"  volatile $prim tmp;"
      s += s"  for (int i=0; i<len; i++) {\n"
      s += s"    tmp = values[i];\n"
      s += s"  }\n"
    }*/
    s += s"  (*jni)->Release${prim.capitalize}ArrayElements(jni, primArray, values, JNI_ABORT);\n"
    s += s"  return result;\n"
    s += s"}\n"
    s += s"\n"
    s
  }

  /**
    * Generate C code to access a collection of objects from normal ByteBuffers
    * @param str string to append to
    * @return String with code appended
    */
  def generateAccessParallelCollectionBB(str: String): String = {
    var s = str
    s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessParallelCollectionBB(JNIEnv * jni, jobject me, jarray byteBuffers, jint threads, jint object_size) {\n"
    s += s"  jint buffers = (*jni)->GetArrayLength(jni, byteBuffers);\n"
    s += s"  jarray* bufRefs = (jarray*)malloc(sizeof(jarray) * buffers);\n"
    s += s"  jlong* bufLens = (jlong*)malloc(sizeof(jlong) * buffers);\n"
    s += s"  $prim** bufs = ($prim**)malloc(sizeof($prim*) * buffers);\n"
    if (mergeBB) {
      s += s"  int* bufOffsets = (int*)malloc(sizeof(int) * buffers);\n"
      s += s"  int num_prims = 0;\n"
    } else {
      s += s"  $prim * bufResult = ($prim*)malloc(sizeof($prim) * buffers);\n"
    }
    s += s"\n"
    s += s"  //Initialize stuff using JNI\n"
    s += s"  for (int b = 0; b < buffers; b++) {\n"
    s += s"    bufRefs[b] = (*jni)->GetObjectArrayElement(jni, byteBuffers, b);\n"
    s += s"    bufLens[b] = (*jni)->GetArrayLength(jni, bufRefs[b]) / sizeof($prim);\n"
    if (mergeBB) {
      s += s"    bufOffsets[b] = num_prims;\n"
      s += s"    num_prims += bufLens[b];\n"
    }
    s += s"    bufs[b] = ($prim*)(*jni)->GetByteArrayElements(jni, bufRefs[b], NULL);\n"
    s += s"  }\n"
    s += s"\n"
    if (mergeBB) {
      s += s"  // Allocate memory for copied objects\n"
      s += s"  $prim* native_col = ($prim*)malloc(num_prims*sizeof($prim));\n"
      s += s"\n"
    }
    s += s"  $prim result = ($prim)0;\n"
    s += s"\n"
    //s += s"  if (threads != 0) omp_set_num_threads(threads);\n"
    s += s"  #pragma omp parallel for num_threads(threads)\n"
    s += s"  for (int b = 0; b < buffers; b++) {\n"
    if (mergeBB) {
      s += s"    memcpy(($prim*)&native_col[bufOffsets[b]], bufs[b], bufLens[b] * sizeof($prim));\n"
    } else {
      s += s"    bufResult[b] = 0;\n"
      s += s"    for (int i = 0; i < bufLens[b]; i++)\n"
      s += s"      bufResult[b] += bufs[b][i];\n"
    }
    s += s"  }\n"
    if (addPrims) {
      if (mergeBB) {
        s += s"\n"
        s += s"  #pragma omp parallel for reduction(+:result)\n"
        s += s"  for (int i = 0; i < num_prims; i++) {\n"
        s += s"    result += native_col[i];\n"
        s += s"  }\n"
      } else {
        s += s"  for (int b = 0; b < buffers; b++)\n"
        s += s"     result += bufResult[b];"
      }
    }
    s += s"\n"
    s += s"  // Release the arrays, and JNI_ABORT to prevent copying back the values\n"
    s += s"  for (int b = 0; b < buffers; b++) {\n"
    s += s"     (*jni)->ReleaseByteArrayElements(jni, bufRefs[b], (jbyte*)bufs[b], JNI_ABORT);\n"
    s += s"  }\n"
    s += s"\n"
    if (mergeBB) {
      s += s"  free(native_col);\n"
      s += s"  free(bufOffsets);\n"
    } else {
      s += s"  free(bufResult);\n"
    }
    s += s"  free(bufRefs);\n"
    s += s"  free(bufLens);\n"
    s += s"  free(bufs);\n"
    s += s"\n"
    s += s"  return result;\n"
    s += s"}\n\n"
    s
  }

  /**
    * Generate C code to access a single object from a directly allocated (off-heap) ByteBuffer
    * @param str string to append to
    * @return String with code appended
    */
  def generateAccessObjectDirectByteBuffer(str: String): String = {
    var s = str
    s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessObjectDirectByteBuffer(JNIEnv * jni, jobject me, jobject byteArray) {\n"
    s += s"  jsize len = (*jni)->GetDirectBufferCapacity(jni, byteArray) / $primSize;\n"
    s += s"  $prim * values = ($prim*)(*jni)->GetDirectBufferAddress(jni, byteArray);\n"
    s += s"  $prim result = ($prim)0;\n"
    s += "\n"
    if (addPrims) {
      s += "  for (int i=0; i<len; i++) {\n"
      s += "    result += values[i];\n"
      s += "  }\n"
    }
    s += "  return result;\n"
    s += "}\n"
    s += "\n"
    s
  }

  /**
    * Generate C code to access a collection of objects from directly allocated (off-heap) ByteBuffer
    * @param str string to append to
    * @return String with code appended
    */
  def generateAccessParallelCollectionDBB(str: String): String = {
    var s = str
    s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessParallelCollectionDBB(JNIEnv * jni, jobject me, jarray byteBuffers, jint threads, jint object_size) {\n"
    s += s"  jint buffers = (*jni)->GetArrayLength(jni, byteBuffers);\n"
    s += s"  jobject * bufRefs = (jobject*)malloc(sizeof(jobject)*threads);\n"
    s += s"  jlong * bufLens = (jlong*)malloc(sizeof(jlong)*threads);\n"
    s += s"  $prim** bufs = ($prim**)malloc(sizeof($prim*)*threads);\n"
    if (mergeBB) {
      s += s"  int* bufOffsets = (int*)malloc(sizeof(int) * buffers);\n"
      s += s"  int num_prims = 0;\n"
    } else {
      s += s"  $prim * bufResult = ($prim*)malloc(sizeof($prim) * buffers);\n"
    }
    s += s"\n"
    s += s"  //Initialize stuff using JNI\n"
    s += s"  for (int b = 0; b < buffers; b++) {\n"
    s += s"    bufRefs[b] = (*jni)->GetObjectArrayElement(jni, byteBuffers, b);\n"
    s += s"    bufLens[b] = (*jni)->GetDirectBufferCapacity(jni, bufRefs[b]) / $primSize;\n"
    if (mergeBB) {
      s += s"    bufOffsets[b] = num_prims;\n"
      s += s"    num_prims += bufLens[b];\n"
    }
    s += s"    bufs[b] = ($prim*)(*jni)->GetDirectBufferAddress(jni, bufRefs[b]);\n"
    s += s"  }\n"
    s += s"\n"
    if (mergeBB) {
      s += s"  // Allocate memory for copied objects\n"
      s += s"  $prim* native_col = ($prim*)malloc(num_prims*sizeof($prim));\n"
      s += s"\n"
    }
    s += s"  $prim result = ($prim)0;\n"
    s += s"\n"
    s += s"  // Copy stuff\n"
    //s += s"  if (threads != 0) omp_set_num_threads(threads);\n"
    s += s"  #pragma omp parallel for num_threads(threads)\n"
    s += s"  for (int b = 0; b < buffers; b++) {\n"
    if (mergeBB) {
      s += s"    memcpy(($prim*)&native_col[bufOffsets[b]], bufs[b], bufLens[b] * sizeof($prim));\n"
    } else {
      s += s"    bufResult[b] = 0;\n"
      s += s"    for (int i = 0; i < bufLens[b]; i++)\n"
      s += s"      bufResult[b] += bufs[b][i];\n"
    }
    s += s"  }\n"
    if (addPrims) {
      if (mergeBB) {
        s += s"\n"
        s += s"  #pragma omp parallel for reduction(+:result)\n"
        s += s"  for (int i = 0; i < num_prims; i++) {\n"
        s += s"    result += native_col[i];\n"
        s += s"  }\n"
      } else {
        s += s"  for (int b = 0; b < buffers; b++)\n"
        s += s"     result += bufResult[b];"
      }
    }
    if (mergeBB) {
      s += s"  free(native_col);\n"
      s += s"  free(bufOffsets);\n"
    } else {
      s += s"  free(bufResult);\n"
    }
    s += s"  free(bufRefs);\n"
    s += s"  free(bufLens);\n"
    s += s"  free(bufs);\n"
    s += s"\n"
    s += s"  return result;\n"
    s += s"}\n\n"
    s
  }

  /**
    * Generate C code to access a single object from a memory region allocated through the Unsafe library
    * @param str string to append to
    * @return String with code appended
    */
  def generateAccesObjectUnsafe(str: String): String = {
    var s = str
    s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessObjectUnsafe(JNIEnv * jni, jobject me, jlong address, jlong len) {\n"
    s += s"  $prim * values = ($prim*)address;\n"
    s += s"  $prim result = ($prim)0;\n"
    if (addPrims) {
      s += "\n"
      s += "  for (long i=0; i<len; i++) {\n"
      s += "    result += values[i];\n"
      s += "  }\n"
    }
    s += "  return result;\n"
    s += "}\n"
    s += "\n"
    s
  }

  /**
    * Generate C code to access a collection of objects from a memory region allocated through the Unsafe library
    * @param str string to append to
    * @return String with code appended
    */
  def generateAccessParallelCollectionUnsafe(str: String): String = {
    var s = str
    s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessParallelCollectionUnsafe(JNIEnv * jni, jobject me, jlong address, jlong len, jint threads) {\n"
    s += s"  $prim * values = ($prim*)address;\n"
    s += s"  $prim result = ($prim)0;\n"
    if (addPrims) {
      s += "\n"
      s += s"  #pragma omp parallel for reduction(+:result) num_threads(threads)\n"
      s += s"  for (long i = 0; i < len; i++) {\n"
      s += s"    result += values[i];\n"
      s += s"  }\n"
    }
    s += "  return result;\n"
    s += "}\n"
    s += "\n"
    s
  }

  /**
    * Generate code to obtain field IDs for JNI based methods
    * @return The code
    */
  def generateJNIFieldNums(): String = {
    var s: String = ""
    s += "  // Obtain class references\n"
    for (c <- ClassGenerator.classes) {
      if (c.typeRef != 0) {
        // Skip arrays
        val className = s"C${c.typeRef}"
        s += s"""  jclass $className = (*jni)->FindClass(jni, "$className");\n"""
      }
    }
    s += "  // Obtain field IDs\n"
    for (c <- ClassGenerator.classes) {
      if (c.typeRef != 0) {
        // Skip arrays
        val className = s"C${c.typeRef}"
        for (p <- 0 until c.primitives) {
          s += s"""  jfieldID fid_${className}_p$p = (*jni)->GetFieldID(jni, $className, \"p$p\", \"$primSig\");\n"""
        }
        for (a <- 0 until c.arrays) {
          s += s"""  jfieldID fid_${className}_a$a = (*jni)->GetFieldID(jni, $className, \"a$a\", \"[$primSig\");\n"""
        }
        for (r <- 0 until c.references) {
          s += s"""  jfieldID fid_${className}_r$r = (*jni)->GetFieldID(jni, $className, \"r$r\", \"LC${c.refTypes(r)};\");\n"""
        }
      }
    }
    s
  }

  /**
    * Recursively generate code to access an single object with a single thread through the JNI approach
    * @param str The string to append the code to
    * @param su The super object identifier of the object currently working on
    * @param clazz The class of the object the current method call is working on
    * @return The appended string
    */
  def generateAccesObjectJNI(str: String, su: String, clazz: gClass): String = {
    var s: String = str
    var objectName = if (clazz.level == 0) "root" else su

    if (clazz.level == 0) {
      s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessObjectJNI(JNIEnv * jni, jobject this, jobject root, jint size) {\n"
      s += s"  $prim* native_obj = ($prim*)malloc(sizeof($prim)*size);\n"
      s += s"  int pos = 0;\n"
      s += s"  $prim result = ($prim)0;\n"
      s += "  jsize len = 0;\n"
      s += "  jarray arr = NULL;\n"
      s += s"  $prim * arrVals = NULL;\n"
      s += "\n"
      s += generateJNIFieldNums()
    }

    val className = s"C${clazz.typeRef}"

    for (p <- 0 until clazz.primitives) {
      s += s"  native_obj[pos] = (*jni)->Get${prim.capitalize}Field(jni, $objectName, fid_${className}_p$p);\n"
      s += s"  pos++;\n"
    }

    for (a <- 0 until clazz.arrays) {
      s += "\n"
      s += s"  arr = (*jni)->GetObjectField(jni, $objectName, fid_${className}_a$a);\n"
      s += s"  len = (*jni)->GetArrayLength(jni, arr);\n"
      s += s"  arrVals = (*jni)->Get${prim.capitalize}ArrayElements(jni, arr, NULL);\n"
      //s += s"  for (int i = 0; i < len; i++) result += arrVals[i];\n"
      s += s"  memcpy(($prim*)&native_obj[pos], arrVals, len * sizeof($prim));\n"
      s += s"  pos += len;\n"
      s += s"  (*jni)->Release${prim.capitalize}ArrayElements(jni, arr, arrVals, JNI_ABORT);\n"
    }

    for (r <- 0 until clazz.references) {
      s += "\n"
      s += s"  jobject ${objectName}_r$r = (*jni)->GetObjectField(jni, $objectName, fid_${className}_r$r);\n"
      s = generateAccesObjectJNI(s, objectName + s"_r$r", ClassGenerator.get(clazz.refTypes(r)))
    }

    if (clazz.level == 0) {
      if (addPrims) {
        s += "\n"
        s += "  for (int i = 0; i < size; i++) {\n"
        s += "    result += native_obj[i];\n"
        s += "  }\n"
      }
      s += "\n"
      s += s"  free(($prim*)native_obj);\n"
      s += s"  return result;\n"
      s += s"}\n\n"
    }
    s
  }

  /**
    * Recursively generate code to access a collection of objects with multiple threads through the JNI approach
    * @param str The string to append the code to
    * @param su The super object identifier of the object currently working on
    * @param clazz The class of the object the current method call is working on
    * @return The appended string
    */
  def generateAccesParallelCollectionJNI(str: String, su: String, clazz: gClass): String = {
    var s: String = str
    var objectName = if (clazz.level == 0) "obj" else su

    if (clazz.level == 0) {
      s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessParallelCollectionJNI(JNIEnv * jni, jobject this, jobject array, jint threads, jint object_size) {\n"
      s += s"  jsize num_objects = (*jni)->GetArrayLength(jni, array);\n"
      s += s"  // Allocate space for the copy of the object\n"
      s += s"  $prim* native_col = ($prim*)malloc(sizeof($prim) * num_objects * object_size);\n"
      s += s"  $prim result = ($prim)0;\n"
      s += generateJNIFieldNums()
      s += s"  jobject g_array = (*jni)->NewGlobalRef(jni, array);\n"
      s += s"\n"
      s += s"  int obj_per_thread = num_objects / threads;\n"
      s += s"  int leftovers = num_objects - obj_per_thread * threads;\n"
      //s += s"  if (threads != 0) omp_set_num_threads(threads);\n"
      s += s"  #pragma omp parallel num_threads(threads)\n"
      s += s"  {\n"
      s += s"    JNIEnv * thread_jni = jni;\n"
      s += s"    int tid = omp_get_thread_num();\n"
      s += s"    if (tid != 0)\n"
      s += s"      (*global_vm)->AttachCurrentThread(global_vm, (void**)&thread_jni, NULL);\n"
      s += s"\n"
      s += s"    int oend;\n"
      s += s"    int ostart = tid * obj_per_thread;\n"
      s += s"    if (tid == threads - 1)\n"
      s += s"      oend = (tid + 1) * obj_per_thread + leftovers;\n"
      s += s"    else\n"
      s += s"      oend = (tid + 1) * obj_per_thread;\n"
      s += s"\n"
      s += s"    for (jsize i = ostart; i < oend; i++) {\n"
      s += s"      int pos = 0;\n"
      s += s"      jsize len = 0;\n"
      s += s"      jarray arr = NULL;\n"
      s += s"      $prim * arrVals = NULL;\n"
      s += s"\n"
      s += s"      jobject obj;\n"
      s += s"      obj = (*jni)->GetObjectArrayElement(thread_jni, g_array, i);\n"
      //s += s"    assert(obj != NULL);\n"
    }

    val className = s"C${clazz.typeRef}"

    for (p <- 0 until clazz.primitives) {
      s += s"      native_col[i*object_size+pos] = (*jni)->Get${prim.capitalize}Field(thread_jni, $objectName, fid_${className}_p$p);\n"
      s += s"      pos++;\n"
    }

    for (a <- 0 until clazz.arrays) {
      s += "\n"
      s += s"      arr = (*jni)->GetObjectField(thread_jni, $objectName, fid_${className}_a$a);\n"
      s += s"      len = (*jni)->GetArrayLength(thread_jni, arr);\n"
      s += s"      arrVals = (*jni)->Get${prim.capitalize}ArrayElements(thread_jni, arr, NULL);\n"
      s += s"      memcpy(($prim*)&native_col[i*object_size+pos], arrVals, len * sizeof($prim));\n"
      s += s"      pos += len;\n"
      s += s"      (*jni)->Release${prim.capitalize}ArrayElements(thread_jni, arr, arrVals, JNI_ABORT);\n"
    }

    for (r <- 0 until clazz.references) {
      s += "\n"
      s += s"      jobject ${objectName}_r$r = (*jni)->GetObjectField(thread_jni, $objectName, fid_${className}_r$r);\n"
      s = generateAccesParallelCollectionJNI(s, objectName + s"_r$r", ClassGenerator.get(clazz.refTypes(r)))
    }

    if (clazz.level == 0) {
      s += s"    }\n"
      s += s"    if (tid != 0)\n"
      s += s"      (*global_vm)->DetachCurrentThread(global_vm);\n"
      s += s"  }\n"
      s += s"\n"

      if (addPrims) {
        s += s"  #pragma omp parallel for reduction(+:result)\n"
        s += s"  for (int i = 0; i < num_objects * object_size; i++) {\n"
        s += s"    result += native_col[i];\n"
        s += s"  }\n"
      }
      s += s"  (*jni)->DeleteGlobalRef(jni, g_array);\n"
      s += s"  free(($prim*)native_col);\n"
      s += s"  return result;\n"
      s += "}\n\n"
    }
    s
  }

  /**
    * Recursively generate code to access an single object with a single thread through the Direct (copy) approach
    * This means the object is first serialized to a contiguous memory region
    * @param str The string to append the code to
    * @param me The current object wich this method call is working on
    * @param su The super object identifier of the object currently working on
    * @param clazz The class of the object the current method call is working on
    * @return The appended string
    */
  def generateAccessObjectDirectCopy(str: String,
                                     me: String,
                                     su: String,
                                     clazz: gClass): String = {
    var s: String = str
    var objectName = me
    //if (clazz.level == 0) "root" else me
    var off = 16

    if (clazz.level == 0) {
      s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessObjectDirectCopy(JNIEnv * jni, jobject this, jobject initial_ref, jint size) {\n"
      s += s"  $prim result = ($prim)0;\n"
      s += s"  $prim* native_obj = ($prim*)malloc(sizeof($prim)*size);\n"
      s += s"  int pos = 0;\n"
      s += s"  int len = 0;\n"
      s += s"  unsigned char * arr = NULL;\n"
      s += s"  $prim * arrVals = NULL;\n"
      s += s"\n"
      s += s"  unsigned char * $me = (unsigned char *)*(unsigned long*)$su;\n"
    }

    for (p <- 0 until clazz.primitives) {
      s += s"  $prim ${objectName}_p$p = *($prim*)&$objectName[$off];\n"
      //s += s"  result += ${objectName}_p$p;\n"
      s += s"  native_obj[pos] = ${objectName}_p$p;\n"
      s += s"  pos++;\n"
      off += primSize
    }

    for (a <- 0 until clazz.arrays) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"  arr = (unsigned char*)*(unsigned long*)&$objectName[$off];\n"
      s += s"  len = *(int*)&arr[16];\n"
      s += s"  arrVals = ($prim*)&arr[24];\n"
      s += s"  memcpy(($prim*)&native_obj[pos], arrVals, len * sizeof($prim));\n"
      s += s"  pos += len;\n"
      off += primSize
    }

    for (r <- 0 until clazz.references) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"  unsigned char * ${objectName}_r$r = (unsigned char*)(*(unsigned long*)&$objectName[$off]);\n"
      s = generateAccessObjectDirectCopy(s, s"${objectName}_r$r", objectName, ClassGenerator.get(clazz.refTypes(r)))
      off += primSize
    }

    if (clazz.level == 0) {
      if (addPrims) {
        s += "\n"
        s += "  for (int i = 0; i < size; i++) {\n"
        s += "    result += native_obj[i];\n"
        s += "  }\n"
      }
      s += s"  free(($prim*)native_obj);\n"
      s += s"  return result;\n"
      s += "}\n\n"
    }
    s
  }

  /**
    * Recursively generate code to access a collection of objects with multiple threads through the Direct (copy) approach
    * This means the object is first serialized to a contiguous memory region
    * @param str The string to append the code to
    * @param me The current object wich this method call is working on
    * @param su The super object identifier of the object currently working on
    * @param clazz The class of the object the current method call is working on
    * @return The appended string
    */
  def generateAccessParallelCollectionDirectCopy(str: String,
                                                 me: String,
                                                 su: String,
                                                 clazz: gClass): String = {
    var s: String = str
    var objectName = me
    //if (clazz.level == 0) "root" else me
    var off = 16

    if (clazz.level == 0) {
      s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessParallelCollectionDirectCopy(JNIEnv * jni, jobject this, jobject $su, jint threads, jint object_size) {\n"
      s += s"  $prim result = ($prim)0;\n"
      s += "\n"
      s += s"  unsigned char * ${me}_array = (unsigned char *)*(unsigned long*)$su;\n"
      s += "\n"
      s += s"  // Obtain array size from offset 16\n"
      s += s"  int num_objects = *(int*)&${me}_array[16];\n"
      s += s"  // Allocate space for the copied object\n"
      s += s"  $prim* native_col = ($prim*)malloc(sizeof($prim)*num_objects*object_size);\n"
      s += "\n"
      s += s"  // Loop over all objects in the array\n"
      //s += s"  if (threads != 0) omp_set_num_threads(threads);\n"
      s += s"  #pragma omp parallel for num_threads(threads)\n"
      s += s"  for (int i = 0; i < num_objects; i++) {\n"
      s += s"    int pos = 0;                 // Position in the copied object array\n"
      s += s"    int len = 0;                 // Length of any arrays in the object\n"
      s += s"    unsigned char * arr = NULL;  // Pointer to any array objects\n"
      s += s"    $prim * arrVals = NULL;      // Primitives in the array objects \n"
      s += s"\n"
      s += s"    // Get next object\n"
      s += s"    unsigned char * $me = (unsigned char*)*(unsigned long*)&${me}_array[24+i*8];\n"
    }

    for (p <- 0 until clazz.primitives) {
      //s += s"    $prim ${objectName}_p$p = *($prim*)&$objectName[$off];\n"
      s += s"    native_col[i*object_size+pos] = *($prim*)&$objectName[$off];\n"
      s += s"    pos++;\n"
      off += primSize
    }

    for (a <- 0 until clazz.arrays) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"    arr = (unsigned char*)*(unsigned long*)&$objectName[$off];\n"
      s += s"    len = *(int*)&arr[16];\n"
      s += s"    arrVals = ($prim*)&arr[24];\n"
      s += s"    memcpy(($prim*)&native_col[i*object_size+pos], arrVals, len * sizeof($prim));\n"
      s += s"    pos += len;\n"
      off += primSize
    }

    for (r <- 0 until clazz.references) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"    unsigned char * ${objectName}_r$r = (unsigned char*)(*(unsigned long*)&$objectName[$off]);\n"
      s = generateAccessParallelCollectionDirectCopy(s, s"${objectName}_r$r", objectName, ClassGenerator.get(clazz.refTypes(r)))
      off += primSize
    }

    if (clazz.level == 0) {
      s += "  }\n"
      s += "\n"
      if (addPrims) {
        s += s"  #pragma omp parallel for reduction(+:result)\n"
        s += s"  for (int i = 0; i < num_objects * object_size; i++) {\n"
        s += s"    result += native_col[i];\n"
        s += s"  }\n"
      }
      s += s"  free(($prim*)native_col);\n"
      s += s"  return result;\n"
      s += "}\n\n"
    }
    s
  }

  /**
    * Recursively generate code to access an single object with a single thread through the Direct (no-copy) approach
    * @param str The string to append the code to
    * @param me The current object wich this method call is working on
    * @param su The super object identifier of the object currently working on
    * @param clazz The class of the object the current method call is working on
    * @return The appended string
    */
  def generateAccesObjectDirect(str: String,
                                me: String,
                                su: String,
                                clazz: gClass): String = {
    var s: String = str
    var objectName = me
    //if (clazz.level == 0) "root" else me
    var off = 16

    if (clazz.level == 0) {
      s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessObjectDirect(JNIEnv * jni, jobject this, jobject initial_ref) {\n"
      s += s"  int i = 0;\n"
      s += s"  $prim tmp;\n"
      if (avx) {
        s += s"  __m256i avx_tmp;\n"
      }
      s += s"  $prim result = ($prim)0;\n"
      s += "  int len = 0;\n"
      s += "  unsigned char * arr = NULL;\n"
      s += s"  $prim * arrVals = NULL;\n"
      s += "\n"
      s += s"  unsigned char * $me = (unsigned char *)*(unsigned long*)$su;\n"
    }

    for (p <- 0 until clazz.primitives) {
      if (addPrims) {
        s += s"  $prim ${objectName}_p$p = *($prim*)&$objectName[$off];\n"
        s += s"  result += ${objectName}_p$p;\n"
      }
      else if (prefetch) {
        //s += s"  __builtin_prefetch(($prim*)&$objectName[$off], 0, 1);\n"
        s += s"  tmp = *($prim*)&$objectName[$off];\n"
      }
      off += primSize
    }

    for (a <- 0 until clazz.arrays) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"  arr = (unsigned char*)*(unsigned long*)&$objectName[$off];\n"
      s += s"  len = *(int*)&arr[16];\n"
      s += s"  arrVals = ($prim*)&arr[24];\n"

      if (addPrims) {
        s += s"  for (i = 0; i < len; i++)\n"
        s += s"    result += arrVals[i];\n"
      } else if (prefetch) {
        if (avx) {
          s += s"  for (i = 0; i < len - (len%8); i+=8)\n"
          s += s"    avx_tmp = _mm256_loadu_si256 ((__m256i const*)&arrVals[i]);\n"
          s += s"  for (;i<len;i++)\n"
          s += s"    tmp = arrVals[i];\n"
        } else {
          //s += s"  __builtin_prefetch(&arrVals[i], 0, 1);\n"
          s += s"  for (i = 0; i < len; i++)\n"
          s += s"    tmp = arrVals[i];\n"
        }
      }



      off += primSize
    }

    for (r <- 0 until clazz.references) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"  unsigned char * ${objectName}_r$r = (unsigned char*)(*(unsigned long*)&$objectName[$off]);\n"
      s = generateAccesObjectDirect(s, s"${objectName}_r$r", objectName, ClassGenerator.get(clazz.refTypes(r)))
      off += primSize
    }

    if (clazz.level == 0) {
      s += "\n"
      s += "  return result;\n"
      s += "}\n\n"
    }
    s
  }

  /**
    * Recursively generate code to access a collection of objects with multiple threads through the Direct (no-copy) approach
    * @param str The string to append the code to
    * @param me The current object wich this method call is working on
    * @param su The super object identifier of the object currently working on
    * @param clazz The class of the object the current method call is working on
    * @return The appended string
    */
  def generateAccessParallelCollectionDirect(str: String,
                                             me: String,
                                             su: String,
                                             clazz: gClass): String = {
    var s: String = str
    var objectName = me
    //if (clazz.level == 0) "root" else me
    var off = 16

    if (clazz.level == 0) {
      s += s"JNIEXPORT j$prim JNICALL Java_BenchRunner_00024_accessParallelCollectionDirect(JNIEnv * jni, jobject this, jobject $su, jint threads) {\n"
      s += s"  $prim result = 0.0f;\n"
      s += "\n"
      s += s"  unsigned char * ${me}_array = (unsigned char *)*(unsigned long*)$su;\n"
      s += "\n"
      s += s"  // Obtain array size from offset 16\n"
      s += s"  int num_objects = *(int*)&${me}_array[16];\n"
      s += "\n"
      if (addPrims) {
        s += s"  // Allocate space for the partial results\n"
        s += s"  $prim * parResult = ($prim*)malloc(sizeof($prim) * num_objects);\n"
        s += "\n"
      }
      s += s"  // Loop over all objects in the array\n"
      //s += s"  if (threads != 0) omp_set_num_threads(threads);\n"
      s += s"  #pragma omp parallel for num_threads(threads)\n"
      s += s"  for (int o = 0; o < num_objects; o++) {\n"
      s += s"    int i;\n"
      s += s"    int len = 0;\n"
      s += s"    unsigned char * arr = NULL;\n"
      s += s"    $prim * arrVals = NULL;\n"
      if (addPrims) {
        s += s"    $prim part = ($prim)0;\n"
      } else if (prefetch) {
        s += s"    $prim tmp;\n"
        if (avx) {
          s += s"  __m256i avx_tmp;\n"
        }
      }
      s += s"    // Get next object\n"
      s += s"    unsigned char * $me = (unsigned char*)*(unsigned long*)&${me}_array[24+o*8];\n"
    }

    for (p <- 0 until clazz.primitives) {
      if (addPrims) {
        s += s"    $prim ${objectName}_p$p = *($prim*)&$objectName[$off];\n"
        s += s"    part += ${objectName}_p$p;\n"
      }
      else if (prefetch) {
        //s += s"    __builtin_prefetch(($prim*)&$objectName[$off], 0, 1);\n"
        s += s"    tmp = *($prim*)&$objectName[$off];\n"
      }
      off += primSize
    }

    for (a <- 0 until clazz.arrays) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"    arr = (unsigned char*)*(unsigned long*)&$objectName[$off];\n"
      s += s"    len = *(int*)&arr[16];\n"
      s += s"    arrVals = ($prim*)&arr[24];\n"
      if (addPrims) {
        s += s"  for (i = 0; i < len; i++)\n"
        s += s"    part += arrVals[i];\n"
      } else if (prefetch) {
        if (avx) {
          s += s"  for (i = 0; i < len - (len%8); i+=8)\n"
          s += s"    avx_tmp = _mm256_loadu_si256 ((__m256i const*)&arrVals[i]);\n"
          s += s"  for (;i<len;i++)\n"
          s += s"    tmp = arrVals[i];\n"
        } else {
          s += s"  for (i = 0; i < len; i++)\n"
          s += s"    tmp = arrVals[i];\n"
        }
      }
      off += primSize
    }

    for (r <- 0 until clazz.references) {
      s += "\n"
      if (off % 8 != 0) off += primSize
      s += s"    unsigned char * ${objectName}_r$r = (unsigned char*)(*(unsigned long*)&$objectName[$off]);\n"
      s = generateAccessParallelCollectionDirect(s, s"${objectName}_r$r", objectName, ClassGenerator.get(clazz.refTypes(r)))
      off += primSize
    }

    if (clazz.level == 0) {
      if (addPrims) {
        s += "\n"
        s += "    parResult[o] = part;\n"
      }
      s += "  }\n"
      s += "\n"
      if (addPrims) {
        s += s"  #pragma omp parallel for reduction(+:result)\n"
        s += s"  for(int o = 0; o < num_objects; o++) {\n"
        s += s"    result += parResult[o];\n"
        s += s"  }\n"
        s += s"  free(parResult);\n"
      }
      s += s"  return result;\n"
      s += "}\n\n"
    }
    s
  }
}