
import java.io.File
import java.nio.{ByteBuffer, ByteOrder}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.io.Source

object BenchRunner {

  val helpText: String = "Usage: scala -cp target/jvm2accbench-1.0.jar:bench/out \"BenchRunner\" -J-XX:-UseCompressedOops \n" +
    "-h or --help         : Display this help text\n" +
    "-m single|collection : Work on a single object with a single thread or a collection of objects with a number of threads specified by -t\n" +
    "-t                   : Number of threads to use, if set to 0 use statically determined number of threads\n" +
    "-o                   : Number of objects in the collection\n" +
    "-r                   : Repeats of the experiment\n" +
    "-v                   : verbose output\n"

  var verbose: Boolean = false

  var JITOffset: Int = 0
  var repeats: Int = 1
  var threads: Int = 1

  var collectionSize: Int = 1

  var mode: String = "single"
  var parallel: Boolean = true

  val nativeOrder: ByteOrder = ByteOrder.nativeOrder

  /// Instantiator for the generated object class
  val clazz: Class[_] = this.getClass.getClassLoader.loadClass("GeneratedInstantiator")

  /// Size of a single object
  val objectSize: Int = Source.fromFile("bench/object.size").getLines.mkString.toInt
  var primSize: Int = 4

  var bbthreads:Int = 1
  var dbbthreads:Int = 1
  var uthreads:Int = 1
  var jnithreads:Int = 1
  var dpthreads:Int = 1
  var directthreads:Int = 1

  var skipBB:Boolean  = false
  var skipDBB:Boolean = false
  var skipU:Boolean   = false
  var skipJNI:Boolean = false
  var skipDP:Boolean  = false
  var skipD:Boolean   = false

  /// Library compiled from generated sources
  System.load(new File(".").getCanonicalPath + "/lib/libbench.so")

  // Single object access native functions
  @native def accessObjectByteArray(ba: Array[Byte]): Int
  @native def accessObjectDirectByteBuffer(bb: ByteBuffer): Int
  @native def accessObjectUnsafe(address: Long, size: Int): Int
  @native def accessObjectJNI(obj: Any, size : Int): Int
  @native def accessObjectDirectCopy(obj: Any, size : Int): Int
  @native def accessObjectDirect(obj: Any): Int

  // Collection parallel access native functions
  @native def accessParallelCollectionBB(aba : Array[Array[Byte]], threads: Int, objectSize: Int): Int
  @native def accessParallelCollectionDBB(bba : Array[ByteBuffer], threads: Int, objectSize: Int): Int
  @native def accessParallelCollectionUnsafe(address: Long, size: Int, threads: Int): Int
  @native def accessParallelCollectionJNI(obj: Any, threads: Int, objectSize: Int): Int
  @native def accessParallelCollectionDirectCopy(obj: Any, threads: Int, objectSize: Int): Int
  @native def accessParallelCollectionDirect(obj: Any, threads: Int): Int

  /**
    * Verbose mode printing
    * @param something Print "something" if verbose is enabled
    */
  def printv(something: Any): Unit = {
    if (verbose)
      println(something)
  }

  def main(args: Array[String]): Unit = {

    // Parse arguments
    if (args.contains("--help") || args.contains("-h")) {
      println(helpText)
      return
    }

    verbose = args.contains("-v")

    repeats = args(args.indexWhere(s => s == "-r") + 1).toInt

    if (args.contains("-j")) {
      JITOffset = args(args.indexWhere(s => s == "-j") + 1).toInt
    }

    mode = args(args.indexWhere(s => s == "-m") + 1)

    collectionSize = if (mode == "collection") args(args.indexWhere(s => s == "-o") + 1).toInt else 1

    skipBB  = args.contains("--skipbb")
    skipDBB = args.contains("--skipdbb")
    skipU   = args.contains("--skipu")
    skipJNI = args.contains("--skipjni")
    skipDP  = args.contains("--skipdp")
    skipD   = args.contains("--skipd")

    if (mode == "collection") {
      threads = args(args.indexWhere(s => s == "-t") + 1).toInt
      if (threads == 0) { // fixed threads
        bbthreads     =  4
        dbbthreads    =  4
        jnithreads    =  4
        uthreads      = 10
        directthreads = 40
        dpthreads     = 40
      }
      else {
        bbthreads     = threads
        dbbthreads    = threads
        jnithreads    = threads
        uthreads      = threads
        directthreads = threads
        dpthreads     = threads
      }
    }

    print(f"$threads%4d, $objectSize%10d, $repeats%4d, ")

    if (mode == "single") {
      print(f"${1}%8d, $objectSize%10d, ")
      runSingle()
    }
    else if (mode == "collection") {
      print(f"$collectionSize%8d, ${collectionSize * objectSize}%10d, ")
      runParallelCollection()
    }
    else {
      throw new Error("-m parameter must be \"single\" or \"collection\"" + helpText)
    }

  }

  /**
    * Run the experiments on a single object with a single thread
    */
  def runSingle(): Unit = {
    var totalDBB = 0L
    var totalBB = 0L
    var totalU = 0L
    var totalJNI = 0L
    var totalDirect = 0L
    var totalDP = 0L

    var resultDBB = 0L
    var resultBB = 0L
    var resultU = 0L
    var resultJNI = 0L
    var resultDirect = 0L
    var resultDP = 0L

    // ByteBuffer
    if (!skipBB) {
      val BBobj = clazz.newInstance.asInstanceOf[Instantiator].root
      System.gc()
      for (experiment <- 0 until repeats + JITOffset) {
        val (timeBB, bbr) = Measure.time {
          val bb = ByteBuffer.allocate(objectSize)
          bb.order(nativeOrder)
          BBobj.asInstanceOf[ClassUnderTest].writeByteBufferManual(bb.asIntBuffer())
          accessObjectByteArray(bb.array)
        }
        if (experiment >= JITOffset) {
          printv(s"ByteBuffer : $timeBB")
          printv(s"ByteBuffer Result   : $bbr")
          totalBB += timeBB
          resultBB += bbr
        }
      }
    }

    // Direct ByteBuffer
    if (!skipDBB) {
      val DBBobj = clazz.newInstance.asInstanceOf[Instantiator].root
      System.gc()
      for (experiment <- 0 until repeats + JITOffset) {
        val (timeDBB, dbbr) = Measure.time {
          val dbb = ByteBuffer.allocateDirect(objectSize)
          dbb.order(nativeOrder)
          DBBobj.asInstanceOf[ClassUnderTest].writeByteBufferManual(dbb.asIntBuffer())
          accessObjectDirectByteBuffer(dbb)
        }
        if (experiment >= JITOffset) {
          printv(s"ByteBuffer : $timeDBB")
          printv(s"ByteBuffer Result   : $dbbr")
          totalDBB += timeDBB
          resultDBB += dbbr
        }
      }
    }

    // Unsafe
    if (!skipU) {
      val UObj = clazz.newInstance.asInstanceOf[Instantiator].root
      System.gc()
      for (experiment <- 0 until repeats + JITOffset) {
        val (timeU, ur) = Measure.time {
          val ui = UnsafeManager.getUnsafeInstance
          val ua = ui.allocateMemory(objectSize)
          UObj.asInstanceOf[ClassUnderTest].writeUnsafeManual(ua, ui)
          val ret = accessObjectUnsafe(ua, objectSize / primSize)
          ui.freeMemory(ua)
          ret
        }

        if (experiment >= JITOffset) {
          printv(s"Unsafe : $timeU")
          printv(s"Unsafe Result   : $ur")
          totalU += timeU
          resultU += ur
        }
      }
    }

    // JNI
    if (!skipJNI) {
      val JNIObj = clazz.newInstance.asInstanceOf[Instantiator].root
      System.gc()
      for (experiment <- 0 until repeats + JITOffset) {
        val (timeJNI, jnir) = Measure.time(accessObjectJNI(JNIObj, objectSize / primSize))
        if (experiment >= JITOffset) {
          printv(s"Unsafe : $timeJNI")
          printv(s"Unsafe Result   : $jnir")
          totalJNI += timeJNI
          resultJNI += jnir
        }
      }
    }

    // Direct Pickled
    if (!skipDP) {
      val DPObj = clazz.newInstance.asInstanceOf[Instantiator].root
      System.gc()
      for (experiment <- 0 until repeats + JITOffset) {
        val (timeDP, dpr) = Measure.time(accessObjectDirectCopy(DPObj, objectSize / primSize))
        if (experiment >= JITOffset) {
          printv(s"Direct Pickled : $timeDP")
          printv(s"Direct Pickled Result   : $dpr")
          totalDP += timeDP
          resultDP += dpr
        }
      }
    }

    // Direct
    if (!skipD) {
      val DObj = clazz.newInstance.asInstanceOf[Instantiator].root
      System.gc()
      for (experiment <- 0 until repeats + JITOffset) {
        val (timeDirect, directr) = Measure.time(accessObjectDirect(DObj))
        if (experiment >= JITOffset) {
          printv(s"Direct : $timeDirect")
          printv(s"Direct Result   : $directr")
          totalDirect += timeDirect
          resultDirect += directr
        }
      }
    }

    totalDBB /= repeats
    totalBB /= repeats
    totalU /= repeats
    totalJNI /= repeats
    totalDirect /= repeats
    totalDP /= repeats

    print(f"$totalBB%10d, $totalDBB%8d, $totalU%8d, $totalJNI%8d, $totalDP%8d, $totalDirect%8d, $resultBB, $resultDBB, $resultU, $resultJNI, $resultDP, $resultDirect\n")
  }

  /**
    * Run the experiments on a collection of objects with multiple threads
    */
  def runParallelCollection(): Unit = {

    var totalDBB = 0L
    var totalBB = 0L
    var totalU = 0L
    var totalJNI = 0L
    var totalDirect = 0L
    var totalDP = 0L

    var resultDBB = 0L
    var resultBB = 0L
    var resultU = 0L
    var resultJNI = 0L
    var resultDirect = 0L
    var resultDP = 0L

    var DBBArrayOfObjects: ParArray[Any] = null
    var BBArrayOfObjects: ParArray[Any] = null
    var UArrayOfObjects: ParArray[Any] = null
    var JNIArrayOfObjects: Array[Any] = null
    var DirectArrayOfObjects: Array[Any] = null
    var DPArrayOfObjects: Array[Any] = null

    val totalBytes = objectSize * collectionSize
    if (totalBytes < 0)
      throw new Error("Object size times collection size exceeds maximum byte array size.")

    //println(s"total bytes: $totalBytes objects per thread: $objectsPerThread, leftOverObjects:$leftOverObjects")

    // Make Scala threads
    var objectsPerThread = collectionSize / bbthreads
    var leftOverObjects = collectionSize % bbthreads
    var parthreads = (0 until bbthreads).par
    parthreads.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(bbthreads))

    // ByteBuffer
    if (!skipBB) {
      BBArrayOfObjects = ParArray.fill(collectionSize)(clazz.newInstance.asInstanceOf[Instantiator].root)
      for (experiment <- 0 until repeats + JITOffset) {
        System.gc()
        var (timeBB, bbr) = Measure.time {
          // Create a ByteBuffer for each thread
          var bb = (0 until bbthreads).map { i => ByteBuffer.allocate((objectsPerThread + (if (i == bbthreads - 1) leftOverObjects else 0)) * objectSize) }
          parthreads.foreach { thread =>
            // Set endianness
            bb(thread).order(nativeOrder)
            ((thread * objectsPerThread) until ((thread + 1) * objectsPerThread)).foreach { index =>
              BBArrayOfObjects(index).asInstanceOf[ClassUnderTest].writeByteBufferManual(bb(thread).asIntBuffer())
              bb(thread).position(bb(thread).position + objectSize)
            }
            // Have the last thread mop up the final few objects as well.
            if (thread == bbthreads - 1) {
              (0 until leftOverObjects).foreach { index =>
                BBArrayOfObjects(bbthreads * objectsPerThread + index).asInstanceOf[ClassUnderTest].writeByteBufferManual(bb(thread).asIntBuffer())
                bb(thread).position(bb(thread).position + objectSize)
              }
            }
          }
          var bba = bb.map(b => b.array).toArray
          accessParallelCollectionBB(bba, bbthreads, objectSize / primSize)
        }
        if (experiment >= JITOffset) {
          printv(s"ByteBuffer : $timeBB")
          printv(s"ByteBuffer Result   : $bbr")
          totalBB += timeBB
          resultBB += bbr
        }
      }
      BBArrayOfObjects = null
    }

    // Direct ByteBuffer
    if (!skipDBB) {
      objectsPerThread = collectionSize / dbbthreads
      leftOverObjects = collectionSize % dbbthreads
      parthreads = (0 until dbbthreads).par
      parthreads.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(dbbthreads))

      DBBArrayOfObjects = ParArray.fill(collectionSize)(clazz.newInstance.asInstanceOf[Instantiator].root)
      for (experiment <- 0 until repeats + JITOffset) {
        System.gc()
        var (timeDBB, dbbr) = Measure.time {
          // Create a Direct ByteBuffer for each thread
          var dbb = (0 until dbbthreads).map { i => ByteBuffer.allocateDirect((objectsPerThread + (if (i == dbbthreads - 1) leftOverObjects else 0)) * objectSize) }
          parthreads.foreach { thread =>
            // Set endianness
            dbb(thread).order(nativeOrder)
            ((thread * objectsPerThread) until ((thread + 1) * objectsPerThread)).foreach { index =>
              DBBArrayOfObjects(index).asInstanceOf[ClassUnderTest].writeByteBufferManual(dbb(thread).asIntBuffer())
              dbb(thread).position(dbb(thread).position + objectSize)
            }
            // Have the last thread mop up the final few objects as well.
            if (thread == dbbthreads - 1) {
              (0 until leftOverObjects).foreach { index =>
                DBBArrayOfObjects(dbbthreads * objectsPerThread + index).asInstanceOf[ClassUnderTest].writeByteBufferManual(dbb(thread).asIntBuffer())
                dbb(thread).position(dbb(thread).position + objectSize)
              }
            }
          }
          var dbba = dbb.toArray
          accessParallelCollectionDBB(dbba, dbbthreads, objectSize / primSize)
        }
        if (experiment >= JITOffset) {
          printv(s"ByteBuffer : $timeDBB")
          printv(s"ByteBuffer Result   : $dbbr")
          totalDBB += timeDBB
          resultDBB += dbbr
        }
      }
      DBBArrayOfObjects = null
    }

    // Unsafe
    if (!skipU) {
      objectsPerThread = collectionSize / uthreads
      leftOverObjects = collectionSize % uthreads
      parthreads = (0 until uthreads).par
      parthreads.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(uthreads))

      UArrayOfObjects = ParArray.fill(collectionSize)(clazz.newInstance.asInstanceOf[Instantiator].root)
      for (experiment <- 0 until repeats + JITOffset) {
        System.gc()
        var (timeU, ur) = Measure.time {
          // Create an unsafe instance for each thread
          val um = UnsafeManager.getUnsafeInstance
          // Allocate memory in the first unsafe instance
          val ua = um.allocateMemory(totalBytes)

          parthreads.foreach { thread =>
            val ui = UnsafeManager.getUnsafeInstance
            ((thread * objectsPerThread) until ((thread + 1) * objectsPerThread)).foreach { index =>
              val offset = ua + index * objectSize
              //println(s"$thread at $offset placing $index")
              UArrayOfObjects(index).asInstanceOf[ClassUnderTest].writeUnsafeManual(offset, ui)
            }
            // Have the last thread mop up the final few objects as well.
            if (thread == uthreads - 1) {
              (0 until leftOverObjects).foreach { index =>
                val offset = ua + (uthreads * objectsPerThread + index) * objectSize
                UArrayOfObjects(uthreads * objectsPerThread + index).asInstanceOf[ClassUnderTest].writeUnsafeManual(offset, ui)
              }
            }
          }
          val r = accessParallelCollectionUnsafe(ua, totalBytes / primSize, threads)
          um.freeMemory(ua)
          r
        }
        if (experiment >= JITOffset) {
          printv(s"Unsafe : $timeU")
          printv(s"Unsafe Result   : $ur")
          totalU += timeU
          resultU += ur
        }
      }
      UArrayOfObjects = null
    }

    parthreads = null

    // JNI
    if (!skipJNI) {
      JNIArrayOfObjects = Array.fill(collectionSize)(clazz.newInstance.asInstanceOf[Instantiator].root)
      for (experiment <- 0 until repeats + JITOffset) {
        System.gc()
        var (timeJNI, jnir) = Measure.time {
          accessParallelCollectionJNI(JNIArrayOfObjects, jnithreads, objectSize / primSize)
        }
        if (experiment >= JITOffset) {
          printv(s"Unsafe : $timeJNI")
          printv(s"Unsafe Result   : $jnir")
          totalJNI += timeJNI
          resultJNI += jnir
        }
      }
      JNIArrayOfObjects = null
    }

    // Direct Pickled
    if (!skipDP) {
      DPArrayOfObjects = Array.fill(collectionSize)(clazz.newInstance.asInstanceOf[Instantiator].root)
      for (experiment <- 0 until repeats + JITOffset) {
        System.gc()
        var (timeDP, dpr) = Measure.time(accessParallelCollectionDirectCopy(DPArrayOfObjects, dpthreads, objectSize / primSize))
        if (experiment >= JITOffset) {
          printv(s"Direct Pickled : $timeDP")
          printv(s"Direct Pickled Result   : $dpr")
          totalDP += timeDP
          resultDP += dpr
        }
      }
      DPArrayOfObjects = null
    }

    // Direct
    if (!skipD) {
      DirectArrayOfObjects = Array.fill(collectionSize)(clazz.newInstance.asInstanceOf[Instantiator].root)
      for (experiment <- 0 until repeats + JITOffset) {
        System.gc()
        var (timeDirect, directr) = Measure.time(accessParallelCollectionDirect(DirectArrayOfObjects, directthreads))
        if (experiment >= JITOffset) {
          printv(s"Direct : $timeDirect")
          printv(s"Direct Result   : $directr")
          totalDirect += timeDirect
          resultDirect += directr
        }
      }
      DirectArrayOfObjects = null
    }

    totalBB /= repeats
    totalDBB /= repeats
    totalU /= repeats
    totalJNI /= repeats
    totalDirect /= repeats
    totalDP /= repeats

    print(f"$totalBB%10d, $totalDBB%8d, $totalU%8d, $totalJNI%8d, $totalDP%8d, $totalDirect%8d, $resultBB, $resultDBB, $resultU, $resultJNI, $resultDP, $resultDirect\n")
  }
}

