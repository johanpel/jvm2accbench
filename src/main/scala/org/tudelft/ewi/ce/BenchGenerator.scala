import java.io._

import scala.tools.nsc._
import scala.util.Random

/**
  * The BenchGenerator will generate and compile Scala and C code to a class file and a native library, respecctively.
  */
object BenchGenerator {

  // TODO: Make this all a bit less static
  val benchSourceDir: String = "bench"
  val outClassDir: String = benchSourceDir + "/out"
  val outLibDir: String = "lib"

  val helpText: String = "Usage: scala -cp jvm2accbench.jar\n" +
    "-h or --help       : Display this help text" +
    "--jarpath <path>   : path to this jar\n" +
    "--javadir <path>   : path to OpenJDK include directory\n" +
    "--ccompiler <name> : compiler, e.g. gcc\n" +
    "--leaf             : Leaf object layout\n" +
    "-p P               : P primitives (leaf & willow)\n" +
    "-a A               : A arrays (only for leaf)\n" +
    "-s S               : S array size (leaf & willow)\n" +
    "--willow           : Willow tree layout, each object at level\n" +
    "                     L has max(1,W-L) references to next level\n" +
    "                     objects until L=D, then the final object\n" +
    "                     contains 1 array of size S and P primitives\n" +
    "-w W               : Willow tree width\n" +
    "-d D               : Willow tree depth\n" +
    "--add              : Accumulate primitives (recommended)\n" +
    "--avx              : Use AVX intrinsics for copies (Intel AVX only)\n" +
    "--power8           : Use gcc compiler optimizations for POWER8 (-mcpu=power8 and -mtune=power8)" +
    "--critical         : Use JNI GetPrimitiveArrayCritical instead of Get<Primitive>ArrayElements" +
    "--prefetch         : Use __builtin_prefetch to load primitives for native approaches (not recommended)\n" +
    "--mergbb           : Merge ByteBuffers after serializing part of a collection (not recommended)\n" +
    "-v                 : verbose output\n"

  var javaIncludesDir: String = "/usr/lib/jvm/java-openjdk/include"
  var thisClassDir: String = "./bench/BenchGenerator.jar"
  var CCompiler: String = "gcc"
  var verbose = false

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
    CCodeGenerator.addPrims = args.contains("--add")
    CCodeGenerator.mergeBB = args.contains("--mergebb")
    CCodeGenerator.prefetch = args.contains("--prefetch")
    CCodeGenerator.avx = args.contains("--avx")
    CCodeGenerator.power8 = args.contains("--power8")
    CCodeGenerator.useCritical = args.contains("--critical")

    try {
      CCompiler = args(args.indexWhere(s => s == "--ccompiler") + 1)
      javaIncludesDir = args(args.indexWhere(s => s == "--javadir") + 1)
      thisClassDir = args(args.indexWhere(s => s == "--jarpath") + 1)
    }
    catch {
      case e: java.lang.ArrayIndexOutOfBoundsException => throw new Error("Please provide ccompiler, javadir and jarpath\n" + helpText)
    }

    // Check if mandatory arguments were set
    if (CCompiler.isEmpty || javaIncludesDir.isEmpty || thisClassDir.isEmpty) {
      throw new Error("Something went wrong parsing command line arguments.\n" + helpText)
    }

    // Leaf layout
    val (root, size) = if (args.contains("--leaf")) {
      val (primitives, arrays, arraySize) = try {
        (args(args.indexWhere(s => s == "-p") + 1).toInt,
          args(args.indexWhere(s => s == "-a") + 1).toInt,
          args(args.indexWhere(s => s == "-s") + 1).toInt)
      }
      catch {
        case e: java.lang.NumberFormatException => throw new Error("Leaf layout parameters weren't set correctly.\n" + helpText)
      }
      val rootClass = ClassGenerator(primitives, arrays, 0, 0)
      ObjectGenerator.createAndFillLeafObject(rootClass, arraySize)
    }
    // Willow layout
    else if (args.contains("--willow")) {
      val (width, depth, primitives, arraySize) = try {
        (args(args.indexWhere(s => s == "-w") + 1).toInt,
          args(args.indexWhere(s => s == "-d") + 1).toInt,
          args(args.indexWhere(s => s == "-p") + 1).toInt,
          args(args.indexWhere(s => s == "-s") + 1).toInt)
      }
      catch {
        case e: java.lang.NumberFormatException => throw new Error("Willow layout parameters weren't set correctly.\n" + helpText)
      }

      // Generate classes
      val classes = (0 to depth).indices.map(i =>
        if (i == depth)
        // Leaf class
          ClassGenerator(primitives, if (arraySize > 0) 1 else 0, 0, 0, i)
        else {
          // Non-leaf classes
          ClassGenerator(0, 0, if (width - i < 1) 1 else width - i, 0, i)
        }
      )
      classes.foreach(c => {
        // Set class reference types
        for (rt <- c.refTypes.indices) {
          c.refTypes(rt) = c.level + 2
        }
      })
      ObjectGenerator.createWillow(classes, primitives, arraySize)
    }
    // No layout specified
    else {
      throw new Error("Specify --leaf or --willow.\n" + helpText)
    }

    printv("Working directory:" + new File(".").getAbsolutePath)

    Random.setSeed(0)

    printv(s"Total object bytes: $size")
    printv(root)
    printv("Saving object size.")
    saveSourceFile(size.toString, "object.size")

    printv("Generating Scala class code.")
    val scalaCode = ScalaCodeGenerator.generateClassCode + ScalaCodeGenerator.generateInstantiation("", root)

    printv("Saving Scala class code.")
    saveSourceFile(scalaCode, "ClassUnderTest1.scala")

    printv("Compiling Scala class.")
    compileClassFile("ClassUnderTest1.scala")

    printv("Generating C code.")
    var CCode = CCodeGenerator.generateHeader("")
    CCode = CCodeGenerator.generateAccessObjectByteBuffer(CCode)
    CCode = CCodeGenerator.generateAccessParallelCollectionBB(CCode)
    CCode = CCodeGenerator.generateAccessObjectDirectByteBuffer(CCode)
    CCode = CCodeGenerator.generateAccessParallelCollectionDBB(CCode)
    CCode = CCodeGenerator.generateAccesObjectUnsafe(CCode)
    CCode = CCodeGenerator.generateAccessParallelCollectionUnsafe(CCode)
    CCode = CCodeGenerator.generateAccesObjectJNI(CCode, "", root.clazz)
    CCode = CCodeGenerator.generateAccesParallelCollectionJNI(CCode, "", root.clazz)
    CCode = CCodeGenerator.generateAccessObjectDirectCopy(CCode, "root", "initial_ref", root.clazz)
    CCode = CCodeGenerator.generateAccessParallelCollectionDirectCopy(CCode, "object", "array_ref", root.clazz)
    CCode = CCodeGenerator.generateAccesObjectDirect(CCode, "root", "initial_ref", root.clazz)
    CCode = CCodeGenerator.generateAccessParallelCollectionDirect(CCode, "object", "array_ref", root.clazz)

    saveSourceFile(CCode, "bench.c")

    printv("Compiling C code.")
    import sys.process._
    //TODO: use some sort of template like make or w/e to generate the compile and link commands
    val CCompile = CCompiler + " " +
      s"-std=c99 " +
      s"-I$javaIncludesDir " +
      s"-I$javaIncludesDir/linux " +
      s"-O3 " +
      (if (CCodeGenerator.avx) s"-mavx " else s"") +
      s"-c " +
      s"-fPIC " +
      s"-fopenmp " +
      (if (CCodeGenerator.power8) s"-mcpu=power8 -mtune=power8 " else s"") +
      s"$benchSourceDir/bench.c " +
      s"-o $outLibDir/bench.o"

    val CLink = CCompiler + " " +
      s"-shared " +
      s"-fopenmp " + // Apparently this is required in newer versions of gcc
      s"-lgomp " +
      s"$outLibDir/bench.o " +
      s"-o $outLibDir/libbench.so"

    // Run the compile step
    val CCompileExit = CCompile.!

    // Run the link step
    val CLinkExit = CLink.!

    printv("Done")

  }

  /**
    * Compile a class file using Scala NSC from the local bench source directory
    *
    * @param classFile The path to the class file to compile.
    */
  private def compileClassFile(classFile: String): Unit = {
    val settings = new Settings()
    settings.outputDirs.add(benchSourceDir, outClassDir)
    settings.classpath.append(thisClassDir)
    settings.debuginfo
    val compiler = new Global(settings)
    val files = List(benchSourceDir + "/" + classFile)
    val runner = new compiler.Run
    runner.compile(files)
  }

  /**
    * Save some sort of source to a file in the local bench source directory
    *
    * @param classCode The string to save
    * @param name      The name of the file
    */
  private def saveSourceFile(classCode: String, name: String) = {
    // Save to file
    val benchDir = new File(benchSourceDir)
    val bytecodeDir = new File(outClassDir)
    val libDir = new File(outLibDir)
    if (!benchDir.exists) benchDir.mkdir
    if (!bytecodeDir.exists) bytecodeDir.mkdir
    if (!libDir.exists) libDir.mkdir
    val cf = new PrintWriter(new File(benchSourceDir + "/" + name))
    cf.write(classCode)
    cf.close()
  }
}
