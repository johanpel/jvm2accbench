# JVM-to-Accelerator Benchmark tool (jvm2accbench)
Based on a specifiable object graph layout, this tool generates code to serialize or access JVM objects in a native environment using four (plus two variants) approaches:
* ByteBuffer: using ordinary `java.nio.ByteBuffer`
* ByteBuffer (off-heap): using `allocateDirect`
* Unsafe: using the `sun.misc.Unsafe` library
* JNI: using the Java Native Interface
* Direct (copy): directly accessing JVM memory and serializing it to a contiguous memory region allocated with `malloc`
* Direct (no-copy): directly accessing the JVM memory without copying it to a contiguous memory region

We wrote this tool to get insight and numbers on the HotSpot VMs ability to serialize/access data to/in a native environment that will ultimately be attached to some accelerator interface.
The tool can then be used to run a benchmark serializing/accessing a single object with a single thread or a collection of object with multiple threads.

# Requirements:
* gcc 4.8+
* OpenJDK 8
* Scala 2.12.1
* Maven 3

Under these conditions the framework was tested, but it will probably run without too many modifications on different JVMs or with different C compilers.

# Usage:
* Clone this repo:
```
git clone https://github.com/johanpel/jvm2accbench.git
```
* cd to the directory of this project
```
cd jvm2accbench
```
* Run Maven to create the jar
```
mvn package
```
* Read the help documentation of the benchmark code generation stage, assuming the jar generated is named `jvm2accbench-1.0.jar`
```
scala -cp target/jvm2accbench-1.0.jar "BenchGenerator" --help
```
* Example, generate code using gcc for a leaf object with 4 primitives, 2 arrays with 32 primitives, enabling accumulation of all primitives. Make sure to change the javadir:
```
scala -cp target/jvm2accbench-1.0.jar "BenchGenerator" \
--javadir /usr/lib/jvm/java-openjdk/include \
--jarpath ./target/jvm2accbench-1.0.jar \
--ccompiler gcc \
--leaf -p 4 -a 2 -s 32 \
--add
```
* The folder `./bench` should now be created, holding the class files and C sources. Also the folder `./lib` should be created, holding the compiled native library
* Read the help documentation of the benchmark runner
```
scala -cp target/jvm2accbench-1.0.jar "BenchRunner" --help
```
* Example of running a benchmark. Compressed OOPs must be disabled such that internal references of the JVM are actual virtual memory addresses to the objects
```
scala -J-XX:-UseCompressedOops -cp target/jvm2accbench-1.0.jar:bench/out "BenchRunner" -m "single" -r 32
```
* Output format is a comma seperated line with some numbers that mean the following:
```Threads, 
object size (bytes), 
experiment repeats, 
number of objects in collection,
collection size (bytes),
average time for ByteBuffer,
average time for ByteBuffer (off-heap),
average time for Unsafe,
average time for JNI,
average time for Direct (copy),
average time for Direct (no-copy),
accumulated primitives result for ByteBuffer, 
accumulated primitives result for ByteBuffer (off-heap), 
accumulated primitives result for Unsafe, 
accumulated primitives result for JNI, 
accumulated primitives result for Direct (copy),
accumulated primitives result for Direct (no-copy)
```
By average time is meant: the average time to access the whole collection over R measurements, where R is the number of repeats specified with -r. The previous example only shows access of a single object with a single thread. Refer to the `--help` of the `BenchRunner` to see how to run for a collection of objects.
