import sbt._
import xsbt.FileUtilities.write

trait BoilerplateBase {
  self: DefaultProject =>  
  
  def srcManagedScala = "src_managed" / "main" / "scala"
  def writeFilePackage(fileName: String, source: String): Unit = {
    val file = (srcManagedScala / "akihiro" / fileName).asFile
    write(file, source)
  }
}

trait BoilerplateForSorter extends BoilerplateBase {
  self: DefaultProject =>

  lazy val generateSort = {
    val cleanSrcManaged = cleanTask(srcManagedScala) named ("clean src_managed")
    task {
      def AnyValMap(typename: String): (String, Map[Symbol, String]) =
        typename -> Map(
            'type -> typename,
            'decl -> "",
            'le -> "($1 <= $2)",
            'gt -> "($1 > $2)",
            'cast -> "",
            'cext -> "",
            'carg -> "",
            'wcmp -> "",
            'narg -> "")
           
      val types: Map[String, Map[Symbol, String]] = Map(
          AnyValMap("Byte"),
          AnyValMap("Char"),
          AnyValMap("Double"),
          AnyValMap("Float"),
          AnyValMap("Int"),
          AnyValMap("Long"),
          AnyValMap("Short"),
          "Comparable" -> Map(
              'type -> "A",
              'decl -> "[A <: AnyRef with java.lang.Comparable[A]]",
              'le   -> "(($1).compareTo($2) <= 0)",
              'gt   -> "(($1).compareTo($2) > 0)",
              'cast -> ".asInstanceOf[Array[java.lang.Object]]",
              'cext -> "",
              'carg -> "",
              'wcmp -> "",
              'narg -> ""),         
          "Object" -> Map(
              'type -> "A",
              'decl -> "[A <: AnyRef]",
              'le -> "c.compare($1, $2) <= 0",
              'gt -> "c.compare($1, $2) > 0",
              'cast -> "",
              'cext -> ", c",
              'carg -> "c: java.util.Comparator[_ >: \\$TYPE\\$]",
              'wcmp -> "WithCompartor",
              'jsrt -> "[A]",
              'narg -> "[A](c)"))

     def replaceWithMap(str: String) = for { (_, map) <- types.toList.sort(_._1 < _._1) } yield
        str.replaceAll("\\$CARG\\$", map('carg))
           .replaceAll("\\$CARGSEP\\$", (if (map('carg).isEmpty) "" else ", "))
           .replaceAll("\\$WCMP\\$", map('wcmp))
           .replaceAll("\\$NARG\\$", map('narg))
           .replaceAll("\\$TYPE\\$", map('type))
           .replaceAll("\\$DECL\\$", map('decl))
           .replaceAll("\\$CAST\\$", map('cast))
           .replaceAll("\\$CEXT\\$", map('cext))
           .replaceAll("\\$JSRT\\$", map.getOrElse('jsrt, ""))
           .replaceAll("""\$LE\$\s*\(([^,]+),\s*([^()]+)\)""", map('le))
           .replaceAll("""\$GT\$\s*\(([^,]+),\s*([^()]+)\)""", map('gt))       
                          
      val sortInterfaces = replaceWithMap(Plate.sortTemplate)          
      val sorterClasses = replaceWithMap(Plate.sorterTemplate)
      
      val source = """
package akihiro

private class PriorityStack(nelems: Int, nthres: Int, nthreads: Int) {
  private val ls = new Array[Int]((nelems / nthres + 1) & (~1)) // worst case
  private val hs = new Array[Int](nthreads * 2 + 1)             // worst case
  private var lend, hend = 0
  
  final def add(a: Int, b: Int) {
    if (b - a <= nthres) {
      hs(hend) = b; hend += 1
      hs(hend) = a; hend += 1
    } else {
      ls(lend) = b; lend += 1
      ls(lend) = a; lend += 1
    }
  }
  
  final def pop() = {
    if (hend > 0) {
      hend -= 1
      hs(hend)
    } else {
      lend -= 1
      ls(lend)
    }
  }
}

object ParArrays {
  import java.util.concurrent.Semaphore
  import java.util.concurrent.atomic.AtomicInteger
""" + sortInterfaces.mkString("\n") + sorterClasses.mkString("\n") + """
}
"""
      writeFilePackage("ParSort.scala", source)
      None
    } dependsOn(cleanSrcManaged)
  }
}

object Plate {
  val sorterTemplate = """
  private class SorterOf$TYPE$$WCMP$$DECL$($CARG$) extends Runnable {
    val NTHRES = 10000
    var queue: PriorityStack = null
    var array: Array[$TYPE$] = null
    var semaphore: Semaphore = null 
    var isDone = false
    var nRemains: AtomicInteger = null
    var nThreads: Int = _ 
    
    def sort(a: Array[$TYPE$]) {
      sort(a, 0, a.length)
    }
    
    def sort(a: Array[$TYPE$], beg: Int, end: Int) {
      if (beg == end) return
      this.nThreads = Runtime.getRuntime.availableProcessors
      val threads = Array.fill(nThreads)(new Thread(this))
      this.queue = new PriorityStack(end - beg, NTHRES, nThreads)
      this.queue.add(beg, end - 1)
      this.array = a
      this.isDone = false
      this.semaphore = new Semaphore(1)
      this.nRemains = new AtomicInteger(end - beg)
      
      threads foreach {_.start}
      threads foreach {_.join}
    }
    
    def run {
      def partition(p: Int, r: Int) = {
        val i = p + scala.util.Random.nextInt(r - p)
        val x = array(i); array(i) = array(p); array(p) = x
        var k = p
        var l = r + 1
        while ({k += 1; $LE$(array(k), x) && k < r}) {};
        while ({l -= 1; $GT$(array(l), x)}) {};
        while (k < l) {
          val t = array(k); array(k) = array(l); array(l) = t
          while ({k += 1; $LE$(array(k), x)}) {}
          while ({l -= 1; $GT$(array(l), x)}) {}
        }
        val t = array(p); array(p) = array(l); array(l) = t
        l
      }
  
      while (true) {
        semaphore.acquire
        if (isDone) return
        
        var p, r: Int = 0
        synchronized {
          p = queue.pop
          r = queue.pop
        }
        
        if (r - p <= NTHRES) {
          java.util.Arrays.sort$JSRT$(array$CAST$, p, r + 1$CEXT$)
          if (nRemains.addAndGet(p - r - 1) == 0) {
            isDone = true
            semaphore.release(nThreads)
          }
        } else {
          val q = partition(p, r)
          var nsem = 0
          synchronized {
            if (p != q) {queue.add(p, q - 1); nsem += 1}
            if (q != r) {queue.add(q + 1, r); nsem += 1}
          }
          nRemains.decrementAndGet
          semaphore.release(nsem)
        }
      }
    }
  }
"""
  
  val sortTemplate = """
  def sort$DECL$(a: Array[$TYPE$]$CARGSEP$$CARG$) {
    (new SorterOf$TYPE$$WCMP$$NARG$).sort(a)
  }

  def sort$DECL$(a: Array[$TYPE$], beginIndex: Int, endIndex: Int$CARGSEP$$CARG$) {
    (new SorterOf$TYPE$$WCMP$$NARG$).sort(a, beginIndex, endIndex)
  }
"""
}
