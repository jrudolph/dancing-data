package net.virtualvoid.dancing

trait ClipboardReader[T] {
  def selection: List[T]
}

trait RowReader[T] {
  def readCells(it: Iterator[String]): T
}
trait CellReader[T] {
  def readCell(c: String): T
}


/**
 * Helper to extract OpenOffice spreadsheet data into the Scala REPL
 */
object OpenOffice {
  case class ~[A,B](a:A,b:B) {
    def innerToString: String = a match {
      case x@_ ~ _ => x.innerToString+", "+b 
      case _ => a+", "+b
   } 
    override def toString: String = "("+innerToString+")"
  }
  
  implicit def intReader:CellReader[Int] = new CellReader[Int] {
    val Column = """C;X\d+;Y\d+;K(\d+)""".r
    def readCell(c: String): Int = c match {
      case Column(i) => Integer.parseInt(i)
    }      
  }
  implicit def doubleReader:CellReader[Double] = new CellReader[Double] {
    val Column = """C;X\d+;Y\d+;K(\d+|\.\d+|\d+\.\d*)""".r
    def readCell(c: String): Double = c match {
      case Column(d) => java.lang.Double.parseDouble(d)
    }      
  }
  implicit def stringReader:CellReader[String] = new CellReader[String] {
    val Column = "C;X\\d+;Y\\d+;K\"([^\"]*)\"".r
    def readCell(c: String): String = c match {
      case Column(str) => str
    }      
  }
  
  implicit def oneColumn[T](implicit c:CellReader[T]) = new RowReader[T] {
    def readCells(it: Iterator[String]): T = c.readCell(it.next)
  }
  implicit def tuple2[T,U](implicit a:RowReader[T], b: RowReader[U]) = new RowReader[(T, U)]{
    def readCells(it: Iterator[String]): (T, U) = (a.readCells(it), b.readCells(it))
  }
  implicit def tuple3[T,U,V](implicit a:RowReader[T], b: RowReader[U], c:RowReader[V]) = new RowReader[(T, U, V)]{
    def readCells(it: Iterator[String]): (T, U, V) = (a.readCells(it), b.readCells(it), c.readCells(it))
  }
  implicit def tildeReader[T,U](implicit a: RowReader[T], b: RowReader[U]) = new RowReader[T ~ U] {
    def readCells(it: Iterator[String]) = new ~(a.readCells(it), b.readCells(it))
  }
  
  def lines(br: java.io.BufferedReader): Iterator[String] = new Iterator[String] {
    def hasNext = br.ready
    def next = br.readLine
  }
  
  def rows[T](implicit x:RowReader[T]):ClipboardReader[T] = new ClipboardReader[T]{
    def selection:List[T] = {
      import java.awt.Toolkit
      import java.awt.datatransfer._
      import java.io.{InputStreamReader,BufferedReader,InputStream}
      
      val clip = Toolkit.getDefaultToolkit.getSystemSelection
      val is = clip.getData(new DataFlavor("application/x-openoffice-sylk")).asInstanceOf[InputStream]
      val reader = new BufferedReader(new InputStreamReader(is))
      assert(reader.readLine == "ID;PSCALC3")
      
      val res = new scala.collection.mutable.ListBuffer[T]
      
      val it = lines(reader).takeWhile(_ != "E")
      while(it.hasNext)
        res += x.readCells(it)
        
      res.toList
    }
  }
  def implicitly[A](implicit a:A):A = a
  //val x:RowReader[Int] = implicitly
  
  def test {
    /*
    C;X1;Y1;K1
    C;X2;Y1;K"test"
    */
    
    //rows[(Int,String)]
    rows[Int ~ String ~ Int]
    //rows[(Int, String)](tuple2[Int,String,(Int,String)](oneColumn,oneColumn,tuple))
    //rows[(Int, String, Int)](tuple2tuple2[Int,String,(Int,String)](oneColumn,oneColumn,tuple))
  }
}
