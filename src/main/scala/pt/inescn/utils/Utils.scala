package pt.inescn.utils

object Utils {

  /**
   * Used for timing a single call.
   */
  def time[ R ]( block: => R ): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    //println( "Elapsed time: " + ( t1 - t0 ) + "ns" )
    println( "Elapsed time: " + ( t1 - t0 ) / 1e9 + "sec" )
    result
  }

  /**
   * Compare two real values with a given precision. The `eps` parameter
   * determines the precision with which the comparison is done. 
   */
  def approxEqual( a: Double, b: Double, eps: Double = 0.000001 ) = {
    if ( a.isNaN ) b.isNaN
    else if ( a.isInfinity ) b.isInfinity
    else if ( b.isNaN ) a.isNaN
    else if ( b.isInfinity ) a.isInfinity
    else ( ( b + eps ) >= a ) && ( a >= ( b - eps ) )
  }

  /**
   * Compare two real values with a given precision. The `eps` parameter
   * determines the precision with which the comparison is done.  If the check
   * fails the difference is written to standard output.
   * 
   * @see [[approxEqual]]
   */
  def aproxEqualShow( i : Int, a: Double, b: Double, eps: Double = 0.000001 ) = {
    val r = approxEqual( a, b, eps )
    if (! r) println(s"At $i expected $a but got $b")
    r
  }  
  
  def checkColumnValues[T,U,V](m : Map[String, Iterable[(T,U)]], chkShow: (Int,T,U,V) => Boolean, eps: V) = {
      m.forall{ p =>  
        println(s"Checking ${p._1}")
        val t = p._2.zipWithIndex
        t.forall{ case ((a,b),i) => chkShow(i, a, b, eps) }
      }
    }
    
    def checkFloatColumnValues(m : Map[String, Iterable[(Double, Double)]], eps: Double=0.000001) = {
      assert(checkColumnValues[Double, Double, Double](m, aproxEqualShow, eps))
    }

   def checkBooleanColumnValues(m : Map[String, Iterable[(Boolean, Boolean)]]) = {
      assert(checkColumnValues[Boolean, Boolean, Boolean](
          m, 
          { (i: Int, x:Boolean,y:Boolean,_) => val r = (x == y) ; if (!r) println(s"At $i expected $x but got $y") ; r }, 
          eps=true)
          )
    }
  
  
  import com.github.lwhite1.tablesaw.api.Table
  import com.github.lwhite1.tablesaw.api.ColumnType
  import com.github.lwhite1.tablesaw.columns.Column
  import com.github.lwhite1.tablesaw.api.IntColumn
  import com.github.lwhite1.tablesaw.api.BooleanColumn
  import com.github.lwhite1.tablesaw.api.FloatColumn
  import com.github.lwhite1.tablesaw.api.CategoryColumn

  /*
     * SawTable utility
     */
  def addColumn[ A ]( tbl: Table, col: A ) = {
    val ncol = col.asInstanceOf[ Column[ _ ] ]
    //val col1 = c1.asInstanceOf[Column[AnyRef]]
    //val col1 = c1.asInstanceOf[Column[Any]]
    //val col1 = c1.asInstanceOf[Column[IntColumn]]
    tbl.addColumn( ncol )
  }

  /*
     * SawTable utility
     */
  def addColumns[ A ]( tbl: Table, cols: A* ) = {
    val ncols = cols.map{ _.asInstanceOf[ Column[ _ ] ] }
    tbl.addColumn( ncols: _* )
    //ncols.foreach { x => tbl.addColumn(x) }
  }

  /*
     * SawTable utility
     */
  def createIntColumn( colName: String, vals: Seq[ Int ] ) = {
    val col = IntColumn.create( colName )
    vals.foreach { x => col.add( x ) }
    col
  }
}