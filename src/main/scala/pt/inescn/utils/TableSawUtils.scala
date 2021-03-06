package pt.inescn.utils

object TableSawUtils {

  import com.github.lwhite1.tablesaw.api.Table
  import com.github.lwhite1.tablesaw.api.ColumnType
  import com.github.lwhite1.tablesaw.columns.Column
  import com.github.lwhite1.tablesaw.api.IntColumn
  import com.github.lwhite1.tablesaw.api.BooleanColumn
  import com.github.lwhite1.tablesaw.api.FloatColumn
  import com.github.lwhite1.tablesaw.api.DoubleColumn
  import com.github.lwhite1.tablesaw.api.CategoryColumn
  import com.github.lwhite1.tablesaw.api.DateTimeColumn

  /**
   * SawTable utility that adds a column to a table.
   * This is required because the Java base class
   * Column has a polymorphic type that causes problems
   * with Scala's type inference.
   */
  def addColumn[ A ]( tbl: Table, col: A ) = {
    val ncol = col.asInstanceOf[ Column[ _ ] ]
    //val col1 = c1.asInstanceOf[Column[AnyRef]]
    //val col1 = c1.asInstanceOf[Column[Any]]
    //val col1 = c1.asInstanceOf[Column[IntColumn]]
    tbl.addColumn( ncol )
  }

  /**
   * SawTable utility hat adds a column to a table.
   * This is required because the Java base class
   * Column has a polymorphic type that causes problems
   * with Scala's type inference.
   *
   * @see [[pt.inescn.utils.addColumn[ A ]( tbl: Table, col: A )]]
   */
  def addColumns[ A ]( tbl: Table, cols: A* ) = {
    val ncols = cols.map{ _.asInstanceOf[ Column[ _ ] ] }
    tbl.addColumn( ncols: _* )
    //ncols.foreach { x => tbl.addColumn(x) }
  }

  /**
   * SawTable utility that allows one to create
   * a specific type of column.
   *
   * @see [[pt.inescn.utils.addColumn[ A ]( tbl: Table, col: A )]]
   */
  def createIntColumn( colName: String, vals: Seq[ Int ] ) = {
    val col = IntColumn.create( colName )
    vals.foreach { x => col.add( x ) }
    col
  }

  /**
   * SawTable utility that allows one to create
   * a specific type of column.
   *
   * @see [[pt.inescn.utils.addColumn[ A ]( tbl: Table, col: A )]]
   */
  def createFloatColumn( colName: String, vals: Seq[ Int ] ) = {
    val col = FloatColumn.create( colName )
    vals.foreach { x => col.add( x ) }
    col
  }

  /**
   * SawTable utility that allows one to create
   * a specific type of column.
   *
   * @see [[pt.inescn.utils.addColumn[ A ]( tbl: Table, col: A )]]
   */
  def createDoubleColumn( colName: String, vals: Seq[ Int ] ) = {
    val col = DoubleColumn.create( colName )
    vals.foreach { x => col.add( x ) }
    col
  }

  def createDateTimeColumn( colName: String, vals: Seq[ Int ] ) = {
    val col =  DateTimeColumn.create( colName )
    vals.foreach { x => col.add( x ) }
    col
  }
  
  
  /**
   * Returns `true` if the column `tp` is non-numeric otherwise returns a `false`
   */
  def isNonNumeric( tp: ColumnType ) = {
    val nonNumeric = List( ColumnType.BOOLEAN, ColumnType.CATEGORY, ColumnType.LOCAL_DATE, ColumnType.LOCAL_DATE_TIME, ColumnType.LOCAL_TIME, ColumnType.SKIP )
    nonNumeric.contains( tp )
  }

  import collection.JavaConverters._
  
  /**
   * Gets all column values of a given row. Assumes all data can be converted into a double. 
   * Currently only supports doubles, floats, integers and booleans. 
   */
  def getRow(t: Table, row : Int) = {
    val n = t.columnCount
    val c = 1 until t.columnCount
    c.map { x => 
      val cl = t.column(x)
      if (! isNonNumeric(cl.`type`)) cl.toDoubleArray()(row) 
      else if (cl.`type` == ColumnType.BOOLEAN) (t.booleanColumn(x).toIntArray()(row).toDouble)
      else Double.NaN
      }
  }
  

  /**
   * Applies a function to two numeric columns of a table.
   * If the columns are not numerical, an `NaN`is returned.
   * Otherwise the function is applied to the two columns
   * after these have been "converted" to `Double`s.
   *
   * @see [[isNonNumeric]]
   */
  def applyColumns( corr: ( Array[ Double ], Array[ Double ] ) => Double )( t: Table, colIdx1: Int, colType1: ColumnType, colIdx2: Int, colType2: ColumnType ) = {
    //println(s"Col($colIdx1,$colIdx2)")
    if ( isNonNumeric( colType1 ) ) Double.NaN
    else if ( isNonNumeric( colType2 ) ) Double.NaN
    else {
      //import pt.inescn.utils.Utils.time
      //time {
      val c1 = t.column( colIdx1 ).toDoubleArray()
      val c2 = t.column( colIdx2 ).toDoubleArray()
      corr( c1, c2 )
      //import smile.math.Math
      //Math.cor( c1, c2 )
      //}
    }
  }

  /**
   * Applies the function `cor` to all pairs of combinations of the
   * the columns in table `df`.  This function can used to calculate
   * the correlation between features in a data-set. For example
   * to calculate Pearson's correlation set `cor = Math.cor` where
   * `Math.cor` is SMILE's linear correlation function. Note that
   * the function is only applied to numeric columns. . Also note that
   * we only generate all n(n-1)/2 pairs (order does not matter) and
   * do not use the pair (x,x).
   */
  def combColumns( cor: ( Array[ Double ], Array[ Double ] ) => Double )( df: Table ) = {
    val ctypes = df.columnTypes()
    //println( ctypes.mkString( "," ) )
    // Index the column types from 0...n 
    val l = ctypes.zipWithIndex.toList

    // get the set of tails of the columns
    // for example columns(0,1,2,3) -> ((0,1,2,3),(1,2,3), (2,3), (3))
    val tails = l.tails
    val starts = tails.toStream
    // Now pair each head with its tail
    // for example <0,(0,1,2,3)>, <1,(1,2,3)>, <2,(2,3)>,...
    val both = l.zip( starts )
    var r = both.par.flatMap{
      case ( a, b ) =>
        val colIdx1 = a._2
        val ctp1 = a._1
        // Note that we need not pair the same column with itself
        val colIdxs = b.drop( 1 )
        // Now map each head with all elements in each tail        
        val t = colIdxs.par.map{ x =>
          ( colIdx1, x._2, applyColumns( cor )( df, colIdx1, ctp1, x._2, x._1 ) )
        }
        t
    }
    r
  }

  /**
   * Applies the function `cor` to all pairs of numeric columns in
   * the table `t`. Only the values equal to or above the cut-off
   * are returned. This can be use for example to calculate the
   * Pearson's correlation that are above a give value. in this case
   * we can use SMILE's `cor = Math.cor`
   *
   * @see [[combColumns]]
   */
  def findCorrelation( cor: ( Array[ Double ], Array[ Double ] ) => Double, cutoff: ( Double ) => Boolean )( t: Table ) = {
    val corrs = combColumns( cor )( t )
    //println( corrs.mkString( "<", ",", ">" ) )
    corrs.filter( x => cutoff( x._3 ) )
  }

  import pt.inescn.scratchpad.utils.UF

  def findCorrelationComponents( chk_dep: ( ( Int, Int, Double ) ) => Unit )( pairs: Seq[ ( Int, Int, Double ) ] ) = {
    // Determine what sets of correlated variables exist
    val uf = new UF( pairs.size )
    pairs.foreach( f => uf.union( f._1, f._2 ) )
    // Create a map from the set's root to its elements
    // Record all roots and theire members
    val components = pairs.foldLeft( Map[ Int, Set[ Int ] ]() ){
      case ( acc, v ) =>
        val cp1 = uf.find( v._1 )
        val cp2 = uf.find( v._2 )
        chk_dep( v )
        assert( cp1 == cp2 )
        // get root's set
        val s = acc.getOrElse( cp1, Set[ Int ]() )
        // add elements to root's set
        val ns = s + ( v._1, v._2 )
        // update root's set
        acc + ( cp1 -> ns )
    }
    // Calculate the total number of correlated variables (irrespective of the set/root)
    val totlComponents = components.foldLeft( 0 ){
      case ( acc, v ) => acc + v._2.size
    }
    ( uf, components, totlComponents )
  }

  /*
   * Statistical and other ML related calculations
   */

  import pt.inescn.utils.Utils._
  
  /**
   * Calculates a correlate between all numeric pairs of columns. It then selects
   * those columns according to a given cutoff. 
   */
  def report_correlation( cor: ( Array[ Double ], Array[ Double ] ) => Double, cutoff: ( Double ) => Boolean )( chk_dep: ( ( Int, Int, Double ) ) => Unit )( dts: Table ) = {
    val t = time { findCorrelation( cor, cutoff )( dts ) }
    val dtscc = dts.columnCount()
    val dts_pairs = dtscc * ( dtscc - 1 ) / 2
    println( s"Found ${t.size} pairs o significant correlations from a total of $dts_pairs" )

    import pt.inescn.scratchpad.utils.UF

    val ( uf, compont, totlCompont ) = time{ findCorrelationComponents( chk_dep )( t.toList ) }
    println( s"Found ${uf.count} components of significantly correlated features from a total of ${t.size} pairs of correlated features" )
    println( s"Found the following ${compont.size} components of significantly correlated features" )
    println( compont.mkString( "{", ",", "}" ) )
    println( s"Found a total of ${totlCompont} correlated features" )
  }

  /**
   * Container class that holds the checks for near zero variance features.
   * Assumes the features are numeric.
   */
  case class NearZeroCheck(
    frequency_ratio: Double, bad_freq_ratio: Boolean,
    unique_val_ratio: Double, bad_unique_val_ratio: Boolean,
    isConstant: Boolean )

  /*
     *  Calculates the ratios::
      * - The frequency of the most prevalent value over the second most frequent value (called the 
      *   “frequency ratio’’), which would be near one for well-behaved predictors and very large for 
      *   highly-unbalanced data
      *  
      * - The “percent of unique values’’ is the number of unique values divided by the total number of 
      *   samples (times 100) that approaches zero as the granularity of the data increases
      *   
      *  Checks the following conditions (by default):
      *  A predictor is classified as near-zero variance if 
      *  - The percentage of unique values  in the samples is less than {10\%} and when 
      *  - The frequency ratio mentioned above is greater than 19 (95/5). 
      *  -  If a column is constant (1 unique value)
      */
  def calcRatios[ V ]( l: Iterable[ V ], uniqueCut: Double, freqCut: Double ) = {
    val r = l.groupBy( identity ).mapValues( _.size )
    val s = r.toList.sortBy { x => -x._2 }
    val ratio = if ( s.size >= 2 ) {
      val top = s( 0 )._2
      val top_1 = s( 1 )._2
      top.toDouble / top_1.toDouble
    } else {
      Double.NaN
    }
    val bad_ratio = if ( ratio > freqCut ) true else false
    val unique_ratio = r.size / l.size.toDouble
    val bad_unique_ratio = if ( unique_ratio < uniqueCut ) true else false
    NearZeroCheck( ratio, bad_ratio, unique_ratio, bad_unique_ratio, r.size == 1 )
  }

  /*
      * The check for near zero variance verifies the following conditions:
      * 1) few unique values relative to the number of samples 
      * 2) large ratio of the frequency of the most common value to the frequency of the second most 
      *     common value (near-zero variance predictors).
      * 
      * More specifically:
      * - The frequency of the most prevalent value over the second most frequent value (called the 
      *   “frequency ratio’’), which would be near one for well-behaved predictors and very large for 
      *   highly-unbalanced data
      *  
      * - The “percent of unique values’’ is the number of unique values divided by the total number of 
      *   samples (times 100) that approaches zero as the granularity of the data increases
      * 
      * By default, a predictor is classified as near-zero variance if the percentage of unique values 
      * in the samples is less than {10\%} and when the frequency ratio mentioned above is greater 
      * than 19 (95/5). 
      */
  def nearZero[ K, V ]( t: Table, colName: String, uniqueCut: Double = 0.1, freqCut: Double = 19 ) = {
    val col = t.column( colName )
    val meta = col.columnMetadata()
    val tp = meta.getType
    tp match {
      case ColumnType.BOOLEAN =>
        val c = t.booleanColumn( colName )
        val l = c.toIntArray
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.CATEGORY =>
        val c = t.categoryColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.FLOAT =>
        val c = t.floatColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.DOUBLE =>
        val c = t.floatColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.SHORT_INT =>
        val c = t.shortColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.INTEGER =>
        val c = t.intColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.LONG_INT =>
        val c = t.longColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.LOCAL_DATE =>
        val c = t.dateColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.LOCAL_DATE_TIME =>
        val c = t.dateTimeColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.LOCAL_TIME =>
        val c = t.timeColumn( colName )
        val l = c.asScala
        calcRatios( l, uniqueCut, freqCut )
      case ColumnType.SKIP =>
        NearZeroCheck( Double.NaN, true, Double.NaN, true, true )
    }
  }

  def nearZeros( t: Table, uniqueCut: Double = 0.1, freqCut: Double = 19 ) = {
    val cols = t.columnNames.asScala
    val nzc = cols.map { colName => nearZero( t, colName, uniqueCut, freqCut ) }
    val dt = Table.create( s"Near Zero check for ${t.name}" )
    val col0 = CategoryColumn.create( "Column" )
    val col1 = FloatColumn.create( "freqRatio" )
    val col2 = BooleanColumn.create( "badFreqRatio" )
    val col3 = FloatColumn.create( "uniqueValRatio" )
    val col4 = BooleanColumn.create( "badUniqueValRatio" )
    val col5 = BooleanColumn.create( "isConstant" )
    val nt = cols.foreach {
      colName =>
        val nzc = nearZero( t, colName, uniqueCut, freqCut )
        col0.add( colName )
        col1.add( nzc.frequency_ratio )
        col2.add( nzc.bad_freq_ratio )
        col3.add( nzc.unique_val_ratio )
        col4.add( nzc.bad_unique_val_ratio )
        col5.add( nzc.isConstant )
    }
    addColumn( dt, col0 ) // why cannot we not add tis too via the varargs?
    addColumns( dt, col1, col2, col3, col4, col5 )
    dt
  }
  
  /*
   * Check for linear combinations
   * 
   * see: https://topepo.github.io/caret/pre-processing.html#lindep
   * See: https://github.com/haifengl/smile/issues/86
   * See: https://rdrr.io/cran/caret/man/findLinearCombos.html
   * See: https://see.stanford.edu/materials/lsoeldsee263/04-qr.pdf
   * http://stackoverflow.com/questions/12304963/using-eigenvalues-to-test-for-singularity-identifying-collinear-columns
   * http://stats.stackexchange.com/questions/16327/testing-for-linear-dependence-among-the-columns-of-a-matrix
   * http://numpy-discussion.10968.n7.nabble.com/Identifying-Colinear-Columns-of-a-Matrix-td5669.html
   *  identify collinear columns in matrix qr decomposition 
   * http://math.stackexchange.com/questions/759208/multicollinearity-and-svd
   * https://github.com/topepo/caret/blob/36f8216a1f18e17021adfd17ebdffa8e9f058949/pkg/caret/R/findLinearCombos.R
   * https://en.wikipedia.org/wiki/QR_decomposition
   * http://stackoverflow.com/questions/35410769/remove-perfectly-multicollinear-variables-from-data-frame
   * http://stackoverflow.com/questions/13312498/how-to-find-degenerate-rows-columns-in-a-covariance-matrix
   * 
   *  Algorithm AS 268: All Possible Subset Regressions Using the QR Decomposition
      D. M. Smith
      Journal of the Royal Statistical Society. Series C (Applied Statistics)
      Vol. 40, No. 3 (1991), pp. 502-513 
   *
functionBody(findLinearCombos)
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    lcList <- enumLC(x)
    initialList <- lcList
    badList <- NULL
    if (length(lcList) > 0) {
        continue <- TRUE
        while (continue) {
            tmp <- unlist(lapply(lcList, function(x) x[1]))
            tmp <- unique(tmp[!is.na(tmp)])
            badList <- unique(c(tmp, badList))
            lcList <- enumLC(x[, -badList])
            continue <- (length(lcList) > 0)
        }
    }
    else badList <- NULL
    list(linearCombos = initialList, remove = badList)
}
   * 
   *  
# this function does the actual work for all of the enumLC methods
internalEnumLC <- function(qrObj, ...)
{
   R <- qr.R(qrObj)                     # extract R matrix
   numColumns <- dim(R)[2]              # number of columns in R
   rank <- qrObj$rank                   # number of independent columns
   pivot <- qrObj$pivot                 # get the pivot vector

   if (is.null(numColumns) || rank == numColumns)
   {
      list()                            # there are no linear combinations
   } else {
      p1 <- 1:rank
      X <- R[p1, p1]                    # extract the independent columns
      Y <- R[p1, -p1, drop = FALSE]     # extract the dependent columns
      b <- qr(X)                        # factor the independent columns
      b <- qr.coef(b, Y)                # get regression coefficients of
                                        # the dependent columns
      b[abs(b) < 1e-6] <- 0             # zap small values

      # generate a list with one element for each dependent column
      lapply(1:dim(Y)[2],
         function(i) c(pivot[rank + i], pivot[which(b[,i] != 0)]))
   }
}   * 
   */
   // TODO  

}