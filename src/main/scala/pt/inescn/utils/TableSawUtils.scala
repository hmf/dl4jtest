package pt.inescn.utils

object TableSawUtils {
  
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