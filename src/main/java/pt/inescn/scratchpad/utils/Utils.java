package pt.inescn.scratchpad.utils;

import java.io.IOException;

import com.github.lwhite1.tablesaw.reducing.CrossTab;
import com.github.lwhite1.tablesaw.api.ColumnType;
import com.github.lwhite1.tablesaw.api.Table;
//import com.github.lwhite1.tablesaw.columns.Column;
import com.github.lwhite1.tablesaw.api.IntColumn;
import com.github.lwhite1.tablesaw.io.csv.CsvReader;

public final class Utils {

  private Utils() {
    // private constructor
  }

  public static Table xTabCountCols(Table dt, IntColumn c1, IntColumn c2){
    Table xTab = CrossTab.xTabCount(dt, c1, c2);
    return xTab;
  }
   
}
