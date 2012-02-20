package mstparser;

public class RelationalFeature implements java.io.Serializable {
  public String name;
  public String[][] values;

  public RelationalFeature(int size, String declaration, String[] lines) {
    values = new String[size][size];
    String[] declist = declaration.split(" ");

    name = declist[2];
    for (int i=0; i<size; i++) {
      values[i] = lines[i].substring(2).split(" ");
    }
  }

  public String getFeature(int firstIndex, int secondIndex) {
    if (firstIndex == 0 || secondIndex == 0)
      return name+"=NULL";
    else
      //System.out.println(values.length + "** " + name+"="+values[firstIndex-1][secondIndex-1]);
      return name+"="+values[firstIndex-1][secondIndex-1];
  }
}

