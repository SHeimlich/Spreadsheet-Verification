case class MyCell(col: String, row: Int, value: String, makeVar: Boolean) {

  override def toString: String = {
    if(value != null) {
      return (col + row.toString + " = " + value)
    }
    return (col + row.toString + " = null")

  };

  override def equals(obj: scala.Any): Boolean = {
    return this.toString == obj.toString;
  }

}
