case class MyCell(col: String, row: Int, value: String, makeVar: Boolean) {

  override def toString: String = col + row.toString + " = " + value;

  override def equals(obj: scala.Any): Boolean = {
    return this.toString == obj.toString;
  }

}
