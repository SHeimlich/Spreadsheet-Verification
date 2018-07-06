case class MyCell(col: Char, row: Int, value: String, makeVar: Boolean) {

  override def toString: String = col + row.toString + " = " + value;

  override def equals(obj: scala.Any): Boolean = {
    //println("in equals! " + this.toString + " " + obj.toString)
    return this.toString == obj.toString;
  }

}
